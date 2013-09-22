(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

open General
open Support.Common

module FeedAttr = Constants.FeedAttr
module U = Support.Utils
let (>|=) = Lwt.(>|=)

(** Manages the process of downloading feeds during a solve.
    We use the solver to get the current best solution and the set of feeds it queried.
    We download any missing feeds and update any out-of-date ones, resolving each time
    we have more information. *)

let get_values map = StringMap.fold (fun _key value xs -> value :: xs) map []

type watcher = <
  report : feed_url -> string -> unit;
  update : bool * Solver.result -> unit;
>

(* Turn a list of tasks into a list of their resolutions. *)
let rec collect_ex = function
  | [] -> Lwt.return []
  | x :: xs ->
      lwt result = x in
      lwt results = collect_ex xs in
      result :: results |> Lwt.return

class driver config (fetcher:Fetch.fetcher) distro (slave:Python.slave) =
  object (self)
    (** Run the solver, then download any feeds that are missing or that need to be
        updated. Each time a new feed is imported into the cache, the solver is run
        again, possibly adding new downloads.

        Note: if we find we need to download anything, we will refresh everything.

        @param force re-download all feeds, even if we're ready to run (implies update_local)
        @param watcher notify of each partial solve (used by the GUI to show the current state)
        @param update_local fetch PackageKit feeds even if we're ready to run *)
    method solve_with_downloads ?feed_provider ?(watcher:watcher option) requirements
                                ~force ~update_local : (bool * Solver.result * Feed_cache.feed_provider) =
      let force = ref force in
      let seen = ref StringSet.empty in
      let downloads_in_progress = ref StringMap.empty in

      let already_seen url = StringSet.mem url !seen in
      let forget_feed url = seen := StringSet.remove url !seen in

      let report_problem feed_url msg =
        match watcher with
        | Some watcher -> watcher#report feed_url msg
        | None -> log_warning "Feed %s: %s" feed_url msg in

      (* There are three cases:
         1. We want to run immediately if possible. If not, download all the information we can.
            (force = False, update_local = False)
         2. We're in no hurry, but don't want to use the network unnecessarily.
            We should still update local information (from PackageKit).
            (force = False, update_local = True)
         3. The user explicitly asked us to refresh everything.
            (force = True) *)

      let feed_provider =
        match feed_provider with
        | Some feed_provider -> feed_provider
        | None -> new Feed_cache.feed_provider config distro in

      (* Add [url] to [downloads_in_progress]. When [download] resolves (to a function),
         call it in the main thread. *)
      let add_download url download =
        seen := StringSet.add url !seen;
        let wrapped =
          try_lwt
            lwt fn = download in
            Lwt.return (url, fn)
          with Safe_exception (msg, _) ->
            report_problem url msg;
            Lwt.return (url, fun () -> ()) in
        downloads_in_progress := StringMap.add url wrapped !downloads_in_progress
        in

      (** Register a new download. When it resolves, process it in the main thread. *)
      let rec handle_download f dl =
        add_download f (dl >|= fun result () ->
          (* (we are now running in the main thread) *)
          match result with
          | `problem (msg, next_update) -> (
              report_problem f msg;
              match next_update with
              | None -> ()
              | Some next -> handle_download f next
          )
          | `aborted_by_user -> ()    (* No need to report this *)
          | `no_update -> ()
          | `update (new_xml, next_update) ->
              feed_provider#replace_feed f (Feed.parse config.system new_xml None);
              (* On success, we also need to refetch any "distribution" feed that depends on this one *)
              let distro_url = "distribution:" ^ f in
              feed_provider#forget_distro distro_url;
              forget_feed distro_url;
              (* (we will now refresh, which will trigger distro#check_for_candidates *)
              match next_update with
              | None -> ()    (* This is the final update *)
              | Some next ->
                  log_info "Accepted update from mirror, but will continue waiting for primary for '%s'" f;
                  handle_download f next
        ) in

      let rec loop ~try_quick_exit =
        (* Called once at the start, and once for every feed that downloads (or fails to download). *)
        let result = Solver.solve_for config feed_provider requirements in

        let () =
          match watcher with
          | Some watcher -> watcher#update result
          | None -> () in

        match result with
        | (true, _) when try_quick_exit ->
            assert (StringMap.is_empty !downloads_in_progress);
            result
        | (ready, _) ->
            if not ready then force := true;

            (* For each remote feed used which we haven't seen yet, start downloading it. *)
            if !force && config.network_use <> Offline then (
              ListLabels.iter feed_provider#get_feeds_used ~f:(fun f ->
                if not (already_seen f) then (
                  match Feed_cache.parse_feed_url f with
                  | `local_feed _ -> ()
                  | `distribution_feed x -> failwith x
                  | `remote_feed _ as feed ->
                      log_info "Starting download of feed '%s'" f;
                      fetcher#download_and_import_feed feed |> handle_download f
                )
              )
            );

            (* Check for extra (uninstalled) local distro candidates. *)
            if !force || update_local then (
              ListLabels.iter feed_provider#get_feeds_used ~f:(fun f ->
                match feed_provider#get_feed f with
                | None -> ()
                | Some (master_feed, _) ->
                    let f = "distribution:" ^ f in
                    if not (already_seen f) then (
                        add_download f (distro#check_for_candidates master_feed >|= fun () () ->
                          feed_provider#forget_distro f
                        )
                    )
              )
            );

            match get_values !downloads_in_progress with
            | [] -> 
                if config.network_use = Offline && not ready then
                  log_info "Can't choose versions and in off-line mode, so aborting";
                result;
            | downloads ->
                let (url, fn) = Lwt_main.run @@ Lwt.choose downloads in
                downloads_in_progress := StringMap.remove url !downloads_in_progress;
                fn ();    (* Clears the old feed(s) from Feed_cache *)
                (* Run the solve again with the new information. *)
                loop ~try_quick_exit:false
      in
      let (ready, result) = loop ~try_quick_exit:(not (!force || update_local)) in
      (ready, result, feed_provider)

    (** Find the best selections for these requirements and return them if available without downloading. 
     * Returns None if we need to refresh feeds or download any implementations. *)
    method quick_solve reqs =
      let feed_provider = new Feed_cache.feed_provider config distro in
      match Solver.solve_for config feed_provider reqs with
      | (true, results) ->
          let sels = results#get_selections in
          if Selections.get_unavailable_selections config ~distro sels = [] then
            Some sels   (* A set of valid selections, available locally *)
          else
            None        (* Need to download to get the new selections *)
      | (false, _) ->
          None          (* Need to refresh before we can solve *)

    (** Convenience wrapper for Fetch.download_and_import_feed that just gives the final result.
     * If the mirror replies first, but the primary succeeds, we return the primary.
     *)
    method download_and_import_feed url =
      let `remote_feed feed_url = url in
      let update = ref None in
      let rec wait_for (result:Fetch.fetch_feed_response Lwt.t) =
        match_lwt result with
        | `update (feed, None) -> `success feed |> Lwt.return
        | `update (feed, Some next) ->
            update := Some feed;
            wait_for next
        | `aborted_by_user -> Lwt.return `aborted_by_user
        | `no_update -> (
            match !update with
            | None -> Lwt.return `no_update
            | Some update -> Lwt.return (`success update)  (* Use the previous partial update *)
        )
        | `problem (msg, None) -> (
            match !update with
            | None -> raise_safe "%s" msg
            | Some update ->
                log_warning "Feed %s: %s" feed_url msg;
                Lwt.return (`success update)  (* Use the previous partial update *)
        )
        | `problem (msg, Some next) ->
            log_warning "Feed '%s': %s" feed_url msg;
            wait_for next in

      wait_for @@ fetcher#download_and_import_feed url

    (** Ensure all selections are cached, downloading any that are missing.
        If [include_packages] is given then distribution packages are also installed, otherwise
        they are ignored. *)
    method download_selections ~include_packages ~(feed_provider:Feed_cache.feed_provider) sels : [ `success | `aborted_by_user ] Lwt.t =
      let missing =
        let maybe_distro = if include_packages then Some distro else None in
        Selections.get_unavailable_selections config ?distro:maybe_distro sels in
      if missing = [] then (
        Lwt.return `success
      ) else if config.network_use = Offline then (
        let format_sel sel =
          ZI.get_attribute FeedAttr.interface sel ^ " " ^ ZI.get_attribute FeedAttr.version sel in
        let items = missing |> List.map format_sel |> String.concat ", " in
        raise_safe "Can't download as in offline mode:\n%s" items
      ) else (
        (* We're missing some. For each one, get the feed it came from
         * and find the corresponding <implementation> in that. This will
         * tell us where to get it from.
         * Note: we look for an implementation with the same ID. Maybe we
         * should check it has the same digest(s) too?
         *)
        let impl_of_sel sel =
          let id = ZI.get_attribute FeedAttr.id sel in
          let feed_url =
            match ZI.get_attribute_opt FeedAttr.from_feed sel with
            | Some feed -> feed
            | None -> ZI.get_attribute FeedAttr.interface sel in

          let parsed_feed_url = Feed_cache.parse_feed_url feed_url in
          let get_impl () : Feed.implementation option =
            match parsed_feed_url with
            | `distribution_feed master -> (
                match feed_provider#get_feed master with
                | None -> None
                | Some (master_feed, _) ->
                    match feed_provider#get_distro_impls master_feed with
                    | None -> None
                    | Some (impls, _) ->
                        try Some (impls |> List.find (fun impl -> Feed.get_attr_ex FeedAttr.id impl = id))
                        with Not_found -> None
            )
            | `remote_feed url | `local_feed url ->
                match feed_provider#get_feed url with
                | Some (feed, _) -> (
                    try Some (StringMap.find id feed.Feed.implementations)
                    with Not_found -> None
                )
                | _ -> None in

          match get_impl (), parsed_feed_url with
          | Some impl, _ -> `success impl |> Lwt.return
          | None, `distribution_feed _ -> raise_safe "Can't find requested package implementation %s:%s" feed_url id (* TODO fetch candidates? *)
          | None, `local_feed path -> raise_safe "Implementation '%s' not found in local feed '%s'" id path
          | None, (`remote_feed _ as parsed_feed_url) ->
              match_lwt self#download_and_import_feed parsed_feed_url with
              | `aborted_by_user -> Lwt.return `aborted_by_user
              | `no_update -> raise_safe "Failed to update feed '%s'" feed_url
              | `success new_root ->
                  Feed.parse config.system new_root None |> feed_provider#replace_feed feed_url;
                  match get_impl () with
                  | None -> raise_safe "Failed to download feed '%s'" feed_url
                  | Some impl -> `success impl |> Lwt.return in

        lwt results = missing |> List.map impl_of_sel |> collect_ex in
        let impls : Feed.implementation list ref = ref [] in
        let aborted = ref false in
        results |> List.iter (function
          | `success feed -> impls := feed :: !impls
          | `aborted_by_user -> aborted := true
          | `problem msg -> raise_safe "%s" msg);
        if !aborted then Lwt.return `aborted_by_user
        else fetcher#download_impls !impls
      )

    method fetcher = fetcher
    method config = config
    method distro = distro
    method slave = slave
  end
