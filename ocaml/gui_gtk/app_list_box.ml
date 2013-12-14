(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** The app browser dialog *)

open Zeroinstall.General
open Support.Common

module F = Zeroinstall.Feed
module U = Support.Utils
module FC = Zeroinstall.Feed_cache
module FeedAttr = Zeroinstall.Constants.FeedAttr
module Feed_url = Zeroinstall.Feed_url
module Basedir = Support.Basedir

exception Found

(** Search through the configured XDG datadirs looking for .desktop files created by us. *)
let discover_existing_apps config =
  let re_exec = Str.regexp "^Exec=0launch \\(-- \\)?\\([^ ]*\\) " in
  let system = config.system in
  let already_installed = ref StringMap.empty in
  config.basedirs.Basedir.data |> List.iter (fun data_path ->
    let apps_dir = data_path +/ "applications" in
    if system#file_exists apps_dir then (
      match system#readdir apps_dir with
      | Problem ex -> log_warning ~ex "Failed to scan directory '%s'" apps_dir
      | Success items ->
          items |> Array.iter (fun desktop_file ->
            if U.starts_with desktop_file "zeroinstall-" && U.ends_with desktop_file ".desktop" then (
              let full = apps_dir +/ desktop_file in
              try
                full |> system#with_open_in [Open_rdonly] (fun ch ->
                  while true do
                    let line = input_line ch in
                    if Str.string_match re_exec line 0 then (
                      let uri = Str.matched_group 2 line in
                      already_installed := !already_installed |> StringMap.add uri full;
                      raise Found
                    )
                  done
                )
              with
              | End_of_file -> log_info "Failed to find Exec line in %s" full
              | Found -> ()
              | ex -> log_warning ~ex "Failed to load .desktop file %s" full
            )
          )
    )
  );
  !already_installed

(** Use [xdg-open] to show the help files for this implementation. *)
let show_help config sel =
  let system = config.system in
  let help_dir = ZI.get_attribute_opt FeedAttr.doc_dir sel in
  let id = ZI.get_attribute FeedAttr.id sel in

  let path =
    if U.starts_with id "package:" then (
      match help_dir with
      | None -> raise_safe "No doc-dir specified for package implementation"
      | Some help_dir ->
          if Filename.is_relative help_dir then
            raise_safe "Package doc-dir must be absolute! (got '%s')" help_dir
          else
            help_dir
    ) else (
      let path = Zeroinstall.Selections.get_path system config.stores sel |? lazy (raise_safe "BUG: not cached!") in
      match help_dir with
      | Some help_dir -> path +/ help_dir
      | None ->
          match Zeroinstall.Command.get_command "run" sel with
          | None -> path
          | Some run ->
              match ZI.get_attribute_opt "path" run with
              | None -> path
              | Some main ->
                  (* Hack for ROX applications. They should be updated to set doc-dir. *)
                  let help_dir = path +/ (Filename.dirname main) +/ "Help" in
                  if U.is_dir system help_dir then help_dir
                  else path
    ) in
  U.xdg_open_dir ~exec:false system path

let show_help_for_iface tools ~(gui:Zeroinstall.Ui.ui_handler) uri : unit Lwt.t =
  let config = tools#config in
  let distro = Zeroinstall.Distro_impls.get_host_distribution config in
  let reqs = Zeroinstall.Requirements.default_requirements uri in
  let sels =
    let feed_provider = new Zeroinstall.Feed_provider_impl.feed_provider config distro in
    match Zeroinstall.Solver.solve_for config feed_provider reqs with
    | (true, results) -> Some results#get_selections |> Lwt.return
    | (false, _) ->
        (* Slow path: program isn't cached yet *)
        match_lwt gui#run_solver tools `Download_only reqs ~refresh:false with
        | `Success sels -> Some sels |> Lwt.return
        | `Aborted_by_user -> None |> Lwt.return in
  match_lwt sels with
  | None -> Lwt.return ()    (* Aborted by user *)
  | Some sels ->
      let index = Zeroinstall.Selections.make_selection_map sels in
      let sel = StringMap.find_safe uri index in
      show_help config sel;
      Lwt.return ()

let confirm_deletion ~parent name =
  let box = GWindow.dialog
    ~parent
    ~title:"Confirm"
    () in
  let markup = Printf.sprintf "Remove <b>%s</b> from the applications list?" (Gtk_utils.pango_escape name) in
  GMisc.label ~packing:box#vbox#pack ~xpad:20 ~ypad:20 ~markup () |> ignore;
  box#add_button_stock `CANCEL `CANCEL;
  box#add_button_stock `DELETE `DELETE;
  let result, set_result = Lwt.wait () in
  box#set_default_response `DELETE;
  box#connect#response ~callback:(fun response ->
    box#destroy ();
    Lwt.wakeup set_result (
      match response with
      | `DELETE -> `delete
      | `CANCEL | `DELETE_EVENT -> `cancel
    )
  ) |> ignore;
  box#show ();
  result

let create config ~gui ~trust_db =
  let finished, set_finished = Lwt.wait () in
  let tools =
    let distro = Zeroinstall.Distro_impls.get_host_distribution config in
    let download_pool = Zeroinstall.Downloader.make_pool ~max_downloads_per_site:2 in
    object
      method config = config
      method distro = distro
      method make_fetcher watcher = new Zeroinstall.Fetch.fetcher config trust_db distro download_pool watcher
    end in

  let dialog = GWindow.dialog ~title:"0install Applications" () in

  let swin = GBin.scrolled_window
    ~packing:(dialog#vbox#pack ~expand:true)
    ~hpolicy:`NEVER
    ~vpolicy:`AUTOMATIC
    () in

  (* Model *)
  let cols = new GTree.column_list in
  let uri_col = cols#add Gobject.Data.string in
  let name_col = cols#add Gobject.Data.string in
  let icon_col = cols#add (Gobject.Data.gobject_by_name "GdkPixbuf") in

  let model = GTree.list_store cols in

  (* View *)
  let view = GTree.icon_view
    ~model:model
    ~packing:swin#add
    () in
  view#set_text_column name_col;
  view#set_pixbuf_column icon_col;

  (* Buttons *)
  dialog#add_button "Show Cache" `SHOW_CACHE;
  let actions = dialog#action_area in
  let cache_button = List.hd actions#children in
  cache_button#misc#set_tooltip_text "Show all 0install software currently stored on this computer \
    (i.e. those programs which can be run without a network connection). \
    This can be useful if you're running out of disk space and need to delete something.";
  dialog#action_area#set_child_secondary cache_button true;

  dialog#add_button_stock `ADD `ADD;
  let add_button = List.hd actions#children in
  add_button#misc#set_tooltip_text "Add a new application. You can also just drag a 0install feed URL from \
    your web-browser to this window.";

  dialog#add_button_stock `CLOSE `CLOSE;

  dialog#connect#response ~callback:(function
    | `DELETE_EVENT | `CLOSE -> dialog#destroy (); Lwt.wakeup set_finished ()
    | `SHOW_CACHE -> Gtk_utils.async (fun () -> Cache_explorer_box.open_cache_explorer config)
    | `ADD -> () (* TODO *)
  ) |> ignore;
  dialog#show ();

  (* Menu *)
  let menu = GMenu.menu () in

  let menu_iface = ref None in
  let run_item = GMenu.menu_item ~packing:menu#add ~label:"Run" () in
  let help_item = GMenu.menu_item ~packing:menu#add ~label:"Show help" () in
  let edit_item = GMenu.menu_item ~packing:menu#add ~label:"Choose versions" () in
  let delete_item = GMenu.menu_item ~packing:menu#add ~label:"Delete" () in

  let run uri =
    Gtk_utils.async ~parent:dialog (fun () ->
      Gdk.Window.set_cursor dialog#misc#window (Lazy.force Gtk_utils.busy_cursor);
      try_lwt
        config.system#spawn_detach [config.abspath_0install; "run"; "--"; uri];
        Lwt_unix.sleep 0.5
      finally
        Gdk.Window.set_cursor dialog#misc#window (Lazy.force Gtk_utils.default_cursor);
        Lwt.return ()
    ) in

  run_item#connect#activate ~callback:(fun () ->
    run (!menu_iface |? lazy (raise_safe "BUG: no selected item!"))
  ) |> ignore;

  help_item#connect#activate ~callback:(fun () ->
    let uri = !menu_iface |? lazy (raise_safe "BUG: no selected item!") in
    Gtk_utils.async ~parent:dialog (fun () -> show_help_for_iface tools ~gui uri)
  ) |> ignore;

  edit_item#connect#activate ~callback:(fun () ->
    let uri = !menu_iface |? lazy (raise_safe "BUG: no selected item!") in
    let reqs = Zeroinstall.Requirements.default_requirements uri in
    Gtk_utils.async ~parent:dialog (fun () ->
      lwt _ = gui#run_solver tools `Download_only reqs ~refresh:false in
      Lwt.return ()
    )
  ) |> ignore;

  delete_item#connect#activate ~callback:(fun () ->
    match view#get_selected_items with
    | [path] ->
        let row = model#get_iter path in
        let name = model#get ~row ~column:name_col in
        dialog#misc#set_sensitive false;
        Gtk_utils.async ~parent:dialog (fun () ->
          try_lwt
            match_lwt confirm_deletion ~parent:dialog name with
            | `delete -> model#remove row |> ignore; Lwt.return ()  (* TODO: actually delete it *)
            | `cancel -> Lwt.return ()
          finally
            dialog#misc#set_sensitive true;
            Lwt.return ()
        )
    | _ -> log_warning "Invalid selection!"
  ) |> ignore;

  view#event#connect#button_press ~callback:(fun bev ->
    let module B = GdkEvent.Button in
    let path_unsafe = view#get_path_at_pos (B.x bev |> truncate) (B.y bev |> truncate) in
    (* (a bug in lablgtk means the "option" part is missing) *)
    let path : Gtk.tree_path option = Obj.magic path_unsafe in
    match GdkEvent.get_type bev, B.button bev, path with
    | `TWO_BUTTON_PRESS, 1, Some path ->
        let row = model#get_iter path in
        run (model#get ~row ~column:uri_col);
        true
    | `BUTTON_PRESS, 3, Some path ->
        view#select_path path;
        let row = model#get_iter path in
        menu_iface := Some (model#get ~row ~column:uri_col);
        menu#popup ~button:(B.button bev) ~time:(B.time bev);
        true
    | _ ->
        false
  ) |> ignore;

  let default_icon = view#misc#render_icon ~size:`DIALOG `EXECUTE in

  (* We're missing gtk_icon_size_lookup, but can get it this way instead... *)
  let width = GdkPixbuf.get_width default_icon in
  let height = GdkPixbuf.get_height default_icon in

  (* Populate model *)
  discover_existing_apps config |> StringMap.iter (fun uri _path ->
    let url = Feed_url.master_feed_of_iface uri in
    let row = model#append () in
    let name =
      try
        match FC.get_cached_feed config url with
        | Some feed -> feed.F.name
        | None -> Filename.basename uri
      with Safe_exception _ ->
        Filename.basename uri in
    model#set ~row ~column:uri_col uri;
    model#set ~row ~column:name_col name;

    FC.get_cached_icon_path config url
    |> pipe_some (Gtk_utils.load_png_icon config.system ~width ~height)
    |> default default_icon
    |> model#set ~row ~column:icon_col
  );

  finished
