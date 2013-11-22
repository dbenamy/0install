(* Copyright (C) 2013, Thomas Leonard
 * See the README file for details, or visit http://0install.net.
 *)

(** Track executable bits (for Windows). *)

open Support.Common

module U = Support.Utils

(** Wrap a system and emulate the executable bit. We create one of these for each <recipe> we follow (on Windows). *)
let xbit_system (underlying:system) =
  if not on_windows then underlying else (
    let xbit_set = ref StringSet.empty in

    let record_x path = function
      | true -> xbit_set := StringSet.add path !xbit_set
      | false -> xbit_set := StringSet.remove path !xbit_set in

    let record_mode path mode = record_x path (mode land 0o111 <> 0) in

    object (self : #system)
      method argv = underlying#argv
      method isatty = underlying#isatty
      method print_string = underlying#print_string
      method time = underlying#time
      method with_open_in = underlying#with_open_in
      method readdir = underlying#readdir
      method reap_child = underlying#reap_child
      method waitpid_non_intr = underlying#waitpid_non_intr
      method getcwd = underlying#getcwd
      method chdir = underlying#chdir
      method getenv = underlying#getenv
      method environment = underlying#environment
      method readlink = underlying#readlink
      method platform = underlying#platform
      method running_as_root = underlying#running_as_root
      method file_exists = underlying#file_exists
      method exec = underlying#exec
      method create_process = underlying#create_process
      method set_mtime = underlying#set_mtime
      method symlink = underlying#symlink
      method spawn_detach = underlying#spawn_detach
      method bypass_dryrun = underlying#bypass_dryrun

      (* Implement using lstat *)
      method stat path = self#lstat (U.realpath (self :> system) path)

      method lstat path =
        underlying#lstat path |> pipe_some (fun info ->
          log_info "Items:";
          !xbit_set |> StringSet.iter (log_info "- %s");
          if StringSet.mem path !xbit_set then (
            log_info "Is X: %s" path;
            Some Unix.({
            info with st_perm = info.st_perm lor 0o111
          })) else Some info
        )

      method record_x path = record_x path true
      method chmod path mode = underlying#chmod path mode; record_mode path mode
      method mkdir path mode = underlying#mkdir path mode; record_mode path mode
      method with_open_out open_flags mode file fn = let r = underlying#with_open_out open_flags mode file fn in record_mode file mode; r
      method atomic_write open_flags path ~mode fn = let r = underlying#atomic_write open_flags path ~mode fn in record_mode path mode; r

      method hardlink orig copy = underlying#hardlink orig copy; record_x copy (StringSet.mem orig !xbit_set)
      method unlink path      = underlying#unlink path; record_x path false
      method rmdir path       = underlying#rmdir path; record_x path false

      method rename source target =
        log_info "RENAME %s -> %s" source target;
        underlying#rename source target;
        let old_xbits = !xbit_set in
        let add item acc =
          let new_item =
            if item = source then target
            else if U.starts_with item (source ^ Filename.dir_sep) then
              target ^ U.string_tail item (String.length source)
            else item in
          log_info "Rename %s -> %s" item new_item;
          StringSet.add new_item acc in
        xbit_set := StringSet.fold add old_xbits StringSet.empty 
    end
  )
