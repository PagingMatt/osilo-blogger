open Osilo
open Core.Std
open Core.Std.Unix

exception Cannot_find_home
exception Could_not_write_config of int

let init ~peer ~key =
  let home = Sys.getenv "HOME" |> 
    begin function
    | Some h -> h
    | None   -> raise Cannot_find_home
    end
  in
  let dir  = (Printf.sprintf "%s/.osilo-blogger" home) in
  mkdir ~perm:0o700 dir;
  let buf  = Yojson.Basic.to_string (`Assoc [("peer", `String (Peer.host peer)); ("key", `String key)]) in
  let file = openfile ~perm:0o600 ~mode:[O_WRONLY;O_CREAT] (Printf.sprintf "%s/config.json" dir) in
  single_write file ~buf |> fun _ -> ()

module Cli = struct 
  let init =
    Command.basic
      ~summary:"Initialises a blogger client for the local user."
      Command.Spec.(
        empty
        +> flag "-p" (required string) ~doc:" Hostname of peer to blog from."
        +> flag "-k" (required string) ~doc:" Secret key to share with peer."
      )
      (fun p k () -> init ~peer:(Peer.create p) ~key:k)

  let commands = 
    Command.group 
      ~summary:"CLI for the osilo blogger."
      [("init",init);]
end

let () = 
  Command.run
    ~version:"0.1"
    ~build_info:"osilo-blogger"
    Cli.commands
    