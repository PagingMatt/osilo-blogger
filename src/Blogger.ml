open Osilo
open Core.Std
open Core.Std.Unix

exception Cannot_find_home
exception Could_not_write_config of int
exception Decoding_failed of string

let home = Sys.getenv "HOME" |> 
  begin function
  | Some h -> h
  | None   -> raise Cannot_find_home
  end

let dir  = (Printf.sprintf "%s/.osilo-blogger" home)

let string_member s j = 
  match Yojson.Basic.Util.member s j with
  | `String m -> m
  | _         -> raise (Decoding_failed s)

let assoc_member s j = 
  match Yojson.Basic.Util.member s j with
  | `Assoc m -> m
  | _        -> raise (Decoding_failed s)

let read_file file =
  let buf = String.make 65536 'x' in
  let len = read file ~buf        in
  String.prefix buf len

let read_config () = 
  let file = openfile ~mode:[O_RDONLY] (Printf.sprintf "%s/config.json" dir) in
  let f = read_file file in
  let j = Yojson.Basic.from_string f in
  let p = (string_member "peer" j |> Peer.create          ) in
  let k = (string_member "key" j  |> Coding.decode_cstruct) in
  (close file); (p,k)

open Osilo.Auth
open Lwt.Infix

let init_listing my_peer key = 
  let plaintext = (`Assoc [("posts/list.json",`Assoc [])]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = "/client/set/local/blogger" in
  Http_client.post ~peer:my_peer ~path ~body
  >|= fun _ -> ()

let init ~peer ~key =
  mkdir ~perm:0o700 dir;
  let buffer  = Yojson.Basic.to_string (`Assoc [("peer", `String (Peer.host peer)); ("key", `String key)]) in
  let file = openfile ~perm:0o600 ~mode:[O_WRONLY;O_CREAT] (Printf.sprintf "%s/config.json" dir) in
  single_write file ~buf:buffer |> fun _ -> (close file); init_listing peer (Coding.decode_cstruct key)

let invite ~peer =
  let my_peer,key = read_config () in 
  let plaintext = (`Assoc [("R", `String "posts")]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = Printf.sprintf "/client/permit/%s/blogger" (Peer.host peer) in
  Http_client.post ~peer:my_peer ~path ~body
  >|= (fun (c,_) -> 
    if c=204 
    then (Printf.printf "Peer %s has successfully been permitted to read the blog."       (Peer.host peer)) 
    else (Printf.printf "There was a problem giving peer %s permission to read the blog." (Peer.host peer)))

let update_post_list title id my_peer key =
  let plaintext = (`List [`String "posts/list.json"]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = Printf.sprintf "/client/get/local/blogger" in
  Http_client.post ~peer:my_peer ~path ~body
  >|= (fun (c,b) -> Coding.decode_client_message b) 
  >|= (fun (ciphertext,iv) -> Cryptography.CS.decrypt' ~key ~ciphertext ~iv)
  >|= Cstruct.to_string
  >|= Yojson.Basic.from_string
  >|= assoc_member "posts/list.json"
  >|= (fun l -> 
    let entry = `Assoc ((id,`String title)::l) in
    let plaintext = (`Assoc [("posts/list.json",entry)]) |> Yojson.Basic.to_string |> Cstruct.of_string in
    let c',i' = Cryptography.CS.encrypt' ~key ~plaintext in
    let body' = Coding.encode_client_message ~ciphertext:c' ~iv:i' in
    let path = "/client/set/local/blogger" in
    Http_client.post ~peer:my_peer ~path ~body:body'
    >|= fun _ -> ())

let publish_post title id post my_peer key =
  let name = Printf.sprintf "posts/content/%s.json" id in
  let post_content = openfile ~mode:[O_RDONLY] post |> read_file in
  let content = `Assoc [("title",`String title);("content",`String post_content)] in
  let plaintext = (`Assoc [(name,content)]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = "/client/set/local/blogger" in
  Http_client.post ~peer:my_peer ~path ~body
  >|= fun _ -> ()

let post ~title ~post =
  let my_peer,key = read_config () in
  let id = Nocrypto.Rng.generate 16 |> Coding.encode_cstruct |> String.filter ~f:(fun c -> not(c='/')) in
  update_post_list title id my_peer key;
  publish_post title id post my_peer key

let read ~peer ~id = 
  let my_peer,key = read_config () in
  let name = Printf.sprintf "posts/content/%s.json" id in
  let plaintext = (`List [(`Assoc [
    ("path"       ,`String name);
    ("check_cache", `Bool true ); 
    ("write_back" , `Bool true )])]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = Printf.sprintf "/client/get/%s/blogger" (Peer.host peer) in
  Http_client.post ~peer:my_peer ~path ~body
  >|= (fun (c,b) -> Coding.decode_client_message b) 
  >|= (fun (ciphertext,iv) -> Cryptography.CS.decrypt' ~key ~ciphertext ~iv)
  >|= Cstruct.to_string
  >|= Yojson.Basic.from_string
  >|= (fun json -> `Assoc (assoc_member name json))
  >|= (fun j -> 
    let title = string_member "title" j in
    let post  = string_member "content" j in
    Printf.printf "%s:\n\n%s\n\n" title post)

let read_my ~id = 
  let my_peer,key = read_config () in
  let name = Printf.sprintf "posts/content/%s.json" id in
  let plaintext = (`List [`String name]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = Printf.sprintf "/client/get/local/blogger" in
  Http_client.post ~peer:my_peer ~path ~body
  >|= (fun (c,b) -> Coding.decode_client_message b) 
  >|= (fun (ciphertext,iv) -> Cryptography.CS.decrypt' ~key ~ciphertext ~iv)
  >|= Cstruct.to_string
  >|= Yojson.Basic.from_string
  >|= (fun json -> `Assoc (assoc_member name json))
  >|= (fun j -> 
    let title = string_member "title" j in
    let post  = string_member "content" j in
    Printf.printf "%s:\n\n%s\n\n" title post)

let show ~peer = 
  let my_peer,key = read_config () in
  let plaintext = (`List [(`Assoc [
    ("path"       ,`String "posts/list.json");
    ("check_cache", `Bool  false            ); 
    ("write_back" , `Bool  false            )])]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = Printf.sprintf "/client/get/%s/blogger" (Peer.host peer) in
  Http_client.post ~peer:my_peer ~path ~body
  >|= (fun (c,b) -> Coding.decode_client_message b) 
  >|= (fun (ciphertext,iv) -> Cryptography.CS.decrypt' ~key ~ciphertext ~iv)
  >|= Cstruct.to_string
  >|= Yojson.Basic.from_string
  >|= assoc_member "posts/list.json"
  >|= (fun l -> Printf.printf "Posts from %s:\n" (Peer.host peer);
    List.iter l ~f:(fun (i',(`String t)) -> Printf.printf "\"%s\" (ID: %s)\n" t i'))

let show_my () = 
  let my_peer,key = read_config () in
  let plaintext = (`List [`String "posts/list.json"]) |> Yojson.Basic.to_string |> Cstruct.of_string in
  let c,i = Cryptography.CS.encrypt' ~key ~plaintext in
  let body = Coding.encode_client_message ~ciphertext:c ~iv:i in
  let path = Printf.sprintf "/client/get/local/blogger" in
  Http_client.post ~peer:my_peer ~path ~body
  >|= (fun (c,b) -> Coding.decode_client_message b) 
  >|= (fun (ciphertext,iv) -> Cryptography.CS.decrypt' ~key ~ciphertext ~iv)
  >|= Cstruct.to_string
  >|= Yojson.Basic.from_string
  >|= assoc_member "posts/list.json"
  >|= (fun l -> Printf.printf "My posts:\n";
    List.iter l ~f:(fun (i',(`String t)) -> Printf.printf "\"%s\" (ID: %s)\n" t i'))

module Cli = struct 
  let init =
    Command.basic
      ~summary:"Initialises a blogger client for the local user."
      Command.Spec.(
        empty
        +> flag "-p" (required string) ~doc:" Hostname of peer to blog from."
        +> flag "-k" (required string) ~doc:" Secret key to share with peer."
      )
      (fun p k () -> Lwt_main.run (init ~peer:(Peer.create p) ~key:k))

  let invite =
    Command.basic
      ~summary:"Invites another peer to read this blog."
      Command.Spec.(
        empty
        +> flag "-p" (required string) ~doc:" Hostname of peer to invite."
      )
      (fun p () -> Lwt_main.run (invite ~peer:(Peer.create p)))

  let post =
    Command.basic
      ~summary:"Publish a blog post."
      Command.Spec.(
        empty
        +> flag "-t" (required string) ~doc:" Post title."
        +> flag "-f" (required string) ~doc:" File containing post contents."
      )
      (fun t f () -> Lwt_main.run (post ~title:t ~post:f))

  let read =
    Command.basic
      ~summary:"Read one of another peer's blog posts."
      Command.Spec.(
        empty
        +> flag "-p" (required string) ~doc:" Peer blog post is from."
        +> flag "-i" (required string) ~doc:" ID of post to read."
      )
      (fun p i () -> Lwt_main.run (read ~peer:(Peer.create p) ~id:i))

  let read_my =
    Command.basic
      ~summary:"Read one of my blog posts."
      Command.Spec.(
        empty
        +> flag "-i" (required string) ~doc:" ID of post to read."
      )
      (fun i () -> Lwt_main.run (read_my ~id:i))

  let show =
    Command.basic
      ~summary:"Show a summary of a peer's posts."
      Command.Spec.(
        empty
        +> flag "-p" (required string) ~doc:" Peer to get summary for."
      )
      (fun p () -> Lwt_main.run (show ~peer:(Peer.create p)))

  let show_my =
    Command.basic
      ~summary:"Show a summary of my posts."
      Command.Spec.(
        empty
      )
      (fun () -> Lwt_main.run (show_my ()))

  let commands = 
    Command.group 
      ~summary:"CLI for the osilo blogger."
      [("init",init);("invite",invite);("post",post);("read",read);("read-my",read_my);("show",show);("show-my",show_my)]
end

let () = 
  Command.run
    ~version:"0.1"
    ~build_info:"osilo-blogger"
    Cli.commands
    