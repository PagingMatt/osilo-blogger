open Lwt
open Lwt.Infix
open Core.Std
open Cohttp
open Cohttp_lwt_unix
open Core.Std.Unix

exception Cannot_find_home
exception Could_not_write_config of int
exception Decoding_failed of string

let build_uri ~peer ~port ~path =
  Uri.make ~scheme:"https" ~host:peer ~port ~path:path ()

let handle_http_resp (r,b) =
  let code = r |> Response.status |> Code.code_of_status in
  let body = Cohttp_lwt_body.to_string b in body
  >|= fun bdy -> (code,bdy)

let https_post ~peer ~port ~path ~body ~key =
  let uri = build_uri ~peer ~path ~port in
  (Cohttp_lwt_unix.Client.post ~body:(Cohttp_lwt_body.of_string body)
     ~headers:(Header.add_authorization (Header.init ()) (`Other key)) uri)
  >>= handle_http_resp

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
  let p = (string_member "peer" j) in
  let n = (string_member "port" j) |> int_of_string in
  let k = (string_member "key"  j) in
  (close file); (p,n,k)

open Lwt.Infix

let init_listing my_peer my_port key =
  let body = (`Assoc [("posts/list.json",`Assoc [])]) |> Yojson.Basic.to_string in
  let path = "/client/set/local/blogger" in
  https_post ~peer:my_peer ~port:(int_of_string my_port) ~path ~body ~key
  >|= fun (_,_) -> ()

let init ~peer ~port ~key =
  mkdir ~perm:0o700 dir;
  let buffer  = Yojson.Basic.to_string (`Assoc [("peer", `String peer); ("port", `String port); ("key", `String key)]) in
  let file = openfile ~perm:0o600 ~mode:[O_WRONLY;O_CREAT] (Printf.sprintf "%s/config.json" dir) in
  single_write file ~buf:buffer |> fun _ -> (close file); init_listing peer port key

let invite ~peer =
  let my_peer,my_port,key = read_config () in
  let body = (`Assoc [("R", `String "posts")]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/permit/%s/blogger" peer in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,_) ->
      if c=204
      then (Printf.printf "Peer %s has successfully been permitted to read the blog."       peer)
      else (Printf.printf "There was a problem giving peer %s permission to read the blog." peer))

let invite_post ~peer ~id =
  let my_peer,my_port,key = read_config () in
  let body =
    (`Assoc [("R", `String (Printf.sprintf "posts/content/%s.json" id))])
    |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/permit/%s/blogger" peer in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,_) ->
      if c=204
      then (Printf.printf "Peer %s has successfully been permitted to read %s on the blog."    peer id)
      else (Printf.printf "There was a problem giving peer %s permission to %s read the blog." peer id))

let update_post_list title id my_peer my_port key =
  let body = (`List [`String "posts/list.json"]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/local/blogger" in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,b) ->
      Yojson.Basic.from_string b
      |> assoc_member "posts/list.json")
  >|= (fun l ->
      let entry = `Assoc ((id,`String title)::l) in
      let body' = (`Assoc [("posts/list.json",entry)]) |> Yojson.Basic.to_string in
      let path' = "/client/set/local/blogger" in
      https_post ~peer:my_peer ~port:my_port ~path:path' ~body:body' ~key
      >|= fun (_,_) -> ())

let publish_post title id post my_peer my_port key =
  let name = Printf.sprintf "posts/content/%s.json" id in
  let post_content = openfile ~mode:[O_RDONLY] post |> read_file in
  let content = `Assoc [("title",`String title);("content",`String post_content)] in
  let body = (`Assoc [(name,content)]) |> Yojson.Basic.to_string in
  let path = "/client/set/local/blogger" in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= fun (_,_) -> ()

let post ~title ~post =
  let my_peer,my_port,key = read_config () in
  let id = Nocrypto.Rng.generate 16 |> Nocrypto.Base64.encode |> Cstruct.to_string |> String.filter ~f:(fun c -> not(c='/')) in
  update_post_list title id my_peer my_port key
  >>= fun _ -> publish_post title id post my_peer my_port key

let read ~peer ~id =
  let my_peer,my_port,key = read_config () in
  let name = Printf.sprintf "posts/content/%s.json" id in
  let body = (`List [(`Assoc [
      ("path"       ,`String name);
      ("check_cache", `Bool true );
      ("write_back" , `Bool true )])]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/%s/blogger" peer in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,b) -> `Assoc
          (Yojson.Basic.from_string b
           |> assoc_member name))
  >|= (fun j ->
      let title = string_member "title" j in
      let post  = string_member "content" j in
      Printf.printf "%s:\n\n%s\n\n" title post)

let read_my ~id =
  let my_peer,my_port,key = read_config () in
  let name = Printf.sprintf "posts/content/%s.json" id in
  let body = (`List [`String name]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/local/blogger" in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,b) -> `Assoc
          (Yojson.Basic.from_string b
           |> assoc_member name))
  >|= (fun j ->
      let title = string_member "title" j in
      let post  = string_member "content" j in
      Printf.printf "%s:\n\n%s\n\n" title post)

let remove_from_listing my_peer my_port key id =
  let body = (`List [`String "posts/list.json"]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/local/blogger" in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,b) ->
      (Yojson.Basic.from_string b
       |> assoc_member "posts/list.json"))
  >>= (fun l ->
      let entry = `Assoc (List.filter l ~f:(fun (id',_) -> not(id=id'))) in
      let body' = (`Assoc [("posts/list.json",entry)]) |> Yojson.Basic.to_string in
      let path = "/client/set/local/blogger" in
      https_post ~peer:my_peer ~port:my_port ~path ~body:body' ~key)

let remove' my_peer my_port key name =
  let body = (`List [`String name]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/del/local/blogger" in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >>= (fun (_,_) ->
      let path' = Printf.sprintf "/client/inv/blogger" in
      https_post ~peer:my_peer ~port:my_port ~path:path' ~body ~key)
  >|= (fun (_,_) -> ())

let remove ~id =
  let my_peer,my_port,key = read_config () in
  let name = Printf.sprintf "posts/content/%s.json" id in
  remove_from_listing my_peer my_port key id
  >>= (fun (_,_) -> remove' my_peer my_port key name)

let show ~peer =
  let my_peer,my_port,key = read_config () in
  let body = (`List [(`Assoc [
      ("path"       ,`String "posts/list.json");
      ("check_cache", `Bool  false            );
      ("write_back" , `Bool  false            )])]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/%s/blogger" peer in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,b) ->
      (Yojson.Basic.from_string b
       |> assoc_member "posts/list.json"))
  >|= (fun l -> Printf.printf "Posts from %s:\n" peer;
        List.iter l ~f:(fun (i',(`String t)) -> Printf.printf "\"%s\" (ID: %s)\n" t i'))

let show_my () =
  let my_peer,my_port,key = read_config () in
  let body = (`List [`String "posts/list.json"]) |> Yojson.Basic.to_string in
  let path = Printf.sprintf "/client/get/local/blogger" in
  https_post ~peer:my_peer ~port:my_port ~path ~body ~key
  >|= (fun (c,b) ->
      (Yojson.Basic.from_string b
       |> assoc_member "posts/list.json"))
  >|= (fun l -> Printf.printf "My posts:\n";
        List.iter l ~f:(fun (i',(`String t)) -> Printf.printf "\"%s\" (ID: %s)\n" t i'))
