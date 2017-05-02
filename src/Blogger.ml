open Core.Std

let init =
  Command.basic
    ~summary:"Initialises a blogger client for the local user."
    Command.Spec.(
      empty
      +> flag "-p" (required string) ~doc:" Hostname of peer to blog from."
      +> flag "-n" (required string) ~doc:" Port of peer to blog from."
      +> flag "-k" (required string) ~doc:" Secret key to share with peer."
    )
    (fun p n k () -> Lwt_main.run (Client.init ~peer:p ~port:n ~key:k))

let invite =
  Command.basic
    ~summary:"Invites another peer to read this blog."
    Command.Spec.(
      empty
      +> flag "-p" (required string) ~doc:" Hostname of peer to invite."
    )
    (fun p () -> Lwt_main.run (Client.invite ~peer:p))

let invite_post =
  Command.basic
    ~summary:"Invites another peer to read a post on this blog."
    Command.Spec.(
      empty
      +> flag "-p" (required string) ~doc:" Hostname of peer to invite."
      +> flag "-i" (required string) ~doc:" ID of post to invite peer to read."
    )
    (fun p i () -> Lwt_main.run (Client.invite_post ~peer:p ~id:i))

let post =
  Command.basic
    ~summary:"Publish a blog post."
    Command.Spec.(
      empty
      +> flag "-t" (required string) ~doc:" Post title."
      +> flag "-f" (required string) ~doc:" File containing post contents."
    )
    (fun t f () -> Lwt_main.run (Client.post ~title:t ~post:f))

let read =
  Command.basic
    ~summary:"Read one of another peer's blog posts."
    Command.Spec.(
      empty
      +> flag "-p" (required string) ~doc:" Peer blog post is from."
      +> flag "-i" (required string) ~doc:" ID of post to read."
    )
    (fun p i () -> Lwt_main.run (Client.read ~peer:p ~id:i))

let read_my =
  Command.basic
    ~summary:"Read one of my blog posts."
    Command.Spec.(
      empty
      +> flag "-i" (required string) ~doc:" ID of post to read."
    )
    (fun i () -> Lwt_main.run (Client.read_my ~id:i))

let remove =
  Command.basic
    ~summary:"Removes one of my blog posts from my silo and remote caches."
    Command.Spec.(
      empty
      +> flag "-i" (required string) ~doc:" ID of post to remove."
    )
    (fun i () -> Lwt_main.run (Client.remove ~id:i))

let show =
  Command.basic
    ~summary:"Show a summary of a peer's posts."
    Command.Spec.(
      empty
      +> flag "-p" (required string) ~doc:" Peer to get summary for."
    )
    (fun p () -> Lwt_main.run (Client.show ~peer:p))

let show_my =
  Command.basic
    ~summary:"Show a summary of my posts."
    Command.Spec.(
      empty
    )
    (fun () -> Lwt_main.run (Client.show_my ()))

let commands =
  Command.group
    ~summary:"CLI for the osilo blogger."
    [("init",init);("invite",invite);("invite-post",invite_post);("post",post);("read",read);("read-my",read_my);("remove",remove);("show",show);("show-my",show_my)]

let () =
  Command.run
    ~version:"0.1"
    ~build_info:"osilo-blogger"
    commands
