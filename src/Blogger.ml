open Core.Std

let () = 
  Command.run
    ~version:"0.1"
    ~build_info:"osilo-blogger"
    Cli.commands
    