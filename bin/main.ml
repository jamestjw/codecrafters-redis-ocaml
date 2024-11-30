open Lwt
open Redis

let default_backlog = 5 (* max number of concurrent clients *)
let default_address = "127.0.0.1"
let default_port = 6379

let rec handle_connection ic oc server () =
  let%lwt cmd = Parser.get_cmd ic in
  match cmd with
  | Some cmd ->
    let%lwt _ = Logs_lwt.info (fun m -> m "Received command %s" @@ Cmd.show cmd) in
    let%lwt resp = Server.execute_cmd cmd server in
    Lwt_io.write oc (Response.serialize resp) >>= handle_connection ic oc server
  | None -> Logs_lwt.info (fun m -> m "Connection closed")
;;

let accept_connection server conn =
  let fd, _ = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  let%lwt () = Logs_lwt.info (fun m -> m "New connection") in
  Lwt.on_failure (handle_connection ic oc server ()) (fun e ->
    Logs.err (fun m -> m "%s" (Printexc.to_string e)));
  return_unit
;;

let create_server_socket ~address ~port ~backlog =
  let open Lwt_unix in
  (* Create a TCP server socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  Lwt.async (fun _ -> bind sock (ADDR_INET (Unix.inet_addr_of_string address, port)));
  listen sock backlog;
  sock
;;

let create_server sock =
  let server = Server.mk () in
  let rec loop () = Lwt_unix.accept sock >>= accept_connection server >>= loop in
  let start () =
    Lwt.on_failure (Server.run server ()) (fun e ->
      Logs.err (fun m -> m "%s" (Printexc.to_string e)));
    loop ()
  in
  start
;;

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let server_socket =
    create_server_socket
      ~address:default_address
      ~port:default_port
      ~backlog:default_backlog
  in
  let serve = create_server server_socket in
  Lwt_main.run @@ serve ()
;;
