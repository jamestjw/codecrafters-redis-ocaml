(* Client that replicas use to communicate with master *)
open Core
open Lwt_unix
open Lwt

let default_replication_capabilities = "psync2"

let connect_to_server ~host ~port =
  let%lwt _ = Logs_lwt.info (fun m -> m "Connecting to server...") in
  (* Create a socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  (* Prepare the server address *)
  let server_addr = ADDR_INET (Core_unix.Inet_addr.of_string_or_getbyname host, port) in
  (* Connect to the server *)
  let%lwt _ = Lwt_unix.connect sock server_addr in
  let in_channel = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  let out_channel = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  let%lwt _ = Logs_lwt.info (fun m -> m "Successfully connected to master.") in
  return (sock, in_channel, out_channel)
;;

let cmd_to_str cmd =
  let list_to_bulk = List.map ~f:(fun e -> Response.BULK e) in
  let args =
    match cmd with
    | Cmd.PING -> list_to_bulk [ "PING" ]
    | Cmd.REPL_CONF_PORT port ->
      list_to_bulk [ "REPLCONF"; "listening-port"; string_of_int port ]
    | Cmd.REPL_CONF_CAPA capabilities -> list_to_bulk [ "REPLCONF"; "capa"; capabilities ]
    | Cmd.PSYNC (id, offset) -> list_to_bulk [ "PSYNC"; id; string_of_int offset ]
    | _ -> failwith "not implemented yet"
  in
  Response.(serialize @@ ARRAY args)
;;

let send_request oc cmd =
  let%lwt _ = Lwt_io.write oc (cmd_to_str cmd) in
  Lwt.return_unit
;;

let send_ping (ic, oc) =
  let%lwt () = send_request oc Cmd.PING in
  match%lwt Parser.parse_simple ic with
  | Parsed "PONG" -> return @@ Ok ()
  | Parsed msg ->
    return
    @@ Error (Printf.sprintf "invalid response, expected 'PONG' but got %s instead" msg)
  | Disconnected -> return @@ Error (Printf.sprintf "no response from server")
  | InvalidFormat s -> return @@ Error (Printf.sprintf "invalid response to PING: %s" s)
;;

let send_replication_config (ic, oc) port =
  let%lwt _ = Logs_lwt.info (fun m -> m "Sending listening port %d to master" port) in
  let%lwt () = send_request oc @@ Cmd.REPL_CONF_PORT port in
  match%lwt Parser.parse_simple ic with
  | Parsed "OK" ->
    let%lwt _ =
      Logs_lwt.info (fun m ->
        m "Sending replication capabilities %s to master" default_replication_capabilities)
    in
    let%lwt () = send_request oc @@ Cmd.REPL_CONF_CAPA default_replication_capabilities in
    (match%lwt Parser.parse_simple ic with
     | Parsed "OK" -> return @@ Ok ()
     | Parsed msg ->
       return
       @@ Error (Printf.sprintf "invalid response, expected 'OK' but got %s instead" msg)
     | Disconnected -> return @@ Error (Printf.sprintf "no response from server")
     | InvalidFormat s ->
       return @@ Error (Printf.sprintf "invalid response to REPLCONF capa: %s" s))
  | Parsed msg ->
    return
    @@ Error (Printf.sprintf "invalid response, expected 'PONG' but got %s instead" msg)
  | Disconnected -> return @@ Error (Printf.sprintf "No response from server to REPLCONF")
  | InvalidFormat s ->
    return @@ Error (Printf.sprintf "invalid response to REPLCONF listening-port: %s" s)
;;

let initiate_replication_stream (ic, oc) =
  let psync_response_regex = Str.regexp {|^FULLRESYNC \(.*\) \([0-9]+\)$|} in
  let%lwt _ = Logs_lwt.info (fun m -> m "Initiating replication stream") in
  let%lwt () = send_request oc @@ Cmd.PSYNC ("?", -1) in
  match%lwt Parser.parse_simple ic with
  | Parsed res when Str.string_match psync_response_regex res 0 ->
    (* TODO: actually do something with this *)
    return @@ Ok ()
  | Parsed s -> return @@ Error (Printf.sprintf "invalid response to PSYNC: %s" s)
  | Disconnected ->
    return
    @@ Error (Printf.sprintf "No response from server to replication stream initiation")
  | InvalidFormat s -> return @@ Error (Printf.sprintf "invalid response to PSYNC: %s" s)
;;

let close_connection sock = Lwt_unix.close sock
