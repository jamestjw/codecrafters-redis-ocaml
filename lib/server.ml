open Core
open State

type t = { mailbox : (Cmd.t * Response.t Lwt_mvar.t) Lwt_mvar.t }

let mk () = { mailbox = Lwt_mvar.create_empty () }

let filter_expired (v, expiry) =
  match expiry with
  | Some timeout
    when Int63.compare (Time_now.nanoseconds_since_unix_epoch ()) timeout >= 0 -> None
  | _ -> Some v
;;

let get { store; _ } key = StringMap.find_opt key store |> Option.bind ~f:filter_expired

let set ({ store; _ } as state) key value expiry =
  { state with store = StringMap.add key (value, expiry) store }
;;

let get_keys { store; _ } =
  StringMap.to_list store
  |> List.filter_map ~f:(fun (k, (_, expiry)) -> filter_expired (k, expiry))
;;

let get_config { configs; _ } key = StringMap.find_opt key configs

let handle_message cmd ({ replication; _ } as state) =
  match cmd with
  | Cmd.PING -> Response.SIMPLE "PONG", state
  | Cmd.ECHO s -> Response.BULK s, state
  | Cmd.GET k ->
    let res =
      match get state k with
      | None -> Response.NULL_BULK
      | Some v -> Response.BULK v
    in
    res, state
  | Cmd.SET { set_key; set_value; set_timeout } ->
    let expiry =
      match set_timeout with
      | None -> None
      | Some (PX ms) ->
        Some Int63.(Time_now.nanoseconds_since_unix_epoch () + ms_to_ns ms)
      | Some (EX secs) ->
        Some Int63.(Time_now.nanoseconds_since_unix_epoch () + s_to_ns secs)
    in
    Response.SIMPLE "OK", set state set_key set_value expiry
  | Cmd.GET_CONFIG keys ->
    let res =
      List.map ~f:(fun k -> get_config state k |> Option.map ~f:(fun v -> [ k; v ])) keys
      |> List.filter_map ~f:(fun x -> x)
      |> Stdlib.List.flatten
      |> List.map ~f:(fun e -> Response.BULK e)
    in
    Response.ARRAY res, state
  | Cmd.KEYS query ->
    let matcher = Glob.glob_to_matcher query in
    let keys =
      get_keys state
      |> List.filter_map ~f:(fun e -> if matcher e then Some (Response.BULK e) else None)
    in
    Response.ARRAY keys, state
  | Cmd.INFO args -> Response.BULK (Replication.fetch_info replication args), state
  (* TODO: actually do something with these two *)
  | Cmd.REPL_CONF_PORT _ -> Response.SIMPLE "OK", state
  | Cmd.REPL_CONF_CAPA _ -> Response.SIMPLE "OK", state
  (* TODO: complete this *)
  | Cmd.PSYNC _ ->
    Response.SIMPLE (Printf.sprintf "FULLRESYNC %s 0" replication.replication_id), state
  | Cmd.INVALID s -> Response.ERR s, state
;;

let run { mailbox } ~rdb_dir ~rdb_filename ~replica_of ~listening_port =
  let state = mk_state ~rdb_dir ~rdb_filename ~replica_of in
  let rec inner context =
    let%lwt cmd, response_mailbox = Lwt_mvar.take mailbox in
    let resp, context = handle_message cmd context in
    Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp);
    inner context
  in
  if Option.is_some replica_of
  then
    Lwt.async (fun _ -> Replication.initiate_handshake state.replication listening_port);
  inner state
;;

let execute_cmd cmd { mailbox } =
  let response_mailbox = Lwt_mvar.create_empty () in
  let%lwt _ = Lwt_mvar.put mailbox (cmd, response_mailbox) in
  Lwt_mvar.take response_mailbox
;;
