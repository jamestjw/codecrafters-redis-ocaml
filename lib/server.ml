open Core
module StringMap = Stdlib.Map.Make (String)

type t = { mailbox : (Cmd.t * Response.t Lwt_mvar.t) Lwt_mvar.t }

type state =
  { store : (string * float option) StringMap.t
  ; configs : string StringMap.t
  }

let mk_state ~rdb_dir ~rdb_filename =
  let do_key_value map key value expire_timestamp =
    let open Option.Let_syntax in
    let expire_timestamp =
      expire_timestamp
      >>| function
      | Parser.MILLISECS ms -> float_of_int ms /. 1000.0
      | Parser.SECS s -> float_of_int s
    in
    StringMap.add key (value, expire_timestamp) map
  in
  let do_db (map : (string * float option) StringMap.t) (db : Parser.database) =
    List.fold
      ~init:map
      ~f:(fun map { key; value; expire_timestamp } ->
        do_key_value
          map
          (Parser.str_encoding_to_str key)
          (Parser.str_encoding_to_str value)
          expire_timestamp)
      db.kv_pairs
  in
  let rdb_full_filename = Filename.concat rdb_dir rdb_filename in
  let store =
    match Sys_unix.file_exists rdb_full_filename with
    | `No | `Unknown ->
      Logs.info (fun m ->
        m "RDB file (%s) does not exist, starting with blank database" rdb_full_filename);
      StringMap.empty
    | `Yes ->
      let rdb_bytes = Utils.read_all_bytes rdb_filename in
      (match Parser.parse_rdb rdb_bytes with
       | Ok pdb ->
         Logs.info (fun m -> m "RDB file (%s) loaded" rdb_full_filename);
         List.fold ~init:StringMap.empty ~f:do_db pdb.databases
       | Error e ->
         Logs.warn (fun m -> m "Couldn't parse RDB file (%s): %s" rdb_filename e);
         StringMap.empty)
  in
  { store
  ; configs =
      StringMap.empty
      |> StringMap.add "dir" rdb_dir
      |> StringMap.add "dbfilename" rdb_filename
  }
;;

let mk () = { mailbox = Lwt_mvar.create_empty () }

let filter_expired (v, expiry) =
  match expiry with
  | Some timeout when Float.compare (Core_unix.time ()) timeout > 0 -> None
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

let handle_message cmd state =
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
      | Some (PX ms) -> Some (Core_unix.time () +. (Float.of_int ms /. 1000.0))
      | Some (EX secs) -> Some (Core_unix.time () +. Float.of_int secs)
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
  | Cmd.INVALID s -> Response.ERR s, state
;;

let run { mailbox } ~rdb_dir ~rdb_filename =
  let rec inner context =
    let%lwt cmd, response_mailbox = Lwt_mvar.take mailbox in
    let resp, context = handle_message cmd context in
    Lwt.async (fun _ -> Lwt_mvar.put response_mailbox resp);
    inner context
  in
  inner (mk_state ~rdb_dir ~rdb_filename)
;;

let execute_cmd cmd { mailbox } =
  let response_mailbox = Lwt_mvar.create_empty () in
  let%lwt _ = Lwt_mvar.put mailbox (cmd, response_mailbox) in
  Lwt_mvar.take response_mailbox
;;
