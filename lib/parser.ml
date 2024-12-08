open Core
open Lwt
module StringMap = Stdlib.Map.Make (String)

let num_args_regex = Str.regexp {|\*\([0-9]+\)|}
let arg_len_regex = Str.regexp {|\$\([0-9]+\)|}

type 'a parse_result =
  | Disconnected
  | InvalidFormat of string
  | Parsed of 'a

let lower_fst = function
  | [] -> []
  | x :: xs -> String.lowercase x :: xs
;;

let parse_set_cmd = function
  | set_key :: set_value :: args ->
    let rec helper args (set_key, set_value, set_timeout) =
      match lower_fst args with
      | [] -> Cmd.SET { set_key; set_value; set_timeout }
      | "px" :: timeout :: rest ->
        (match Stdlib.int_of_string_opt timeout, set_timeout with
         | _, Some _ -> Cmd.INVALID "timeout set twice for 'SET' command"
         | Some timeout, None when timeout > 0 ->
           helper rest (set_key, set_value, Some (Cmd.PX timeout))
         | _ -> Cmd.INVALID "'SET' requires positive integer for timeout")
      | "ex" :: timeout :: rest ->
        (match Stdlib.int_of_string_opt timeout, set_timeout with
         | _, Some _ -> Cmd.INVALID "timeout set twice for 'SET' command"
         | Some timeout, None when timeout > 0 ->
           helper rest (set_key, set_value, Some (Cmd.EX timeout))
         | _ -> Cmd.INVALID "'SET' requires positive integer for timeout")
      | arg :: _ -> Cmd.INVALID (Printf.sprintf "unknown arg '%s' for 'SET'" arg)
    in
    helper args (set_key, set_value, None)
  | _ -> Cmd.INVALID "invalid args for set, key-value pair required"
;;

let parse_ping_cmd = function
  | [] -> Cmd.PING
  | _ -> Cmd.INVALID "'PING' takes no args"
;;

let parse_echo_cmd = function
  | [ e ] -> Cmd.ECHO e
  | _ -> Cmd.INVALID "'ECHO' takes one arg"
;;

let parse_get_cmd = function
  | [ key ] -> Cmd.GET key
  | _ -> Cmd.INVALID "'GET' takes one arg"
;;

let parse_config_cmd args =
  match lower_fst args with
  | [ "get" ] -> Cmd.INVALID "wrong number of arguments for 'config|get' command"
  | "get" :: keys -> Cmd.GET_CONFIG keys
  | _ -> Cmd.INVALID "invalid 'CONFIG' subcommand"
;;

let parse_keys_cmd args =
  match lower_fst args with
  | [ query ] -> Cmd.KEYS query
  | _ -> Cmd.INVALID "'KEYS' takes one arg"
;;

let args_to_cmd args =
  match lower_fst args with
  | "ping" :: args -> parse_ping_cmd args
  | "echo" :: args -> parse_echo_cmd args
  | "get" :: args -> parse_get_cmd args
  | "set" :: args -> parse_set_cmd args
  | "config" :: args -> parse_config_cmd args
  | "keys" :: args -> parse_keys_cmd args
  | cmd :: _ -> Cmd.INVALID (Printf.sprintf "unrecognised command %s" cmd)
  | _ -> Cmd.INVALID "invalid command"
;;

(* Polls the input channel for a valid command, if this returns None this
   means that the connection has been dropped by the client. *)
let rec get_cmd ic =
  let parse_len regexp =
    let%lwt msg = Lwt_io.read_line_opt ic in
    match msg with
    | None -> return Disconnected
    | Some msg ->
      if Str.string_match regexp msg 0
      then return @@ Parsed (Stdlib.int_of_string @@ Str.matched_group 1 msg)
      else return @@ InvalidFormat msg
  in
  let parse_args num_args =
    let rec parse_args' num_args acc =
      if num_args = 0
      then return @@ Parsed (List.rev acc)
      else (
        let%lwt arg_len = parse_len arg_len_regex in
        match arg_len with
        | Disconnected -> return Disconnected
        | InvalidFormat s ->
          let%lwt _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
          return (InvalidFormat s)
        | Parsed arg_len ->
          let%lwt msg = Lwt_io.read_line_opt ic in
          (match msg with
           | None -> return Disconnected
           | Some arg when String.length arg <> arg_len ->
             let%lwt _ =
               Logs_lwt.err (fun m -> m "Argument (%s) length != %d" arg arg_len)
             in
             return @@ InvalidFormat arg
           | Some arg -> parse_args' (num_args - 1) (arg :: acc)))
    in
    parse_args' num_args []
  in
  let%lwt num_args = parse_len num_args_regex in
  match num_args with
  | Disconnected -> return None
  | InvalidFormat s ->
    let%lwt _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
    get_cmd ic
  | Parsed num_args when num_args <= 0 -> get_cmd ic
  | Parsed num_args ->
    let%lwt arg_list = parse_args num_args in
    (match arg_list with
     | Disconnected -> return None
     | InvalidFormat s -> return @@ Some (Cmd.INVALID s)
     | Parsed arg_list -> return @@ Some (args_to_cmd arg_list))
;;

(* RDB file, parsed based on the specs in https://rdb.fnordig.de/file_format.html *)

type header = { rdb_version : int }

and str_encoding =
  | LP_STR of string
  | INTEGER of int
  (* bytes and uncompressed_len *)
  | COMPRESSED_STR of int list * int

(* TODO: implement rest *)
and metadata = (str_encoding StringMap.t[@opaque])

and expire_timestamp =
  | SECS of int
  | MILLISECS of int

and kv_pair =
  { key : str_encoding
  ; value : str_encoding
  ; expire_timestamp : expire_timestamp option
  }

and database =
  { db_index : int
  ; hash_tbl_sz : int
  ; expire_hash_tbl_sz : int
  ; kv_pairs : kv_pair list
  }

and rdb =
  { header : header
  ; metadata : metadata
  ; databases : database list
  ; checksum : string
  }
[@@deriving show { with_path = false }]

let str_encoding_to_str = function
  | LP_STR s -> s
  | INTEGER i -> string_of_int i
  | _ -> failwith "not implemented yet"
;;

let hex_str_to_int_opt s = Stdlib.int_of_string_opt (Printf.sprintf "0x%s" s)
let ascii_hex_to_str l = List.map ~f:(fun e -> Char.of_int_exn e) l |> String.of_list

let parse_length (bytes : int list) =
  match bytes with
  | [] -> Error "malformed length prefix"
  (* First 2 bits of the byte are 00 *)
  | b :: rest when Int.(b land 0b11000000) = 0 -> Ok (b, rest)
  (* First 2 bits of the first byte are 01 *)
  | b1 :: b2 :: rest when Int.(b1 land 0b11000000) = 0b01000000 ->
    (* Take the last 6 bits of the first byte, join them to the next byte *)
    Ok (Int.shift_left Int.(b1 land 0b111111) 8 + b2, rest)
  (* First 2 bits of the first byte are 10, the next 4 bytes are the length *)
  | b1 :: b2 :: b3 :: b4 :: b5 :: rest when Int.(b1 land 0b11000000) = 0b10000000 ->
    Ok (Int.((b2 lsl 24) + (b3 lsl 16) + (b4 lsl 8) + b5), rest)
  | _ -> Error "invalid prefix length format"
;;

let list_split_n_opt l n =
  let left, right = List.split_n l n in
  if List.length left <> n then None else Some (left, right)
;;

let parse_length_prefixed_string bytes =
  let open Result.Let_syntax in
  let%bind len, bytes = parse_length bytes in
  let%bind str_bytes, rest =
    Result.of_option
      ~error:"insufficient bytes in prefixed length string"
      (list_split_n_opt bytes len)
  in
  return (ascii_hex_to_str str_bytes, rest)
;;

let little_endian_to_int l =
  List.mapi ~f:(fun i v -> Int.(v lsl (8 * i))) l |> List.fold ~f:( + ) ~init:0
;;

let is_special_format bytes =
  match bytes with
  | byte :: _ when Int.(byte land 0b11000000) = 0b11000000 -> true
  | _ -> false
;;

let parse_special_format bytes =
  let open Result.Let_syntax in
  match bytes with
  (* Integers as String
     Read 1st byte, check if the first two bits are 11, then the remaining 6
     bits are read.

     If the value of those 6 bits is:
     - 0: indicates that an 8 bit integer follows
     - 1: indicates that a 16 bit integer follows
     - 2: indicates that a 32 bit integer follows *)
  | byte :: rest when Int.(byte land 0b11000000) = 0b11000000 ->
    (match Int.(byte land 0b111111), rest with
     | 0, b1 :: rest -> Ok (INTEGER b1, rest)
     | 1, b1 :: b2 :: rest -> Ok (INTEGER (little_endian_to_int [ b1; b2 ]), rest)
     | 2, b1 :: b2 :: b3 :: b4 :: rest ->
       Ok (INTEGER (little_endian_to_int [ b1; b2; b3; b4 ]), rest)
     | 3, _ ->
       let%bind compressed_len, bytes = parse_length bytes in
       let%bind uncompressed_len, bytes = parse_length bytes in
       let%bind str_bytes, rest =
         Result.of_option
           ~error:"insufficient bytes in compressed string"
           (list_split_n_opt bytes compressed_len)
       in
       Ok (COMPRESSED_STR (str_bytes, uncompressed_len), rest)
     | _ -> Error "invalid special format")
  | _ -> Error "not special format??"
;;

let parse_string_encoding bytes =
  let open Result.Let_syntax in
  if is_special_format bytes
  then parse_special_format bytes
  else parse_length_prefixed_string bytes >>| fun (str, bytes) -> LP_STR str, bytes
;;

let parse_header bytes =
  let open Result.Let_syntax in
  (* 52 45 44 49 53 30 30 31 31  // Magic string + version number (ASCII): "REDIS0011". *)
  let header_bytes, rest = List.split_n bytes 9 in
  if List.length header_bytes <> 9
  then Error "missing bytes in header"
  else (
    let magic_bytes, version_number_bytes = List.split_n header_bytes 5 in
    (* FIXME: Type system doesn't guarantee that this will always work *)
    let magic = ascii_hex_to_str magic_bytes in
    let version_number_str = ascii_hex_to_str version_number_bytes in
    let%bind version_num =
      Result.of_option
        ~error:"invalid version number"
        (Stdlib.int_of_string_opt version_number_str)
    in
    match magic with
    | "REDIS" -> Ok ({ rdb_version = version_num }, rest)
    | _ -> Error "invalid magic string")
;;

let byte_is_op_code = function
  | 0xFA | 0xFB | 0xFC | 0xFD | 0xFE | 0xFF -> true
  | _ -> false
;;

(* Also known as auxiliary fields in the spec *)
let parse_metadata bytes =
  let open Result.Let_syntax in
  let rec parse_kvs bytes map =
    match bytes with
    | 0xFA :: bytes ->
      (* TODO: we need to support other string formats too, i.e. integer strings
         and compressed LZF strings *)
      let%bind key, bytes = parse_length_prefixed_string bytes in
      let%bind value, bytes = parse_string_encoding bytes in
      parse_kvs bytes (StringMap.add key value map)
    | rest -> Ok (map, rest)
  in
  parse_kvs bytes StringMap.empty
;;

let ints_to_hex_str ints =
  List.map ~f:(fun i -> Printf.sprintf "%X" i) ints |> String.concat
;;

let parse_one bytes expected err =
  match bytes with
  | byte :: bytes when byte = expected -> Ok (byte, bytes)
  | _ -> Error err
;;

let parse_databases bytes =
  let open Result.Let_syntax in
  let rec parse_kvs bytes acc =
    match bytes with
    | 0xFC :: bytes ->
      let%bind timestamp_bytes, bytes =
        Result.of_option ~error:"missing bytes for timestamp" (list_split_n_opt bytes 8)
      in
      parse_kv (Some (MILLISECS (little_endian_to_int timestamp_bytes))) bytes acc
    | 0xFD :: bytes ->
      let%bind timestamp_bytes, bytes =
        Result.of_option ~error:"missing bytes for timestamp" (list_split_n_opt bytes 4)
      in
      parse_kv (Some (SECS (little_endian_to_int timestamp_bytes))) bytes acc
    | b :: _ when byte_is_op_code b ->
      (* We are probably in the EOF section *)
      return (List.rev acc, bytes)
    | bytes ->
      (* No expiry here, just move on *)
      parse_kv None bytes acc
  and parse_kv expire_timestamp bytes acc =
    match bytes with
    | 0 :: bytes ->
      (* dealing with a string *)
      let%bind key, bytes = parse_string_encoding bytes in
      let%bind value, bytes = parse_string_encoding bytes in
      parse_kvs bytes ({ key; value; expire_timestamp } :: acc)
    | t :: _ -> Fmt.failwith "can't handle type %d" t
    | _ -> Error "missing kv pair in database section"
  in
  let rec parse_database' bytes acc =
    match bytes with
    | 0xFE :: bytes ->
      let%bind db_index, bytes = parse_length bytes in
      let%bind _, bytes = parse_one bytes 0xFB "expected 0xFB in database section" in
      let%bind hash_tbl_sz, bytes = parse_length bytes in
      let%bind expire_hash_tbl_sz, bytes = parse_length bytes in
      let%bind kv_pairs, bytes = parse_kvs bytes [] in
      let database = { db_index; hash_tbl_sz; expire_hash_tbl_sz; kv_pairs } in
      parse_database' bytes (database :: acc)
    | _ -> Ok (acc, bytes)
  in
  parse_database' bytes []
;;

let parse_eof bytes =
  let open Result.Let_syntax in
  match bytes with
  | 0xFF :: bytes ->
    let%bind checksum_bytes, rest =
      Result.of_option ~error:"insufficient checksum bytes" (list_split_n_opt bytes 8)
    in
    Ok (ints_to_hex_str checksum_bytes, rest)
  | _ -> Error "malformed/missing end of file section"
;;

let parse_rdb bytes =
  let open Result.Let_syntax in
  let%bind header, bytes = parse_header bytes in
  let%bind metadata, bytes = parse_metadata bytes in
  let%bind databases, bytes = parse_databases bytes in
  let%bind checksum, bytes = parse_eof bytes in
  match bytes with
  | [] -> Ok { header; metadata; databases; checksum }
  | _ -> Error "unexpected  trailing bytes"
;;
