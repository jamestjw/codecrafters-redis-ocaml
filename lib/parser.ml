open Core
open Lwt

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

let args_to_cmd args =
  match lower_fst args with
  | "ping" :: args -> parse_ping_cmd args
  | "echo" :: args -> parse_echo_cmd args
  | "get" :: args -> parse_get_cmd args
  | "set" :: args -> parse_set_cmd args
  | "config" :: args -> parse_config_cmd args
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

let hex_str_to_int_opt s = Stdlib.int_of_string_opt (Printf.sprintf "0x%s" s)

let ascii_hex_to_str l =
  let opts = List.map ~f:hex_str_to_int_opt l in
  if List.exists ~f:Option.is_none opts
  then None
  else (
    let str =
      List.filter_opt opts
      |> List.map ~f:(fun e -> Stdlib.Option.get (Char.of_int e))
      |> String.of_list
    in
    Some str)
;;

(* RDB file *)

type header = { rdb_version : int }

let parse_length_prefixed_string data =
  match data with
  | [] -> Error "malformed length prefixed string"
  | len_str :: rest ->
    (match hex_str_to_int_opt len_str with
     | None -> Error "invalid byte"
     | Some len when List.length rest <> len -> Error "bytes do not match prefix length"
     | Some _ -> Result.of_option ~error:"invalid byte in string" (ascii_hex_to_str rest))
;;

let parse_header lines =
  let open Result.Let_syntax in
  match lines with
  | [] -> Error "missing header"
  | line :: rest ->
    let%bind magic =
      Result.of_option ~error:"malformed magic" (List.slice line 0 5 |> ascii_hex_to_str)
    in
    let%bind version_num_str =
      Result.of_option
        ~error:"malformed version number"
        (List.slice line 5 9 |> ascii_hex_to_str)
    in
    let%bind version_num =
      Result.of_option
        ~error:"invalid version number"
        (Stdlib.int_of_string_opt version_num_str)
    in
    (match magic with
     | "REDIS" -> Ok ({ rdb_version = version_num }, rest)
     | _ -> Error "invalid magic string")
;;

let parse_rdb str = failwith "todo"
