open Lwt
open Lwt.Syntax

let num_args_regex = Str.regexp {|\*\([0-9]+\)|}
let arg_len_regex = Str.regexp {|\$\([0-9]+\)|}

type 'a parse_result =
  | Disconnected
  | InvalidFormat of string
  | Parsed of 'a

let args_to_cmd args =
  let lower_fst = function
    | [] -> []
    | x :: xs -> String.lowercase_ascii x :: xs
  in
  match lower_fst args with
  | [ "ping" ] -> Some Cmd.PING
  | [ "echo"; arg ] -> Some (Cmd.ECHO arg)
  | [ "get"; key ] -> Some (Cmd.GET key)
  | [ "set"; key; value ] -> Some (Cmd.SET (key, value))
  | _ -> None
;;

(* Polls the input channel for a valid command, if this returns None this
   means that the connection has been dropped by the client. *)
let rec get_cmd ic =
  let parse_len regexp =
    let* msg = Lwt_io.read_line_opt ic in
    match msg with
    | None -> return Disconnected
    | Some msg ->
      if Str.string_match regexp msg 0
      then return @@ Parsed (int_of_string @@ Str.matched_group 1 msg)
      else return @@ InvalidFormat msg
  in
  let parse_args num_args =
    let rec parse_args' num_args acc =
      if num_args = 0
      then return @@ Parsed (List.rev acc)
      else
        let* arg_len = parse_len arg_len_regex in
        match arg_len with
        | Disconnected -> return Disconnected
        | InvalidFormat s ->
          let* _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
          return (InvalidFormat s)
        | Parsed arg_len ->
          let* msg = Lwt_io.read_line_opt ic in
          (match msg with
           | None -> return Disconnected
           | Some arg when String.length arg <> arg_len ->
             let* _ =
               Logs_lwt.err (fun m -> m "Argument (%s) length != %d" arg arg_len)
             in
             return @@ InvalidFormat arg
           | Some arg -> parse_args' (num_args - 1) (arg :: acc))
    in
    parse_args' num_args []
  in
  let* num_args = parse_len num_args_regex in
  match num_args with
  | Disconnected -> return None
  | InvalidFormat s ->
    let* _ = Logs_lwt.err (fun m -> m "Received malformed length %s" s) in
    get_cmd ic
  | Parsed num_args when num_args <= 0 -> get_cmd ic
  | Parsed num_args ->
    let* arg_list = parse_args num_args in
    (match arg_list with
     | Disconnected -> return None
     | InvalidFormat _ -> get_cmd ic
     | Parsed arg_list ->
       (match args_to_cmd arg_list with
        | None ->
          let* _ =
            Logs_lwt.err (fun m -> m "Unknown command %s" @@ String.concat "  " arg_list)
          in
          return None
        | Some cmd -> return @@ Some cmd))
;;