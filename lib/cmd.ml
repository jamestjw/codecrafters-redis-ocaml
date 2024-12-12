type set_timeout =
  | PX of int (* milliseconds until expiry *)
  | EX of int (* seconds until expiry *)
[@@deriving show { with_path = false }]

type t =
  | PING
  | ECHO of string
  | GET of string
  | SET of
      { set_key : string
      ; set_value : string
      ; set_timeout : set_timeout option
      }
  | GET_CONFIG of string list
  | KEYS of string
  | INFO of string list
  | REPL_CONF_PORT of int
  | REPL_CONF_CAPA of string (* replication capabilities *)
  | PSYNC of string * int
  | INVALID of string
[@@deriving show { with_path = false }]
