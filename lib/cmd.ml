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
  | INVALID of string
[@@deriving show { with_path = false }]
