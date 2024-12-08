open Core

let read_all_bytes filename =
  let ic = In_channel.create ~binary:true filename in
  let rec read_all acc =
    match In_channel.input_byte ic with
    | None -> List.rev acc
    | Some b -> read_all (b :: acc)
  in
  read_all []
;;
