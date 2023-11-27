let table = Hashtbl.create 16
let prefixes = Hashtbl.create 16
let register (n : string) (solve : unit -> unit) = Hashtbl.replace table n solve
let get (n : string) = Hashtbl.find_opt table n
let list () = table |> Hashtbl.to_seq_keys |> List.of_seq |> List.sort compare

module type S = sig
  val name : string*string
  val solve_part1 : unit -> unit
  val solve_part2 : unit -> unit
end

let register_prefix s = Hashtbl.replace prefixes s ()
let is_prefix s = Hashtbl.mem prefixes s

let register_mod ?variant (module X : S) =
  let variant = Option.to_list variant in
  let prefix, name = X.name in
  register (String.concat " " ([prefix; name; "1" ] @ variant)) X.solve_part1;
  register (String.concat " " ([prefix; name; "2" ] @ variant)) X.solve_part2

let default_prefix = "aoc2023"

let exec s =
  match get s with
    Some f -> f ()
  | None -> failwith ("Invalid argument :" ^ s)

let run argv =
  match List.tl (Array.to_list argv) with
    [ "list" ] -> list () |> List.iter print_endline
  | [ sol ] -> exec String.(concat " " [default_prefix; sol; "1"])
  | [ sol; ("1"|"2" as v) ] -> exec String.(concat " " [default_prefix; sol; v])
  | [ sol; ("1"|"2" as v); var ] -> exec String.(concat " " [default_prefix; sol; v; var])
  | [ prefix; sol ] -> exec String.(concat " " [prefix; sol; "1"])
  | [ prefix; sol; ("1"|"2" as v) ] -> exec String.(concat " " [prefix; sol; v])
  | [ prefix; sol; ("1"|"2" as v); var ] -> exec String.(concat " " [prefix; sol; v; var])
  | l -> failwith ("Invalid argument :" ^ String.(concat " " l))
