let input = ref stdin
let set_input ic = input := ic

let get_input () = !input

exception Error of string
let table = Hashtbl.create 16
let register (n : string list) (solve : unit -> unit) = Hashtbl.replace table n solve
let get (n : string list) = Hashtbl.find_opt table n
let list () = table |> Hashtbl.to_seq_keys |> List.of_seq |> List.sort compare

module type S = sig
  val name : string * string
  val solve_part1 : unit -> unit
  val solve_part2 : unit -> unit
end

let register_mod ?variant (module X : S) =
  let variant = Option.to_list variant in
  let prefix, name = X.name in
  register ([prefix; name; "1" ] @ variant) X.solve_part1;
  register ([prefix; name; "2" ] @ variant) X.solve_part2

let default_prefix =
  let open Unix in
  let tm = gmtime (gettimeofday ()) in
  "aoc" ^ (string_of_int (1900 + tm.tm_year))

let color_time t =
  let open Ansi in
  if t < 10. then green else if t < 100. then cyan else
  if t < 1000. then yellow else red


let buffer = Buffer.create 16
let bfmt = Format.formatter_of_buffer buffer
let printf x = Format.fprintf bfmt x
let exec l =
  match get l with
    Some f ->
    let s = String.(concat " " l) in
    let prompt = Format.sprintf "%s, answer: " s in
    let blank = String.make (String.length prompt) ' ' in
    let t0 = Unix.gettimeofday () in
    let () = Format.pp_print_flush bfmt () in
    let () = Buffer.clear buffer in
    let () = f () in
    let t1 = Unix.gettimeofday () in
    let t = 1000. *. (t1 -. t0) in
    let col = color_time t in
    Ansi.(printf "%a%s%a" bfg yellow prompt clear color);
    let () = Format.pp_print_flush bfmt () in
    let () = match Buffer.contents buffer |> String.split_on_char '\n' with
        [] -> Ansi.printf "\n"
      | p :: rest ->
        Ansi.(printf "%a%s%a\n%!" fg green p clear color);
        rest |> List.iter (fun s -> Ansi.(printf "%s%a%s%a\n%!" blank fg green s clear color))
    in
    Buffer.clear buffer;
    Ansi.(printf "%a%s, time:   %a%fms%a\n%!" bfg yellow s bfg col t clear color)

  | None -> raise (Error String.(concat " " ("Invalid solution:" :: l)))
let exec_dir prefix files =
  let solutions = Hashtbl.fold (fun l f acc ->
      match l with
        p::s::_::[] when p = prefix -> [p;s]::acc
      |_ -> acc
    ) table [] |> List.sort_uniq (fun a b -> compare a b)
  in
  let ls = List.length solutions in
  let ld = List.length files in
  if ls <> ld then
    raise (Error Format.(sprintf "prefix %s has %d solutions but %d input files were given." prefix ls ld))
  else
    let t0 = Unix.gettimeofday () in
    List.iter2 (fun sol file ->
        try
          for i = 1 to 2 do
            let ic = open_in file in
            set_input ic;
            Fun.protect
              ~finally:(fun () -> try close_in ic;set_input stdin with _ -> ())
              (fun () -> exec (sol @ [string_of_int i]))
          done;
          Ansi.printf "\n";
        with Sys_error msg -> raise (Error msg)
      ) solutions files;
    let t1 = Unix.gettimeofday () in
    let t = 1000. *. (t1 -. t0) in
    let col = if t < 1000. then Ansi.green else if t < 5000. then Ansi.cyan else Ansi.red in
    Ansi.(printf "\n%a%s, total time:   %a%fms%a\n%!" fg yellow  prefix bfg col t clear color)


let run argv =
  match List.tl (Array.to_list argv) with
    [ "list" ] -> list () |> List.iter (fun l -> String.concat " " l |> print_endline)
  |  "bench" :: prefix :: dirs -> exec_dir prefix dirs
  | [ sol ] -> exec [default_prefix; sol; "1"]
  | [ sol; ("1"|"2" as v) ] -> exec [default_prefix; sol; v]
  | [ sol; ("1"|"2" as v); var ] -> exec [default_prefix; sol; v; var]
  | [ prefix; sol ] -> exec [prefix; sol; "1"]
  | [ prefix; sol; ("1"|"2" as v) ] -> exec [prefix; sol; v]
  | [ prefix; sol; ("1"|"2" as v); var ] -> exec [prefix; sol; v; var]
  | l -> raise (Error String.(concat " " ("Invalid argument:" :: l)))


