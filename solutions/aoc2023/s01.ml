open Utils
open Syntax

let predicate_from pred s step from to_ =
  let rec loop i =
    if i == to_ then raise Not_found else
      match pred s i with
        None -> loop (i+step)
      | Some v -> v
  in
  loop from

let is_digit_opt s i =
  match s.[i] with
  | '0'..'9' as c -> Some (Char.code c - Char.code '0')
  | _ -> None

let digit_from = predicate_from is_digit_opt

let digits = [|("one", 1); ("two", 2); ("three",3);
               ("four", 4); ("five", 5); ("six", 6);
               ("seven", 7); ("eight", 8); ("nine",9) |]

let words_or_digits_from  =
  predicate_from (fun s i ->
      let*| () = is_digit_opt s i in
      digits
      |> Array.find_opt (fun (w, _) -> 0 = String.compare_from s i w)
      |> Option.map snd)

let solve debug read_digit  =
  let res =
    Input.fold_lines (fun acc line ->
        let len = String.length line in
        let d1 = read_digit line 1 0 len in
        let d2 = read_digit line (-1) (len-1) (-1) in
        if debug then Printf.printf "%s %d%d\n" line d1 d2;
        d1 * 10 + d2 + acc) 0
  in
  Ansi.(printf "%a%d%a\n%!" bfg green res clear color)

let mk_sol ?variant debug =
  let module S = struct
    let name = Name.mk "s01"

    let solve_part1 () = solve debug digit_from
    let solve_part2 () = solve debug words_or_digits_from

  end in Solution.register_mod ?variant (module S)

let () =
  mk_sol false;
  mk_sol ~variant:"debug" true