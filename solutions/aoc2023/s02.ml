open Utils
open Syntax

module S =
struct
  let name = Name.mk "s02"


  let valid_game acc id sets =
    if List.for_all (fun set ->
        List.for_all
          (fun (c, n) ->
             (c = "red" && n <= 12)
             || (c = "green" && n <= 13)
             || (c = "blue" && n <= 14))  set)
        sets
    then acc + id else acc

  let cube_power acc _id sets =
    let table = ~%[] in
    sets
    |> List.iter (
      List.iter (fun (c, n) ->
          table.%{c} <- max n (table.%?{c} or n)));
    acc + Hashtbl.fold (fun _ n acc -> acc*n) table 1

  let read_file f =
    Input.fold_scan "Game %d:%s@\n" (fun acc id text ->
        let sets =
          String.split_on_char ';' text
          |> List.map (fun s ->
              String.split_on_char ',' s
              |> List.map (fun s -> Scanf.sscanf s " %d %s" (fun n color -> color,n)))
        in
        f acc id sets
      ) 0

  let solve_part1 () =
    let n = read_file valid_game in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part2 () =
    let n = read_file cube_power in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

end

let () = Solution.register_mod (module S)