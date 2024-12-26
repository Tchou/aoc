open Utils
open Syntax
module S =
struct
  let name = Name.mk "s21"

  let read_input () =
    Input.fold_scan "%[^(](contains %[^)])"
      (fun acc ing_s all_s ->
         let ingredients =
           ing_s
           |> String.trim
           |> String.split_on_char ' '
         in
         let allergens =
           all_s
           |> String.trim
           |> String.split_on_char ','
           |> List.map String.trim
         in
         (ingredients, allergens)::acc
      ) []
  module StringSet = Set.Make(String)

  let count_non_allergen food_list =
    let allergen_map = ~%[] in
    let () =
      food_list
      |> List.iter (fun (ingredients, allergens) ->
          let ing_set = StringSet.of_list ingredients in
          allergens
          |> List.iter (fun alg ->
              try
                let old_i = allergen_map.%{alg} in
                allergen_map.%{alg}<- StringSet.inter old_i ing_set
              with
                Not_found -> allergen_map.%{alg} <- ing_set
            )
        )
    in
    let all_allergens =
      Hashtbl.fold (fun _ set acc -> StringSet.union acc set)
        allergen_map StringSet.empty
    in
    allergen_map
    |> Hashtbl.to_seq |> List.of_seq,
    food_list
    |> List.fold_left (fun acc (ingredients, al) ->
        ingredients
        |> List.fold_left (fun acc i ->
            if StringSet.mem i all_allergens then acc
            else acc + 1) acc
      ) 0


  let find_matching f map =
    let used = ~%[] in
    let rec loop acc = function
        [] -> f acc
      | (allergen, ingredients)::rest ->
        StringSet.iter (fun ingredient ->
            if not (used %? ingredient) then begin
              used.%{ingredient} <- ();
              loop ((allergen, ingredient)::acc) rest;
              used %- ingredient;
            end
          ) ingredients
    in
    loop [] map

  let solve_part1 () =
    let food_list = read_input () in
    let _, n = count_non_allergen food_list in
    Solution.printf "%d" n

  let solve_part2 () =
    let food_list = read_input () in
    let map, _ = count_non_allergen food_list in
    let exception Found of (string * string) list in
    let s =
      try
        find_matching (fun l -> raise (Found l)) map;
        "<FAILURE>"
      with
        Found l ->
        l
        |> List.sort (Compare.fst)
        |> List.map snd
        |> String.concat ","
    in
    Solution.printf "%s" s

end

let () = Solution.register_mod (module S)