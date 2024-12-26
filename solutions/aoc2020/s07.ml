open Utils
open Syntax

module S =
struct
  let name = Name.mk "s07"

  let load_input () =
    let tree = ~%[] in
    Input.fold_scan "%s %s bags contain%[ a-z0-9,]." (fun () key1 key2 s ->
        let key = key1 ^ " " ^ key2 in
        let l =
          match String.split_on_char ',' s with
            [" no other bags" ] -> []
          | l -> List.map (fun s -> match String.split_on_char ' ' s with
                [ ""; n; a; b; ("bag"|"bags")] -> (int_of_string n, a ^ " " ^ b)
              | _ -> assert false
            ) l
        in tree.%{key} <- l
      ) ();
    tree

  let rec has_shiny_gold cache tree k =
    match cache.%?{k} with
      Some b -> b
    | None ->
      let content = tree.%{k} in
      let res =
        List.exists
          (fun (_, k') -> k' = "shiny gold" || has_shiny_gold cache tree k')
          content
      in
      cache.%{k} <- res;
      res

  let count_shiny_gold tree =
    let cache = ~%[] in
    Hashtbl.fold (fun k _ acc ->
        acc + if has_shiny_gold cache tree k then 1 else 0
      ) tree 0

  let solve_part1 () =
    load_input ()
    |> count_shiny_gold
    |> Solution.printf "%d"


  let rec count_bags k tree =
    List.fold_left (Agg.Left.sum (fun (n, k')->
        n * count_bags k' tree
      )) 1 tree.%{k}
  let solve_part2 () =
    load_input ()
    |> count_bags "shiny gold"
    |> pred (* don't count the shiny bag itself *)
    |> Solution.printf "%d"

end

let () = Solution.register_mod (module S)