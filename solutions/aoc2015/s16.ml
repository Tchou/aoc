open Utils
module S =
struct
  let name = Name.mk "s16"

  let read_props s =
    s 
    |> String.split_on_char ','
    |> List.map (fun s ->
        match String.split_on_char ':' s with 
          [ n; v] -> String.(trim n, trim v |> int_of_string)
        | _ -> assert false)
  let read_input () =
    Input.list_scan "Sue %d:%[^\n]"
      (fun n p -> n, read_props p)

  let constraints1 =  [
    "children", ((=)3);
    "cats", ((=)7);
    "samoyeds", ((=)2);
    "pomeranians", ((=)3);
    "akitas", ((=)0);
    "vizslas", ((=)0);
    "goldfish", ((=)5);
    "trees", ((=)3);
    "cars", ((=)2);
    "perfumes", ((=)1) 
  ]
  let constraints2 =  [
    "children", ((=)3);
    "cats", ((<)7);
    "samoyeds", ((=)2);
    "pomeranians", ((>)3);
    "akitas", ((=)0);
    "vizslas", ((=)0);
    "goldfish", ((>)5);
    "trees", ((<)3);
    "cars", ((=)2);
    "perfumes", ((=)1);
  ]

  let compatible constraints (_, props) =
    props 
    |> List.for_all (fun (pn, pv) ->
        not (List.exists (fun (cn, f) -> cn = pn && not (f pv)) constraints)
      )
  let find_sue constraints sue_list =
    match List.filter (compatible constraints) sue_list with
      [ (n, _) ] -> n
    | l -> fail "%d sues remaining" (List.length l)

  let solve constraints () = 
    let sue_list = read_input () in
    let n = find_sue constraints sue_list in
    Solution.printf "%d" n
  let solve_part1 = solve constraints1
  let solve_part2 = solve constraints2

end

let () = Solution.register_mod (module S)