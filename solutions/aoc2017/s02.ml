open Utils
module S =
struct
  let name = Name.mk "s02"
  let read_input () =
    Input.list_lines (fun l ->
        String.split_on_char '\t' l
        |> List.filter_map 
          (function "" -> None | s -> Some (int_of_string s))
      ) 

  let sum_diff_in_max l =
    List.fold_left (fun acc s ->
        acc + Iter.((max list s) - (min list s)))
      0 l

  let sum_divide l =
    List.fold_left (fun acc s ->
        acc +
        (Iter.(pairs ~refl:false ~sym:false list s)
         |> Seq.find_map (fun (a, b) ->
             let a = max a b
             and b = min a b in
             if a mod b = 0 then
               Some (a/b) else None)
         |> Option.get)) 0 l
  let solve f =
    let l = read_input () in
    let n = f l in
    Solution.printf "%d" n
  let solve_part1 () = solve sum_diff_in_max

  let solve_part2 () = solve sum_divide
end

let () = Solution.register_mod (module S)