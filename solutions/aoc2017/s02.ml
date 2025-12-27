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
    Iter.(
      l 
      |> list
      |> map (fun s -> (s |> list |> max_) - (s |> list |> min_))
      |> sum int
    )

  let pr_id i = Format.printf "%d\n%!" i; i
  let sum_divide l =
    Iter.(l
           |> list
           |> map (fun s ->
               s |> list |> pairs ~refl:false ~sym:false 
               |> find_map (fun (a, b) -> 
                   let a = max a b 
                   and b = min a b in
                   if a mod b = 0 then Some (a/b)
                   else None)
               |> Option.get
             )
           |> sum int
          )
  let solve f =
    let l = read_input () in
    let n = f l in
    Solution.printf "%d" n
  let solve_part1 () = solve sum_diff_in_max

  let solve_part2 () = solve sum_divide
end

let () = Solution.register_mod (module S)