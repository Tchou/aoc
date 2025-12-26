open Utils
open Syntax
module S =
struct
  let name = Name.mk "s04"

  let read_input () = Input.list_fields ' ' Fun.id

  let count_valid tr l =
    Iter2.(
      l
      |> list
      |> count_if (fun l -> 
          let memo = ~%[] in
          List.for_all (fun s -> 
              let s = tr s in
              let b = memo %? s in
              memo.%{s} <- ();
              not b) l
        ))
  let solve tr =
    let l = read_input () in
    let n = count_valid tr l in
    Solution.printf "%d" n

  let solve_part1 () = solve Fun.id

  let sort_string s =
    String.explode s |> List.sort (Char.compare) |> String.implode
  let solve_part2 () = solve sort_string
end

let () = Solution.register_mod (module S)