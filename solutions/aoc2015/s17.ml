open Utils
open Syntax
module S =
struct
  let name = Name.mk "s17"

  let read_input () =
    Input.list_scan "%d" Fun.id

  let enumerate capacity containers =
    let count = ~%[] in 
    let rec loop cap cl used =
      if cap < 0 then 0 
      else if cap = 0 then (count.%{used} <- 1 + (count.%?{used} or 0); 1) else
        match cl with
          [] -> 0
        | cont :: ccl ->
          (loop (cap - cont) ccl (1+used)) +
          (loop cap ccl used)
    in 
    let num_comb = loop capacity containers 0 in
    let count_min = ref 0 in
    let used_min = ref max_int in
    count |> Hashtbl.iter (fun k v ->
        if k < !used_min then (used_min := k; count_min := v));
    num_comb, !count_min


  let solve get () = 
    let containers = read_input () in
    let r = enumerate 150 containers in
    Solution.printf "%d" (get r)
  let solve_part1 = solve fst
  let solve_part2 = solve snd 

end

let () = Solution.register_mod (module S)