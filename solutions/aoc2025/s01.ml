open Utils
module S =
struct
  let name = Name.mk "s01"

  let read_input () =
    Input.list_scan "%[RL]%d"
      (fun s n -> 
         (if s = "L" then -n else n))


(*
  For part 2, for negative numbers, if we are on 0, we must not count it twice.
*)

  let eval_dial l start part1 =
    l 
    |> List.fold_left (fun (accc, accv) n ->
        let accv' = ((accv + n) mod 100 + 100) mod 100 in
        let r = if part1 then int_of_bool (accv' = 0) else
          if n > 0 then (accv + n)/100 else 
            (accv + n - 100)/(-100) - int_of_bool (accv = 0)
        in
        (accc + r , accv'))
      (0, start)
    |> fst

  let solve part1 () =
    let l = read_input () in
    let n = eval_dial l 50 part1 in
    Solution.printf "%d" n
  let solve_part1 = solve true
  let solve_part2 = solve false
end

let () = Solution.register_mod (module S)