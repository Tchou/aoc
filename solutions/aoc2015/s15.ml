open Utils
module S =
struct
  let name = Name.mk "s15"

  let read_input () =
    Input.list_scan 
      "%[^:]: capacity %d, durability %d, flavor %d, texture %d, calories %d"
      (fun n c d f t cal ->
         n, [|c;d;f;t;cal|]
      )

  let add_prop n prop total_prop =
    let total_prop = Array.copy total_prop in
    for i = 0 to 4 do
      total_prop.(i) <- total_prop.(i) + prop.(i) * n;
    done;
    total_prop

  let prod1 prop =
    let res = ref 1 in
    for i = 0 to 3 do
      res := !res * (if prop.(i) < 0 then 0 else prop.(i));
    done;
    !res
  let enumerate part2 ts ingredients =
    let max_score = ref 0 in
    let rec loop n ilist curr_total =
      if n >= List.length ilist then 
        match ilist with
          [ ] -> assert false
        | [ (_name, props)] -> 
          let total_prop = add_prop n props curr_total in
          if not part2 || total_prop.(4) = 500 then
            let total = prod1 total_prop in
            if total > !max_score then max_score := total
        | (_name, props) :: iilist ->
          let curr_total' = add_prop 1 props curr_total in
          loop (n-1) ilist curr_total';
          loop n iilist curr_total
    in
    loop ts ingredients [|0;0;0;0;0|];
    !max_score


  let solve part2 () =
    let ingredients = read_input () in
    let n = enumerate part2 100 ingredients in
    Solution.printf "%d" n
  let solve_part1 = solve false
  let solve_part2 = solve true
end

let () = Solution.register_mod (module S)