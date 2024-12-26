open Utils

let solve compute print_result () =
  let _ =
    Input.fold_fields ' '
      (fun (current_cycle, current_value) -> function
         | [ "noop" ] ->
           compute current_cycle current_value;
           (current_cycle + 1, current_value)
         | [ "addx"; sv ] ->
           compute current_cycle current_value;
           compute (current_cycle + 1) current_value;
           let v = int_of_string sv in
           (current_cycle + 2, current_value + v)
         | _ -> assert false)
      (1, 1)
  in
  print_result ()
let compute1, get_result1 =
  let total = ref 0 in
  let target_cycle = ref 20 in
  let c1 cur_cycle cur_value =
    if cur_cycle = !target_cycle then begin
      total := !total + (!target_cycle * cur_value);
      target_cycle := 40 + !target_cycle
    end
  in
  let g1 () = Solution.printf "%d" !total in
  (c1, g1)

let compute2 cur_cycle cur_value =
  let line_pos = (cur_cycle - 1) mod 40 in
  let c =
    if cur_value - 1 <= line_pos && line_pos <= cur_value + 1 then "█" else " "
  in
  if line_pos = 0 then Solution.printf "%3d: " cur_cycle;
  Solution.printf "%s" c;
  if line_pos = 39 then Solution.printf "%-3d@\n" cur_cycle

module Sol = struct
  let name = Name.mk "s10"
  let solve_part1 = solve compute1 get_result1

  let solve_part2 =
    solve compute2 ignore
end

let () = Solution.register_mod (module Sol)
