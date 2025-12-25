open Utils
module S =
struct
  let name = Name.mk "s25"
  let iterate_until target_row target_col =
    let rec loop_diag row col v  =
      if row = target_row && col = target_col then
        `Found v
      else if row = 0 then `Continue v
      else
        loop_diag (row-1) (col+1) ((v * 252533) mod 33554393)
    in
    let rec loop_until start_row v = 
      match loop_diag start_row 1 v with
        `Found v -> v
      | `Continue v -> loop_until (start_row+1) v
    in
    loop_until 1 20151125

  let read_input () =
    let l = Input.read_line () in
    Scanf.sscanf l "%[a-zA-Z ,.]%d%[a-zA-Z ,.]%d."
      (fun _ a _ b -> a, b)
  let solve_part1 () =
    let row, col = read_input () in
    let n = iterate_until row col in
    Solution.printf "%d" n
  let solve_part2 () = ()
end






let () = Solution.register_mod (module S)