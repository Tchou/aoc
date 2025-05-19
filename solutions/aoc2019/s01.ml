open Utils
module S =
struct
  let name = Name.mk "s01"

  let read_input () =
    Input.fold_lines (fun acc s -> (int_of_string s)::acc) []
    |> List.rev

  let fuel m = m / 3 - 2

  let rec_fuel m =
    let rec loop m acc =
      let m' = fuel m in
      if m' > 0 then loop m' (acc+m')
      else acc
    in
    loop m 0

  let solve calc =
    let modules = read_input () in
    let n =
      modules
      |> List.to_seq
      |> Seq.map calc
      |> Iter.(sum seq int) in
    Solution.printf "%d" n

  let solve_part1 () = solve fuel

  let solve_part2 () = solve rec_fuel
end

let () = Solution.register_mod (module S)
