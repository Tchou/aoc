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
    let n = List.fold_left (Agg.Left.sum calc) 0 modules in
    Ansi.(printf "%a%d%a\n" fg green n clear color)


  let solve_part1 () = solve fuel

  let solve_part2 () = solve rec_fuel
end

let () = Solution.register_mod (module S)
