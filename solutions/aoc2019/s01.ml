open Utils
module S =
struct
  let name = Name.mk "s01"

  let read_input () =
    Input.list_scan "%d" Fun.id

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
      Iter2.(modules
             |> list
             |> map calc
             |> sum int) 
    in
    Solution.printf "%d" n

  let solve_part1 () = solve fuel

  let solve_part2 () = solve rec_fuel
end

let () = Solution.register_mod (module S)
