open Utils
module S =
struct
  let name = Name.mk "s05"

  let read_input () =
    Input.list_scan "%d" Fun.id
    |> Array.of_list

  let execute part2 tab =
    let tab = Array.copy tab in
    let rec loop i c =
      if i < 0 ||  i >= Array.length tab then c
      else
        let i' = i + tab.(i) in
        let o = if part2 && tab.(i) >= 3 then -1 else 1 in
        tab.(i) <- tab.(i) + o;
        loop i' (c+1)
    in
    loop 0 0

  let solve part2 =
    let instrs = read_input () in
    let n = execute part2 instrs in
    Solution.printf "%d" n

  let solve_part1 () = solve false
  let solve_part2 () = solve true

end

let () = Solution.register_mod (module S)