open Utils
module S =
struct
  let name = Name.mk "s01"

  let read_input () =
    Input.read_line ()

  let digit c = Char.code c - Char.code '0'

  let count s o =
    let total = ref 0 in
    let len = String.length s in
    String.iteri (fun i c -> 
        if c = s.[(i+o) mod len] then
          total := !total + digit c) s;
    !total
  let solve s o =
    let n = count s o in
    Solution.printf "%d" n

  let solve_part1 () =
    let s = read_input () in
    solve s 1

  let solve_part2 () =
    let s = read_input () in
    solve s (String.length s / 2)
end

let () = Solution.register_mod (module S)