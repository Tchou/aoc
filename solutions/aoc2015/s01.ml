open Utils
module S =
struct
  let name = Name.mk "s01"

  let read_input () =
    Input.read_line ()

  let count s = 
    Iter2.(string s
           |> fold (fun acc c -> if c = '('  then acc+1 else acc-1) 0)

  let find_basement_step s =
    let floor = ref 0 in
    Iter2.(istring s
           |> find (fun (i, c) -> 
               if c = '(' then incr floor else decr floor;
               !floor = -1
             ))
    |> fst |> succ
  let solve_part1 () =
    let s = read_input () in
    let n = count s in 
    Solution.printf "%d" n

  let solve_part2 () =
    let s = read_input () in
    let n = find_basement_step s in 
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)
