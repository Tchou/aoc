open Utils
module S =
struct
  let name = Name.mk "s01"

  let read_input () =
    Input.read_line ()

  let count = String.fold_left (fun acc c -> if c = '(' then acc+1 else acc-1) 0

  let find_basement_step s =
    let floor = ref 0 in
    let exception Found of int in
    try
      s
      |> String.iteri (fun i c -> if c = '(' then incr floor else decr floor;
                        if !floor = -1 then raise_notrace (Found (i+1)));
      0
    with Found n -> n

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
