open Utils

module S = struct
  let name = Name.mk "s01"

  let replace_min a v =
    let rec loop i len =
      if i < len then if a.(i) < v then a.(i) <- v else loop (i + 1) len
    in
    Array.sort compare a;
    loop 0 (Array.length a)

  let solve len =
    let arr_max = Array.make len 0 in
    let _ =
      Input.fold_lines
        (fun current_sum -> function
          | "" ->
              replace_min arr_max current_sum;
              0
          | i -> current_sum + int_of_string i)
        0
    in
    let n = Array.fold_left ( + ) 0 arr_max in
    Solution.printf "%d" n

  let solve_part1 () = solve 1
  let solve_part2 () = solve 3
end


let () = Solution.register_mod (module S)

