open Utils

module S =
struct
  let name = Name.mk "s06"

  let rec simulate n pool new_pool =
    if n = 0 then pool
    else begin
      for i = 1 to 8 do
        new_pool.(i-1) <- pool.(i);
      done;
      new_pool.(8) <- pool.(0);
      new_pool.(6) <- new_pool.(6) + pool.(0);
      simulate (n-1) new_pool pool;
    end
  let solve n =
    let pool = Array.make 9 0 in
    let new_pool = Array.make 9 0 in
    Input.read_line ()
    |> String.split_on_char ','
    |> List.iter (fun s ->
        let i = int_of_string s in
        pool.(i) <- pool.(i) + 1);
    simulate n pool new_pool
    |> Array.fold_left (+) 0
    |> Solution.printf "%d"

  let solve_part1 () = solve 80

  let solve_part2 () = solve 256
end

let () = Solution.register_mod (module S)