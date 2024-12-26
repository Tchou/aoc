open Utils

module S =
struct
  let name = Name.mk "s01"

  let solve_part1 () =
    Input.fold_scan "%d" (fun (count, prev) d ->
        (if d > prev then count+1 else count), d) (0, max_int)
    |> fst
    |> Solution.printf "%d"

  let solve_part2 () =
    let a0 = Input.read_line () |> int_of_string in
    let a1 = Input.read_line () |> int_of_string in
    let a2 = Input.read_line () |> int_of_string in
    let window = [|a0; a1; a2|] in
    let sum = a0 + a1 + a2 in
    Input.fold_scan "%d" (fun (count, sum) d ->
        let nsum = sum - window.(0) + d in
        let count = if nsum > sum then count + 1 else count in
        window.(0) <- window.(1);
        window.(1) <- window.(2);
        window.(2) <- d;
        (count, nsum)) (0, sum)
    |> fst
    |> Solution.printf "%d"

end

let () = Solution.register_mod (module S)