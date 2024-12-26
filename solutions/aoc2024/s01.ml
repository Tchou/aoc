open Utils
module S =
struct
  let name = Name.mk "s01"
  let read_input () =
    Input.fold_scan "%d %d" (fun (acc1, acc2) i1 i2 ->
        (i1::acc1, i2::acc2)
      ) ([], [])

  let mk_sol f =
    let l1, l2 = read_input () in
    let res = f l1 l2 in
    Solution.printf "%d" res

  let solve_part1 () =
    mk_sol (fun l1 l2 ->
        List.fold_left2 (fun acc i1 i2 ->
            acc + (abs (i1 - i2)))
          0
          (List.sort compare l1)
          (List.sort compare l2))

  let solve_part2 () =
    mk_sol (fun l1 l2 ->
        let open Syntax in
        let count = ~%[] in
        l2 |> List.iter (fun i2 ->
            count.%{i2}<- 1+ (count.%?{i2} or 0));
        l1 |> List.fold_left
          (Agg.Left.sum (fun i -> i * (count.%?{i} or 0))) 0
      )
end

let () = Solution.register_mod (module S)