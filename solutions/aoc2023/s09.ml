open Utils
module S =
struct
  let name = Name.mk "s09"
  let complete numbers =
    let arr = Array.of_list numbers in
    let rec down n =
      let stop = ref true in
      let prev = ref (arr.(1) - arr.(0)) in
      arr.(0) <- !prev;
      for i = 1 to n do
        let a =  arr.(i+1) - arr.(i) in
        if a != !prev then stop := false;
        prev := a;
        arr.(i) <- a;
      done;
      if !stop then n else down (n-1)
    in
    let last = Array.length arr - 1 in
    let n = down (last - 1) in
    let res = ref 0 in
    for i = n to last do
      res := !res + arr.(i);
    done;
    !res

  let solve dir =
    Input.fold_fields ' '
      (fun acc fields ->
         Seq.cons (fields
                   |> List.map int_of_string
                   |> dir
                   |> complete) acc
      ) Seq.empty
    |> Iter.seq
    |> Iter.(sum int)
    |> Solution.printf "%d"
  let solve_part1 () = solve Fun.id

  let solve_part2 () = solve List.rev
end

let () = Solution.register_mod (module S)