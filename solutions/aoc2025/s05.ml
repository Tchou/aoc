open Utils
module S =
struct
  let name = Name.mk "s05"

  let read_input () =
    let intervals = InputUntil.list_lines 
        (function "" -> None
                | s -> Scanf.sscanf s "%d-%d" (fun a b -> Some (Interval.make_inc a b)))
    in
    let ids = Input.list_scan "%d" Fun.id in
    intervals|>Interval.Set.of_list, ids

  let count_ids intervals ids =
    Iter2.(ids |> list |> count_if (fun i -> Interval.Set.mem i intervals))

  let count_intervals (int_list : Interval.Set.t) _ =
    Iter2.((int_list :> Interval.t list)
           |> list
           |> map Interval.length
           |> sum int)
  let solve count () =
    let ints, ids = read_input () in
    let n = count ints ids in
    Solution.printf "%d" n
  let solve_part1 = solve count_ids
  let solve_part2 = solve count_intervals
end

let () = Solution.register_mod (module S)