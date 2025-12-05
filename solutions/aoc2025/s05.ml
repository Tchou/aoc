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
    intervals, ids

  let mem (i : int) ilist =
    List.exists (Interval.mem i) ilist

  let count_ids intervals ids =
    Iter.(count_if list (fun i -> mem i intervals)) ids

  let count_intervals ilist _ =
    Interval.merge [] ilist
    |> List.map (Interval.length)
    |> Iter.(sum list (module Int))

  let solve count () =
    let ints, ids = read_input () in
    let n = count ints ids in
    Solution.printf "%d" n
  let solve_part1 = solve count_ids

  let solve_part2 = solve count_intervals
end

let () = Solution.register_mod (module S)