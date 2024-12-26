open Utils
module S =
struct
  let name = Name.mk "s07"

  let read_input () =
    Input.fold_scan "%d: %[0-9 ]" (fun acc n s ->
        (n, s |> String.split_on_char ' ' |> List.map int_of_string)::acc
      ) []

  let pow10 n = (* faster than log + float/int conversions *)
    let rec loop n acc =
      if n = 0 then acc else
        loop (n/10) (acc*10)
    in
    loop n 1

  let concat n1 n2 = (n1 * (pow10 n2)) + n2

  let can_be_combined part2 total l =
    let rec loop l =
      match l with
        [] -> false
      | [ v ] -> v == total
      | v1 :: v2 :: ll ->
        loop ((v1+v2)::ll) ||
        loop ((v1*v2)::ll) ||
        (part2 && loop (concat v1 v2 :: ll))
    in
    loop l

  let total_calibration_result part2 =
    List.fold_left (Agg.Left.sum (fun (total, l) ->
        if can_be_combined part2 total l then total else 0)) 0
  let solve ops =
    let lst = read_input () in
    let n = total_calibration_result ops lst in
    Solution.printf "%d" n

  let solve_part1 () = solve false

  let solve_part2 () = solve true

end

let () = Solution.register_mod (module S)