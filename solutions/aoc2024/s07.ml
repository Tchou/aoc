open Utils
module S =
struct
  let name = Name.mk "s07"

  let read_input () =
    Input.list_scan "%d: %[0-9 ]" (fun n s ->
        (n, s |> String.split_on_char ' ' |> List.map int_of_string)
      )

  let pow10 n acc = (* faster than log + float/int conversions for small numbers*)
    let rec loop n acc =
      if n = 0 then acc else
        loop ((n * 0x1999999A) lsr 32) ((acc lsl 3) + (acc lsl 1))
    in
    loop n acc

  let concat n1 n2 = (pow10 n2 n1) + n2

  let can_be_combined part2 total l =
    let rec loop l acc =
      if acc > total then false else
        match l with
          [] -> acc == total
        | v :: ll ->
          loop ll (acc+v) ||
          loop ll (acc*v) ||
          (part2 && loop ll (concat acc v))
    in
    match l with
      [] -> assert false
    | v ::  ll -> loop ll v

  let total_calibration_result part2 l =
    let open Iter2 in 
    l
    |> list
    |> map (fun (total, l) ->
        if can_be_combined part2 total l then total else 0)
    |> sum int
  let solve ops =
    let lst = read_input () in
    let n = total_calibration_result ops lst in
    Solution.printf "%d" n

  let solve_part1 () = solve false

  let solve_part2 () = solve true

end

let () = Solution.register_mod (module S)