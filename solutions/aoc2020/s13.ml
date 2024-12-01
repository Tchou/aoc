open Utils


module S =
struct
  let name = Name.mk "s13"

  let input1() =
    let timestamp = read_line () |> int_of_string in
    let bus_ids =
      read_line ()
      |> String.split_on_char ','
      |> List.filter_map int_of_string_opt
    in
    timestamp, bus_ids
  let solve_part1 () =
    let timestamp, bus_ids = input1 () in
    let next_ts bus_id =
      let r = timestamp mod bus_id in
      if r = 0 then 0
      else bus_id - r
    in
    let bid, d =
      List.fold_left (fun ((_, adif) as acc) bid ->
          let diff = next_ts bid in
          if diff < adif then (bid, diff) else acc
        ) (-1, max_int) bus_ids
    in
    Ansi.(printf "%a%d%a\n" fg green (bid * d) clear color)

  module ZMath = MathGen(Z)

  let solve_part2 () =
    let _ = read_line () in
    let _, l =
      read_line ()
      |> String.split_on_char ','
      |> List.fold_left (fun (acc_i, acc_l) s ->
          if s = "x" then Z.succ acc_i , acc_l
          else
            let open Z.Compare in
            let n = Z.of_string s in
            (Z.succ acc_i, ((if acc_i = Z.zero then Z.zero else Z.(n - (erem acc_i  n))), n)::acc_l)
        ) (Z.zero, [])

    in
    let t, _ = ZMath.solve_crt l in
    Ansi.(printf "%a%a%a\n" fg green Z.pp_print t clear color)


end

let () = Solution.register_mod (module S)