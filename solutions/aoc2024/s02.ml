open Utils
module S =
struct
  let name = Name.mk "s02"

  let read_input () =
    Input.fold_fields ' ' (fun acc line ->
        (List.map int_of_string line)::acc) []
    |> List.rev

  let same_sign s1 s2 =
    (s1 < 0 && s2 < 0) || (s1 > 0 && s2 > 0)

  let rec well_ordered s l =
    match l with
      [] | [ _ ] -> true
    | i1::(i2 :: _ as ll) ->
      let d = i1 - i2 in
      let ad = abs d in
      1 <= ad && ad <= 3 && same_sign s d && well_ordered s ll

  let is_safe l = match l with
      [] | [ _ ] -> true
    | i1 :: i2 :: _ ->
      i1 <> i2 && well_ordered (i1 - i2) l

  let is_mostly_safe l =
    let rec loop l acc =
      match l with
        [] -> false
      | e:: ll ->
        if is_safe (List.rev_append acc ll) then true
        else loop ll (e::acc)
    in
    is_safe l || loop l []

  let solve f =
    let l = read_input () in
    let n = List.fold_left (Agg.Left.sum (fun l -> int_of_bool (f l) )) 0 l
    in
    Solution.printf "%d" n

  let solve_part1 () = solve is_safe
  let solve_part2 () = solve is_mostly_safe

end

let () = Solution.register_mod (module S)