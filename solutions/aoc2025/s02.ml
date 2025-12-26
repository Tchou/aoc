open Utils
module S =
struct
  let name = Name.mk "s02"
(*
  Brute-force works well enough.
*)
  let read_input () =
    Input.read_line ()
    |> String.split_on_char ','
    |> List.map 
      (fun s -> 
         Scanf.sscanf s "%d-%d" (fun x y -> (x, y)))

  let equal_segments s i1 i2 len =
    let rec loop i = if i >= len then true
      else
        s.[i1+i] = s.[i2+i] && loop (i+1)
    in 
    loop 0

  let is_rep ns nl len_rep =
    if nl mod len_rep <> 0 || nl = len_rep then false
    else
      let num = nl/len_rep in
      let rec loop i =
        if i >= num then true
        else
          equal_segments ns 0 (i*len_rep) len_rep
          && loop (i+1)
      in
      loop 1

  let is_any_rep n =
    let ns = string_of_int n in
    let nl = String.length ns in
    try 
      for i = nl/2 downto 1 do
        if is_rep ns nl i then raise_notrace Exit;
      done;
      false
    with Exit -> true
  let is_two_rep n =
    let ns = string_of_int n in
    let nl = String.length ns in
    nl mod 2 = 0 && is_rep ns nl (nl/2)

  let repeats_in_range test (a, b) =
    let c = ref 0 in 
    for i = a to b do 
      if test i then c := !c + i;
    done;
    !c

  let count_reps test rl  =
    Iter2.(rl
           |> list
           |> map (repeats_in_range test)
           |> sum int)

  let solve test =
    let rl = read_input () in
    let n = count_reps test rl in
    Solution.printf "%d" n
  let solve_part1 () = solve is_two_rep
  let solve_part2 () = solve is_any_rep
end

let () = Solution.register_mod (module S)