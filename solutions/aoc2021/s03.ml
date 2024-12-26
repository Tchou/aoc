open Utils

module S =
struct
  let name = Name.mk "s03"
  let solve_part1 () =
    let s = Input.read_line () in
    let acc = Array.make (String.length s) 0 in
    String.iteri (fun i c -> acc.(i) <- if c = '0' then -1 else 1) s;
    Input.fold_lines (fun () s ->
        for i = 0 to String.length s - 1 do
          let k = if s.[i] = '0' then -1 else 1 in
          acc.(i) <- acc.(i) + k;
        done
      ) ();
    let gamma = ref 0 in
    let epsilon = ref 0 in
    let pow = ref 1 in
    for i = Array.length acc - 1 downto 0 do
      let k = if acc.(i) > 0 then 1 else 0 in
      gamma := !gamma + !pow * k;
      epsilon := !epsilon + !pow * (1-k);
      pow := !pow * 2;
    done;
    Solution.printf "%d" (!gamma * !epsilon)


  let filter_by_bits i cmp def lst =
    match lst with
      [] | [ _ ] -> lst
    | _ ->
      let zero_count, one_count =
        List.fold_left (fun (zc, oc) s ->
            if s.[i] = '0' then (zc+1, oc)
            else (zc, oc+1)) (0,0) lst
      in
      let b =
        if zero_count = one_count then def else
        if cmp zero_count one_count then '0' else '1'
      in
      List.filter (fun s -> s.[i] = b) lst


  let solve_part2 () =
    let l = Input.fold_lines (fun acc s -> s ::acc) [] in
    let n = String.length (List.hd l) in
    let oxygen = ref l in
    for i = 0 to n - 1 do
      oxygen := filter_by_bits i (>) '1' !oxygen
    done;
    let co2 = ref l in
    for i = 0 to n - 1 do
      co2 := filter_by_bits i (<) '0' !co2;
    done;
    match !oxygen, !co2 with
      [s1], [s2] ->
      let res =
        (int_of_string ("0b" ^ s1)) * (int_of_string ("0b" ^ s2))
      in
      Solution.printf "%d" res
    | _ -> assert false
end

let () = Solution.register_mod (module S)