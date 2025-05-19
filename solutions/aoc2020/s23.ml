open Utils

module S =
struct
  let name = Name.mk "s23"

  let read_input () =
    let s = Input.read_line () in
    let t = Dll.singleton (Char.code s.[0] - Char.code '0') in
    for i = 1 to String.length s - 1 do
      ignore @@ Dll.insert_before t (Char.code s.[i] - Char.code '0')
    done;
    t

  let play max_val rounds t =
    let dummy = Dll.singleton (-1) in
    let map = Array.make (max_val + 1) dummy in
    let remove_from_map c = map.(c) <- dummy in
    let is_in_map c = map.(c) != dummy in
    Dll.iter (fun t -> map.(Dll.peek t) <- t) t;
    let decr_wrap n =
      if n = 1 then max_val else n - 1
    in
    let rec step1 n current =
      let c1, _ = Dll.(pop (next current)) in
      let c2, _ = Dll.(pop (next current)) in
      let c3, _ = Dll.(pop (next current)) in
      remove_from_map c1;
      remove_from_map c2;
      remove_from_map c3;
      step2 n current (decr_wrap (Dll.peek current)) c1 c2 c3
    and step2 n current dest_num c1 c2 c3 =
      if not (is_in_map dest_num) then step2 n current (decr_wrap dest_num) c1 c2 c3
      else
        step3 n current (map.(dest_num)) c1 c2 c3
    and step3 n current dest c1 c2 c3 =
      let n3 = Dll.insert_after dest c3 in map.(c3) <- n3;
      let n2 = Dll.insert_after dest c2 in map.(c2) <- n2;
      let n1 = Dll.insert_after dest c1 in map.(c1) <- n1;
      if n = 1 then ()
      else step1 (n-1) Dll.(next current)
    in
    step1 rounds t;
    map.(1)

  let collect t =
    let acc = ref "" in
    Dll.iter (fun t -> let v = Dll.peek t in if v <> 1 then
                 acc := !acc ^ string_of_int v
             ) t;
    !acc

  let fill_million current =
    for i = 10 to 1000000 do
      ignore @@ Dll.insert_before current i;
    done;
    current


  let solve_part1 () =
    let t = read_input () in
    let t1 = play 9 100 t in
    let s = collect t1 in
    Solution.printf "%s" s

  let solve_part2 () =
    let t = read_input () in
    let t = fill_million t in
    let t1 = play 1_000_000 10_000_000 t in
    let n = Dll.(peek (next t1)) * Dll.(peek (next (next t1))) in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)