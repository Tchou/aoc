open Utils

module S =
struct
  let name = Name.mk "s24"

  let read_input () =
    let n = ref 0 in
    let l = Input.list_scan "%d/%d" (fun a b -> 
        n := max !n (max a b);
        (a,b))
    in
    !n, l

  let strongest_bridge part1 m l =
    let map = Array.make (m+1) [] in
    let () = 
      List.iter (fun ((a,b) as c) -> 
          map.(a) <- c :: map.(a);
          map.(b) <- c :: map.(b);
        ) l
    in
    let f = int_of_float (10.0 ** (ceil (log10 (float (m+1))))) in
    let free = Array.make ((m+1)*f+(m+1)) true in
    let max_score = ref 0 in
    let max_len = ref 0 in
    let rec loop len n total =
      if part1 || len >= !max_len then begin
        max_len := len;
        if total > !max_score then max_score := total;
      end;
      let comps = map.(n) in
      comps
      |> List.iter (fun (a,b) -> 
          let idx = (a*f+b) in
          if free.(idx) then begin
            free.(idx) <- false;
            loop (len+1) (if a = n then b else a) (total + a + b);
            free.(idx) <- true;
          end)
    in
    loop 0 0 0;
    !max_score



  let solve part1 =
    let m, l = read_input () in
    let n = strongest_bridge part1 m l in
    Solution.printf "%d" n

  let solve_part1 () = solve true

  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S)