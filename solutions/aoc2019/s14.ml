open Utils
open Syntax
module S =
struct
  let name = Name.mk "s14"
  let atom s =
    Scanf.sscanf s "%d %s" (fun d s -> (d, s))
  let read_input () =
    let map = ~%[] in
    Input.fold_scan "%[0-9A-Z, ]=> %[0-9A-Z, ]" (fun () s1 s2 ->
        let l = String.split_on_char ',' s1 in
        let (quantity, name) = atom (String.trim s2) in
        let formula = List.map (fun s ->
            atom (String.trim s)
          ) l
        in
        map.%{name} <- (quantity, formula)) ();
    map

  let pp fmt l =
    let l = List.map (fun (a,b) -> string_of_int a ^ " " ^ b) l in
    let s = String.concat ", " l in
    Format.fprintf fmt "[ %s ]" s

  let div_sup a b =
    a / b + int_of_bool (a mod b <> 0)

  let expand map l =
    let extra_map = ~%[] in
    let rec loop l ore_count  =
      match l with
        [] -> ore_count
      | (amount, name) :: ll ->
        (* we need to produce an amount number of name element  *)
        let extra_avaliable = extra_map.%?{ name } or 0 in
        (* If we have extra left-over from previous reactions, use them. *)
        if extra_avaliable >= amount then begin
          extra_map.%{name} <- extra_avaliable - amount;
          loop ll ore_count
        end else (* Otherwise, consume leftover *)
          let amount = amount - extra_avaliable in
          (* get the formula *)
          let produced, formula = map.%{name} in
          (* each application of the rule creates [produced] elements,
             we need to call it at least k times *)
          let k = div_sup amount produced in
          let rproduced = k * produced in
          let over_production = rproduced - amount in (* stash what was over produced *)
          let () = extra_map.%{name} <- over_production  in
          (* We created k times name elements, so we multiply each ingredient's quantity of name
             k times. If an ingredient is ore, add its amount to the global amount of ore required.
             Ohterwise add it to the todo list. *)
          let nl, nore_count =
            List.fold_left (fun (l, oc) (p, n) ->
                if n = "ORE" then (l, oc + p*k)
                else  (p * k, n):: l, oc) (ll, ore_count) formula
          in
          loop nl nore_count
    in
    (loop l 0)
  let binary_search map ore =
    let rec loop fsup osup finf oinf =
      let fmid = finf + (fsup - finf)/2 in
      let omid = expand map [(fmid, "FUEL")] in
      if omid = oinf then fmid
      else if omid < ore then loop fsup osup fmid omid else
        loop fmid omid finf oinf
    in
    let osup = expand map [(ore, "FUEL")] in
    loop ore osup 0 0


  let solve_part1 () =
    let map = read_input () in
    let n = expand map [(1, "FUEL")] in
    Solution.printf "%d" n

    let solve_part2 () =
    let map = read_input () in
    let n = binary_search map 1000000000000 in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)