open Utils
open Syntax
module S =
struct
  let name = Name.mk "s12"

  let read_input () =
    let pot_line = ~%[] in
    let min_i = ref max_int in
    let max_i = ref min_int in
    let last =
      match String.split_on_char ' ' (Input.read_line ()) with
        _initial :: _state :: s::[] ->
        String.iteri (fun i c -> pot_line.%{i} <- c;
                       if c = '#' then begin
                         min_i := min !min_i i;
                         max_i := max !max_i i;
                       end
                     ) s;
        String.length s - 1
      | _ -> assert false
    in
    ignore (Input.read_line ());
    let rules = ~%[] in
    Input.fold_lines (fun acc l ->
        match String.split_on_char ' ' l with
          pattern :: _ :: res :: [] ->
          rules.%{Bytes.of_string pattern} <- res.[0]
        | _ -> assert false
      ) ();
    pot_line, !min_i, !max_i, rules

  let string_of_pots first last pots =
    String.init (last - first + 1) (fun i -> pots.%{first + i})

  let pots_of_string first last str =
    let pots = ~%[] in
    String.iteri (fun i c ->
        pots.%{first + i} <- c) str;
    pots

  let apply rules first last pots new_pots =
    let b = Bytes.make 5 '.' in
    let min_i = ref max_int in
    let max_i = ref min_int in
    for i = first - 3 to last + 3 do
      b.$[0] <- pots.%?{i} or '.';
      b.$[1] <- pots.%?{i+1} or '.';
      b.$[2] <- pots.%?{i+2} or '.';
      b.$[3] <- pots.%?{i+3} or '.';
      b.$[4] <- pots.%?{i+4} or '.';
      let c = rules.%?{b} or '.' in
      new_pots.%{i+2} <- c;
      if c = '#' then begin
        min_i := min !min_i (i+2);
        max_i := max !max_i (i+2);
      end;
    done;
    !min_i, !max_i


  let score pots = Hashtbl.fold (fun i c acc -> if c = '#' then i+acc else acc) pots 0
  let simulate rules first last pots n =
    let new_pots = ~%[] in
    let rec loop k first last pots new_pots  =
      if k = n then score pots
      else
        let () = Hashtbl.clear new_pots in
        let first', last' = apply rules first last pots new_pots in
        loop (k+1) first' last' new_pots pots
    in
    loop 0 first last pots new_pots

  let solve_part1 () =
    let pots, first, last, rules = read_input () in
    let count = simulate rules first last pots 2345 in
    Solution.printf "%d" count

(*
   By printing the values we see that the pattern of pots reaches
   a fixed point and then only moves to the right.
   The function wich returns the part with the pots reaches a fix point:
   Use it to determine the final score:
*)

  let compute_cycle rules first last pots n =
    let new_pots = ~%[] in
    let f (first, last, s) =
      let pots = pots_of_string first last s in
      Hashtbl.clear new_pots;
      let first', last' = apply rules first last pots new_pots in
      let s' = string_of_pots first' last' new_pots in
      (first', last', s')
    in
    let equal (f1, l1, s1) (f2, l2, s2) =
      (l1 - f1) == (l2 - f2) || s1 = s2
    in
    let lam, mu, (first', last', s) = Misc.find_cycle equal f (first, last, string_of_pots first last pots) in
    if n <= mu then simulate rules first last pots n (* Just in case we were asked a value before the cycle *)
    else
      (* the cycle has size lam, and starts after the mu-th call. Afterwards,
         the same sequence of pots just slides to the right *)
      let d = mu - first' in
      (* Given a number n of generations, the first is at index (n-d) *)
      Hashtbl.clear new_pots;
      let pots = pots_of_string (n-d) (n-d + last'-first' + 1) s in
      score pots


  let solve_part2 () =
    let pots, first, last, rules = read_input () in
    let n = compute_cycle rules first last pots 50_000_000_000 in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)