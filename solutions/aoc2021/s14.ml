open Utils
open Syntax

(**
   Observation :
   the template grows exponentially, since on char is added between every
   pair, the size increases by ~ 50%. Hence the size after n steps, for
   an initial template size t :
   s = t*1.5*1.5... *1.5 = t (3/2)^n.
   Even though 40 steps is manageable (~few GB of ram), there is a more elegant
   solution.
   Consider a template:
   ABCDE. It's made of pairs AB BC CD DE. Here every letter is duplicated, except
   the first and the last. If we now insert BC -> X
   AB BX XC CD DE
   We keep the invariant that every letter is duplicated, except the initial A
   and terminal E.
   So we just need to 
   - count the number of times each rule is applied.
   - after that for each pair XY with a count of n, create a table saying that
   X occurs n times, and Y n times
   - then from this table, subtract 1 to the count of the first letter and the
   count of the last letter: these correspond to the first and last letter of
   the initial template that were never duplicated.
   - divide every count by two
   - add back 1 to the count of the first and last letter
   - compute the score according to the statement.
*)

module S =
struct
  let name = Name.mk "s14"

  let read_template () =
    let s = read_line ()  in
    let template = ~%[] in
    for i = 1 to String.length s - 1 do
      let key = s.[i-1], s.[i] in
      template.%{key} <- 1 + (template.%?{key} or 0);
    done;
    template, s.[0], s.[String.length s - 1]

  let step map template =
    let ntemplate = ~%[] in
    Hashtbl.iter (fun (c1, c2) n ->
        match map.%?{c1, c2} with
          None -> ntemplate.%{c1, c2} <- n
        | Some r ->
          ntemplate.%{c1, r} <- n + (ntemplate.%?{c1, r} or 0);
          ntemplate.%{r, c2} <- n + (ntemplate.%?{r, c2} or 0)
      ) template;
    ntemplate


  let repeat map template n =
    let res = ref template in
    for _ = 0 to n-1 do
      res := step map !res;
    done;
    !res

  let score template c_fst c_lst =
    let dcounts = ~%[] in
    Hashtbl.iter (fun (c1, c2) n ->
        dcounts.%{c1} <- n + (dcounts.%?{c1} or 0);
        dcounts.%{c2} <- n + (dcounts.%?{c2} or 0)) template;
    dcounts.%{c_fst} <- dcounts.%{c_fst} - 1;
    dcounts.%{c_lst} <- dcounts.%{c_lst} - 1;
    let counts = ~%[] in
    Hashtbl.iter (fun c n -> counts.%{c} <- n/2) dcounts;
    counts.%{c_fst} <- counts.%{c_fst} + 1;
    counts.%{c_lst} <- counts.%{c_lst} + 1;
    let arr = counts |> Hashtbl.to_seq |> Array.of_seq in
    Array.sort (Compare.snd) arr;
    let _, nmin = arr.(0) in
    let _, nmax = arr.(Array.length arr - 1) in
    (nmax- nmin)

  let solve n =
    let template, c0, cn = read_template () in
    let _ = read_line () in (* empty *)
    let map = ~%[] in
    Input.fold_scan "%c%c -> %c" (fun () c1 c2 r ->
        map.%{c1,c2} <- r
      ) ();
    let template = repeat map template n in
    let n = score template c0 cn in
    Ansi.printf "%d\n" n

  let solve_part1 () = solve 10
  let solve_part2 () = solve 40
end

let () = Solution.register_mod (module S)