open Utils
open Syntax

module S =
struct
  let name = Name.mk "s08"

  let comps = [ "==", (=); "!=", (<>);  "<", (<); 
                ">", (>); ">=", (>=); "<=", (<=)]
  let ops = [ "dec", (-); "inc", (+) ]
  let read_input () =
    Input.list_scan "%[a-z] %[incde] %d if %[a-z] %[<>!=] %d"
      (fun r1 op n r2 cmp c ->
         (r1, List.assoc op ops, n, r2, List.assoc cmp comps, c))

  let eval instrs =
    let regs = ~%[] in
    let vmax = ref min_int in
    List.iter (fun (r1, op, n, r2, cmp, c) ->
        if cmp (regs.%?{r2} or 0 ) c then begin
          let v = op (regs.%?{r1} or 0) n in
          let () = vmax := max !vmax v in
          regs.%{r1} <- v
        end) instrs;
    Iter.(regs |> values |> max_ ), !vmax

  let solve f =
    let instrs = read_input () in
    let n = f (eval instrs) in
    Solution.printf "%d" n

  let solve_part1 () = solve fst
  let solve_part2 () = solve snd
end

let () = Solution.register_mod (module S)