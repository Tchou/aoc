open Utils
open Syntax

module S =
struct
  let name = Name.mk "s12"

  module StrSet = Set.Make(String)
  let is_upper =
    String.for_all (fun c -> c >= 'A' && c <= 'Z')

  (* Simple DFS. For the second part, if a small cave has not been visited along
     the path:
     - either mark it as visited, another cave can be visited twice
     - or don't mark it, but remember it *has to be visited twice*, otherwise
       this branching will also find paths where this cave is not visited twice,
       counting doubles.
  *)
  let dfs revisit graph =
    let counter = ref 0 in
    let rec loop visited visit_small v =
      if v = "end" then begin
        match visit_small with
          None -> incr counter
        | Some w -> if StrSet.mem w visited then incr counter
      end else if not (StrSet.mem v visited) then begin
        let adj = graph.%?{v} or [] in
        if is_upper v then
          List.iter (loop visited visit_small) adj
        else if v = "start" || visit_small <> None then
          List.iter (loop (StrSet.add v visited) visit_small) adj
        else begin
          List.iter (loop visited (Some v)) adj;
          List.iter (loop (StrSet.add v visited) None) adj
        end
      end
    in
    let visit_small = if revisit then None else Some "start" in
    loop StrSet.empty visit_small "start"; !counter

  let solve revisit =
    let graph =
      Input.fold_scan "%[^-]-%s"
        (fun acc src dst ->
           acc.%{src} <- dst::(acc.%?{src} or []);
           acc.%{dst} <- src::(acc.%?{dst} or []);
           acc
        ) ~%[]
    in
    let n = dfs revisit graph in
    Ansi.printf "%d\n" n

  let solve_part1 () = solve false
  let solve_part2 () =
    Time.time solve true |> snd |> Printf.printf "%f ms\n%!"
end

let () = Solution.register_mod (module S)