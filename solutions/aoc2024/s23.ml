open Utils
open Syntax
module S =
struct
  let name = Name.mk "s23"
  module StrSet = Set.Make(String)

  let read_input () =
    let graph = ~%[] in
    Input.fold_scan "%[^-]-%[^-]" (fun graph v1 v2 ->
        graph.%{v1} <- StrSet.(add v2 (graph.%?{v1} or empty));
        graph.%{v2} <- StrSet.(add v1 (graph.%?{v2} or empty));
        graph
      ) graph

  let order v1 v2 v3 =
    let open String in
    let v1, v2 = if compare v1 v2  < 0 then v1, v2 else v2, v1 in
    if compare v2 v3 < 0 then v1, v2, v3
    else if compare v1 v3 < 0 then v1, v3, v2 else v3, v1, v2

  let find_triangles graph =
    let found = ~%[] in
    graph
    |> Hashtbl.iter (fun v1 vl1 ->
        vl1
        |> StrSet.iter (fun v2 ->
            graph.%{v2}
            |> StrSet.iter (fun v3 -> if StrSet.mem v3 vl1 then found.%{order v1 v2 v3} <- ())
          )
      );
    found |> Hashtbl.to_seq_keys

  let count_triangles seq =
    Seq.fold_left (fun acc (v1, v2, v3) ->
        if v1.[0] = 't' || v2.[0] = 't' || v3.[0] = 't' then 1+acc
        else acc) 0 seq

  let solve_part1 () =
    let graph = read_input () in
    let n = count_triangles (find_triangles graph) in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  (* Bron–Kerbosch algorithm, can't be used for part 1 since the triangles are not maximal cliques *)

  let rec find_clique f graph r p x =
    if StrSet.is_empty p && StrSet.is_empty x then f r
    else
      let u = if StrSet.is_empty p then StrSet.choose x else StrSet.choose p in
      StrSet.fold (fun v (ap, ax) ->
          StrSet.(find_clique f graph (add v r) (inter ap graph.%{v}) (inter ax graph.%{v}));
          StrSet.(remove v ap, add v ax)
        ) StrSet.(diff p graph.%{u}) (p, x)
      |> ignore

  let find_max_clique graph =
    let max_clique = ref StrSet.empty in
    let max_clique_size = ref 0 in
    let update clique =
      let size = StrSet.cardinal clique in
      if size > !max_clique_size then begin
        max_clique_size := size;
        max_clique := clique;
      end
    in
    find_clique update graph StrSet.empty (graph |> Hashtbl.to_seq_keys |> StrSet.of_seq) StrSet.empty;
    !max_clique

  let solve_part2 () =
    let graph = read_input () in
    let clique = find_max_clique graph in
    let s = clique |> StrSet.elements |> String.concat "," in
    Ansi.(printf "%a%s%a\n%!" fg green s clear color)
end

let () = Solution.register_mod (module S)