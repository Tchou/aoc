open Utils
open Syntax

module S =
struct
  let name = Name.mk "s15"

  module Graph (M : sig val size : int end): sig
    include GRAPH with type t = string array and type v = (int*int)
    val (.@[]) : t -> v -> int
    val n_row : t -> int
    val n_col : t -> int
  end =
  struct
    let size = M.size

    type t = string array
    type v = (int * int)

    let n_row g = size * Array.length g
    let n_col g = size * String.length g.(0)

    let (.@[]) g (row, col) =
      let h = Array.length g in
      let w = String.length g.(0) in
      let r = row mod h in
      let c = col mod w in
      let v = Char.code g.(r).[c] - Char.code '1' in
      let v2 = ((v + (row/h) + (col/w)) mod 9) + 1 in
      v2

    let iter_vertices g f =
      for row = 0 to n_row g - 1 do
        for col = 0 to n_col g - 1 do
          f (row, col)
        done;
      done

    let iter_succ g (r, c) f  =
      let l = [ (-1, 0); (1, 0); (0,1); (0,-1)] in
      let nr = n_row g in
      let nc = n_col g in
      List.iter (fun (i, j) ->
          let ri = r + i in
          let cj = c + j in
          if ri >= 0 && cj >= 0 && ri < nr && cj < nc then
            let w = ri, cj in
            f (w, g.@[w])) l
  end

  let path_finder n graph =
    let module G = Graph (struct let size = n end) in
    let module P = GraphAlgo(G) in
    let src = 0, 0 in
    let dst = (G.n_row graph - 1, G.n_col graph - 1) in
    let finish_map = P.dijkstra graph src [dst] in
    finish_map.%{dst}

  let read_input () =
    Input.fold_lines (fun acc s -> s :: acc) []
    |> List.rev
    |> Array.of_list


  let display n graph path =
    let module G = Graph (struct let size = n end) in
    let last_col = (n * String.length (graph.(0))) - 1 in
    G.iter_vertices graph (fun (r, c) ->
        let v = G.(graph.@[r, c]) in
        if List.mem (r, c) path then
          Ansi.(printf "%a%d%a" bfg cyan v clear color)
        else Ansi.printf "%d" v;
        if c = last_col then Ansi.printf "@\n")
  let solve size =
    let graph = read_input () in
    let n, _p = path_finder size graph in
    (*display size graph p;*)
    Solution.printf "%d" n
  let solve_part1 () = solve 1

  let solve_part2 () = solve 5
end

let () = Solution.register_mod (module S)