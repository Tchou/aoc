open Utils
open Syntax
module S =
struct
  let name = Name.mk "s20"
  module G = Grid.StringGrid

  let read_input () =
    let grid = G.read () in
    let portal_map = ~%[] in
    G.iter (fun p c ->
        if c = '.' then
          G.iter4 (fun q c1 d ->
              let open Grid in
              if c1 >= 'A' && c1 <= 'Z' then
                let c2 = grid.G.!(q +! d) in
                let key =
                  String.implode
                    (if d = east || d = south then [c1; c2] else [c2; c1])
                in
                portal_map.%{key} <- p :: (portal_map.%?{key} or [])
            ) grid p
      ) grid;
    let is_outer (x, y) = x = 2 || y = 2 || x = G.width grid - 3 || y = G.height grid - 3 in
    let portals = ~%[] in
    portal_map
    |> Hashtbl.iter (fun _ l ->
        match l with
          [p1; p2] ->
          portals.%{p1} <- p2, if is_outer p2 then 1 else -1;
          portals.%{p2} <- p1, if is_outer p1 then 1 else -1;
        | _ ->());
    (grid, portals), List.hd portal_map.%{"AA"}, List.hd portal_map.%{"ZZ"}

  module Graph  =
  struct
    type t = G.t * (int*int, (int*int) * int) Hashtbl.t
    type v = Grid.position
    let iter_vertices _ _ = assert false
    let iter_succ (grid, portals) p f =
      G.iter4 (fun q c _ ->
          if c = '.' then f (q, 1)
        ) grid p;
      match portals.%{p} with
        q,_ -> f (q, 1)
      | exception Not_found -> ()
  end
  module Alg = GraphAlgo(Graph)

  let solve_part1 () =
    let g, start, exit = read_input () in
    let map = Alg.dijkstra g start [exit] in
    let n = fst map.%{exit} in
    Solution.printf "%d" n

  module Graph2  =
  struct
    type t = G.t * (int*int, (int*int) * int) Hashtbl.t
    type v = Grid.position * int (* level *)
    let iter_vertices _ _ = assert false
    let iter_succ (grid, portals) (p, lvl) f =
      G.iter4 (fun q c _ ->
          if c = '.' then f ((q, lvl), 1)
        ) grid p;
      match portals.%{p} with
        q,n -> let lvl' = lvl + n in if lvl' >= 0 then f ((q,lvl'), 1)
      | exception Not_found -> ()
  end
  module Alg2 = GraphAlgo(Graph2)

  let solve_part2 () =
    let g, start, exit = read_input () in
    let map = Alg2.dijkstra g (start,0) [(exit,0)] in
    let n = fst map.%{exit,0} in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)