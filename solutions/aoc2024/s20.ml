open Utils
open Syntax
module S =
struct

  module G = Grid.StringGrid

  module Gra = struct
    type v = Grid.position
    type t = G.t
    let iter_vertices g f =
      G.iter (fun v _ -> f v) g

    let iter_succ g v f =
      G.iter4 (fun w c _ ->
          if c <> '#' then f (w,1)) g v
  end

  module GraAlg = GraphAlgo(Gra)

  let read_input () =
    let g = G.read () in
    let start = G.find (fun c -> c = 'S') g in
    let exit = G.find (fun c -> c = 'E') g in
    g, start, exit


  let find_shortcuts cheat_len limit g start exit =
    let map = GraAlg.dijkstra g start [exit] in
    let min_dist, paths = map.%{exit} in
    let path = List.hd paths in
    (* Find for each point on the path, the other points further on the path that
       are at a Manhattan distance cheat_len or less
       The point that is on the path at a distance <= cheat_len will be considered
       (which is wrong, it's not a cheat since it does not go through a wall), but
       won't ever contribute since it does not improve strictly the total cost.

    *)
    let count = ref 0 in
    let rec loop i l =
      match l with
        [] -> ()
      | ((x, y) as p) :: ll ->
        List.iteri (fun j ((x', y') as p') ->
            let j = i + j + 1 in
            let md = abs (x' - x) + abs (y' - y) in
            if md > 1 && md <= cheat_len &&
               i + md + (min_dist - j) + limit <= min_dist then
              incr count
          ) ll;
        loop (i+1) ll
    in loop 0 path;
    !count

  let name = Name.mk "s20"
  let solve md limit =
    let grid, start, exit = read_input () in
    let n = find_shortcuts md limit grid start exit in
    Solution.printf "%d" n

  let solve_part1 () = solve 2 100
  let solve_part2 () = solve 20 100
end

let () = Solution.register_mod (module S)