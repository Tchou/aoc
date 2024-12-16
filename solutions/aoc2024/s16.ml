open Utils
open Syntax
module S =
struct
  module G = Grid.StringGrid
  let name = Name.mk "s16"

  module Gra =
  struct
    type v = Grid.position * Grid.dir
    type t = G.t

    let iter_vertices _  _ = ()

    let iter_succ g (p, d) f =
      G.iter4 (fun p2 c d2 ->
          if c <> '#' then begin
            if d = d2 then f ((p2, d2), 1)
            else if d = Grid.opposite d2 then f ((p2, d2), 2001)
            else f ((p2, d2), 1001)
          end
        ) g p
  end
  module Alg = GraphAlgo(Gra)

  let score grid start exit =
    let el = ref [] in
    G.iter4 (fun _ c d -> if c = '.' then el := (exit, Grid.opposite d) :: !el ) grid exit;
    let map = Alg.dijkstra grid (start, Grid.east) !el in
    Hashtbl.fold (fun _ (dist, _) acc -> min dist acc) map max_int

  let explore grid start exit target =
    let cache = ~%[] in
    let visited = ~%[] in
    let rec loop ((v, d) as p) dist path =
      if dist <= target then
        if v = exit then List.iter (fun x -> cache.%{x}<-()) path
        else
          let dv = visited.%?{p} or max_int in
          if dist <= dv then  begin
            Gra.iter_succ
              grid p
              (fun (((v2, _) as p2), n) -> loop p2 (dist + n) (v2::path));
            visited.%{p} <- dist
          end
    in
    loop (start, Grid.east) 0 [start];
    Hashtbl.length cache


  let read_input () =
    let grid = G.read () in
    let start = G.find (function 'S' ->true | _ -> false) grid in
    let exit = G.find (function 'E' -> true | _ -> false) grid in
    grid, start, exit
  let solve_part1 () =
    let grid, start, exit = read_input () in
    let n = score grid start exit in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)



  let solve_part2 () =

    let grid, start, exit = read_input () in
    let dist = score grid start exit in
    let n = explore grid start exit dist in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

end

let () = Solution.register_mod (module S)