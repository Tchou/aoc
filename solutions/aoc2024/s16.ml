open Utils
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


  let score part2 grid start exit =
    let el = ref [] in
    G.iter4 (fun _ c d -> if c = '.' then el := (exit, Grid.opposite d) :: !el ) grid exit;
    let all_path = part2 in
    let all_positions = ref 0 in
    let post = if part2 then
        let module GB = Grid.BytesGrid in
        let h, w = G.(height grid, width grid) in
        let visited = GB.init h (fun _ -> Bytes.make w '\x00') in
        Some (fun ((v, _) as u) ->
            if visited.GB.!!(v) == '\x00' then begin
              visited.GB.!(v) <- '\x01';
              incr all_positions;
            end)
      else None
    in
    let map = Alg.dijkstra grid ?post ~all_path (start, Grid.east) !el in
    Hashtbl.fold (fun _ (dist, _) acc -> min dist acc) map max_int, !all_positions

  let read_input () =
    let grid = G.read () in
    let start = G.find (function 'S' ->true | _ -> false) grid in
    let exit = G.find (function 'E' -> true | _ -> false) grid in
    grid, start, exit
  let solve_part1 () =
    let grid, start, exit = read_input () in
    let n,_ = score false grid start exit in
    Solution.printf "%d" n

  let solve_part2 () =
    let grid, start, exit = read_input () in
    let _, n = score true grid start exit in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)