open Utils
open Syntax
module S =
struct
  let name = Name.mk "s12"
  module G = Grid.StringGrid

  let hdirs = Grid.[east; west]
  let vdirs = Grid.[south; north]

  let in_plot p1 l =
    l |> List.exists (fun p2 ->
        Grid.(dir4 |> List.exists (fun d ->
            p1 = p2 +! d
          ))
      )

  let is_on_side grid p a =
    not (G.inside grid p) || grid.G.!(p) <> a

  let perimeter grid l a =
    l
    |> List.map (fun p ->
        Grid.dir4
        |> Iter.count_if (fun d ->
            is_on_side grid Grid.(p +! d) a
          ) List.to_seq)
    |> Iter.sum (module Int) List.to_seq

  (* for the side, find the number of corners, that is the number
     of points that have two neighbors one each vertical/horizontal
     which is not in the surface (external) or which has both
     neighbor in the surface but not the diagonal (internal)

      XXXX        1XX2   1X2X
      X.X         X.X    X X
      XXX         1XX    1X2
        X           X      X
        X           2      X
  *)
  let ext_corners = Grid.[ west,north; north, east; east, south; south, west ]
  let int_corners = List.map (fun (p1, p2) -> (p1, p2), Grid.(p1+! p2)) ext_corners

  let sides grid l a =
    let open Grid in
    let is_external p (d1, d2) =
      let p1 = p +! d1 in
      let p2 = p +! d2 in
      is_on_side grid p1 a && is_on_side grid p2 a
    in
    let is_internal p ((d1, d2), d3) =
      let p1 = p +! d1 in
      let p2 = p +! d2 in
      let p3 = p +! d3 in
      not (is_on_side grid p1 a) &&
      not (is_on_side grid p2 a) &&
      ((G.inside grid p3) && grid.G.!(p3) <> a)
    in
    let count_corners p test corners =
      corners
      |> Iter.count_if (test p) List.to_seq
    in
    l
    |> List.map (fun p ->
        let ce = count_corners p is_external ext_corners in
        let ci = count_corners p is_internal int_corners in
        ci + ce)
    |> Iter.sum (module Int) List.to_seq

  (*
  Do a simple DFS for each (non-visited) point in the grid to find each plot containing
  that point.
  *)

  let map_plots2 grid =
    let module GB = Grid.BytesGrid in
    let map = ~%[] in
    let visited = GB.init G.(height grid) (fun _ -> Bytes.make G.(width grid) '\x00') in
    let rec dfs c stack acc =
      match stack with
        [] -> acc
      | p :: sstack ->
        if visited.GB.!(p) = '\x01' then dfs c sstack acc else
          begin
            visited.GB.!(p) <- '\x01';
            let nstack = ref sstack in
            G.iter4 (fun p' c' _ -> if c' = c then nstack := p'::!nstack) grid p;
            dfs c !nstack (p::acc)
          end
    in
    G.iter
      (fun p c -> if visited.GB.!(p) = '\x00' then map.%{c} <- (dfs c [p] []) :: (map.%?{c} or []))
      grid;
    map

  let count_plots perim grid map =
    map
    |> Hashtbl.to_seq
    |> Seq.map (fun (a, ll) ->
        ll
        |> List.map (fun l ->
            (perim grid l a) * (List.length l))
        |> Iter.sum (module Int) List.to_seq)
    |> Iter.sum (module Int) Fun.id

  let solve perim =
    let grid = G.read () in
    let map = map_plots2 grid in
    let n = count_plots perim grid map in
    Solution.printf "%d" n
  let solve_part1 () = solve perimeter
  let solve_part2 () = solve sides

end

let () = Solution.register_mod (module S)