open Utils
open Syntax
module S =
struct
  module G = Grid.IntGrid
  let name = Name.mk "s22"
  let read_input () =
    let depth = Scanf.sscanf (Input.read_line ()) "depth: %d" Fun.id in
    let (tx, ty) = Scanf.sscanf (Input.read_line ()) "target: %d, %d"
        (fun x y -> x, y)
    in
    depth, (tx, ty)

  let rec geologic_index depth ((tx, ty) as target) grid (x, y) =
    if (x = 0 && y = 0) || (x = tx && y = ty) then 0
    else if y = 0 then 16807 * x
    else if x = 0 then 48271 * y
    else
      erosion_level depth target grid (x-1, y)
      * erosion_level depth target grid (x, y-1)

  and erosion_level depth target grid pos =
    match grid.%?{pos} with
      Some e -> e
    | None ->
      let i = geologic_index  depth target grid pos in
      let e = (i + depth) mod 20183 in
      grid.%{pos} <- e;
      e
  let region_type depth target grid pos =
    (erosion_level depth target grid pos) mod 3

  let total_risk_level depth ((tx, ty) as target) =
    let grid = ~%[] in
    let total = ref 0 in
    for y = 0 to ty do
      for x = 0 to tx do
        total := !total + region_type depth target grid (x, y)
      done;
    done;
    grid, !total


  let solve_part1 () =
    let depth, target = read_input () in
    let _, n = total_risk_level depth target in
    Solution.printf "%d" n

  type tool = Neither | Torch | ClimbingGear

  module Gra = struct
    type v = Grid.position * tool
    type t = (int * Grid.position * (Grid.position, int) Hashtbl.t)

    let iter_vertices _ _ = assert false

    let valid_move tool region =
      match tool, region with
      | Neither, 0 -> [ 8, Torch; 8, ClimbingGear ]
      | Torch, 0 ->   [ 1, Torch; 8, ClimbingGear ]
      | ClimbingGear, 0 -> [ 1, ClimbingGear; 8, Torch]

      | Neither, 1 -> [ 1,Neither; 8, ClimbingGear ]
      | Torch, 1 -> [ 8, Neither; 8, ClimbingGear ]
      | ClimbingGear, 1 -> [1, ClimbingGear; 8, Neither]

      | Neither, 2 -> [ 1,Neither; 8, Torch ]
      | Torch, 2 -> [ 8, Neither; 1, Torch ]
      | ClimbingGear, 2 -> [8, Torch; 8, Neither]
      | _, _ -> assert false
    let iter_succ (depth, target, grid) ((pos, tool):v) (f: v*int -> unit) =
      Grid.dir4
      |> List.iter (fun d ->
          let (px, py) = Grid.(pos +! d) in
          if px >= 0 && py >= 0 then begin
            let r = region_type depth target grid (px, py) in
            valid_move tool r
            |> List.iter (fun (n, t') -> f (((px, py), t'), n))
          end
        )
  end

  module GrAlgo = GraphAlgo(Gra)

  let fastest_path depth target =
    let grid = ~%[] in
    let map =
      GrAlgo.dijkstra (depth, target, grid) ((0,0),Torch) [(target, Torch)]
    in
    map
    |> Hashtbl.to_seq_values
    |> Seq.map fst
    |> Iter.(min seq)
  let solve_part2 () =
    let depth, target = read_input () in
    let n = fastest_path depth target in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)