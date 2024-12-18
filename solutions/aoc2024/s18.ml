open Utils
open Syntax
module S =
struct
  let name = Name.mk "s18"

  module G = Grid.BytesGrid

  let rec iter_n f n l =
    match l, n with
      _, 0 -> l
    | [], n when n > 0 -> failwith "iter_n"
    | x::ll, n -> f x; iter_n f (n-1) ll
    | _ -> assert false

  let read_input size =
    let positions = ref [] in
    let grid = G.init size (fun _ -> Bytes.make size '.') in
    Input.fold_scan "%d,%d" (fun () x y ->
        positions := (x, y)::!positions) ();
    grid, !positions |> List.rev

  module Gra =
  struct
    type v = Grid.position
    type t = G.t

    let iter_vertices grid f =
      G.iter (fun v _ -> f v) grid

    let iter_succ g v f =
      G.iter4 (fun w c _ ->
          if c <> '#' then f (w, 1)) g v
  end

  module Alg = GraphAlgo(Gra)

  let min_dist limit grid positions =
    let rem_pos = iter_n (fun p -> grid.G.!(p) <- '#' ) limit positions in
    let exit = G.width grid - 1, G.height grid -1 in
    let map = Alg.dijkstra ~first:true grid (0,0) [exit] in
    let d, l = map.%{exit} in d, l, rem_pos
  let solve_part1 () =
    let grid, positions = read_input 71 in
    let n,_, _ = min_dist 1024 grid positions in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  let rec min_block limit grid positions =
    let _, l, rem_positions = min_dist limit grid positions in
    let exit = G.width grid - 1, G.height grid -1 in
    let on_path = ~%[] in
    let mk_path l = List.hd l |> List.iter (fun p -> on_path.%{p} <- ()) in
    let () = mk_path l in
    let rec loop pos =
      match pos with
        [] -> assert false (* no position found *)
      | block :: ppos ->
        grid.G.!(block) <- '#';
        if on_path %? block then begin (* invalidate the current path *)
          let map = Alg.dijkstra ~first:true grid (0,0) [exit] in
          let d, l = map.%{exit} in
          if d = max_int then block
          else begin
            Hashtbl.clear on_path;
            mk_path l;
            loop ppos;
          end
        end else loop ppos
    in
    loop positions

  let solve_part2 () =
    let grid, positions = read_input 71 in
    let x, y = min_block 1024 grid positions in
    Ansi.(printf "%a%d,%d%a\n%!" fg green x y clear color)

end

let () = Solution.register_mod (module S)