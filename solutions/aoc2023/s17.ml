open Utils
module S (A : sig val show : bool end) =
struct
  let name = Name.mk "s17"
  module Graph =
  struct
    type t = string array
    type v = { cell : int*int;
               dir : (int * int);
               steps : int
             }

    let start = { cell = (0, 0);
                  dir = (0,0);
                  steps = 0;
                }

    let last_cell g =
      let r = Array.length g - 1 in
      let c = String.length g.(0) - 1 in
      r, c
    let targets_gen min max g =
      let r, c = last_cell g in
      let cell = r, c in
      let t = ref [] in
      for steps = min to max do
        t := { cell; dir = (1,0); steps }::!t;
        t := { cell; dir = (0,1); steps }::!t;
      done;
      !t
    let iter_vertices _g _f = assert false

    let cost g (r, c) =
      Char.code g.(r).[c] - Char.code '0'
    let is_valid g (r, c) =
      r >= 0 && c >= 0 && r < Array.length g && c < String.length g.(0)

    let left (i, j) = (-j, i)
    let right (i, j) = (j, -i)

    let iter_succ_gen apply g ({cell;dir;steps} as v) f =
      if steps = 0 then begin
        apply g f v (1,0);
        apply g f v (0,1);
      end else begin
        apply g f v dir;
        apply g f v (left dir);
        apply g f v (right dir);
      end
    let apply g f {cell;dir;steps}  ((i, j) as dir') =
      let r, c = cell in
      let cell = r + i, c + j in
      if is_valid g cell then
        let cst = cost g cell in
        if dir' = dir && steps < 3 then
          f ({cell; dir; steps = steps + 1}, cst)
        else if dir <> dir' then
          f ({cell; dir=dir'; steps = 1}, cst)

    let iter_succ = iter_succ_gen apply
    let targets = targets_gen 1 3
  end

  module Graph2 =
  struct
    include Graph
    let targets = targets_gen 4 10

    let apply g f {cell;dir;steps}  ((i, j) as dir') =
      let r, c = cell in
      let cell = r + i, c + j in
      if is_valid g cell then
        let cst = cost g cell in
        if dir' = dir && steps < 10 then
          f ({cell; dir; steps = steps + 1 }, cst)
        else if steps = 0 || (dir <> dir' && steps >= 4) then
          f ({cell; dir=dir'; steps = 1}, cst)
    let iter_succ = iter_succ_gen apply
  end

  module Algo = GraphAlgo(Graph)
  module type G =
  sig
    include GRAPH with type t = String.t array and type v = Graph.v
    val start : v
    val targets : t -> v list
  end

  let load_input () =
    Input.fold_lines (Fun.flip List.cons) []
    |> List.rev |> Array.of_list

  let tile_color = function
    '1'..'2' -> Ansi.white
    | '3'..'5' -> Ansi.yellow
    | '6'..'7' -> Ansi.magenta
    | _ -> Ansi.red

  let solve (module G : G) =
    let g = load_input () in
    let module Algo = GraphAlgo(G) in
    let final_map =
      Algo.dijkstra g ~first:true G.start (G.targets g)
    in
    let len,p =
      Hashtbl.fold (fun _  (c, p) (acc,accp) ->
          if c < acc then (c, p) else (acc,accp)
        ) final_map (max_int,[])
    in
    if A.show then begin
      let open Ansi in
      Array.iteri (fun r s ->
          String.iteri (fun c b ->
              if List.exists (fun v -> v.Graph.cell = (r,c) ) p
              then printf "%a%c%a" bbg cyan b clear color
              else printf "%a%c%a" fg (tile_color b) b clear color
            )
            s; printf "\n" )g
    end;
    Ansi.printf "%d\n" len


  let solve_part1 () = solve (module  Graph)
  let solve_part2 () = solve (module  Graph2)
end

let () = Solution.register_mod (module S(struct let show = false end))
let () = Solution.register_mod ~variant:"visualize" (module S(struct let show = true end))