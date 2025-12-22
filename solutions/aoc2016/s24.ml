open Utils
open Syntax
module S =
struct
  open Grid
  let name = Name.mk "s24"
  let read_input () =
    StringGrid.read ()

  let locations grid =
    let rec loop pos acc =
      match StringGrid.find_from 
              (function '0'..'9' -> true | _ -> false) 
              grid pos 
      with
      exception Not_found -> acc
      | (x, y) as pos ->
        let acc = (grid.StringGrid.!(pos), pos)::acc in
        let npos = (x+1, y) in
        let npos = if StringGrid.inside grid npos then npos else (0, y+1) in
        loop npos acc
    in
    loop (0,0) []

  module G : GRAPH with type t = StringGrid.t and type v = position = struct
    type t = StringGrid.t
    type v = position

    let iter_vertices g f =
      StringGrid.iter (fun p c -> 
          if c <> '#' then f p) g

    let iter_succ g v f = 
      dir4 |> List.iter (fun d ->
          let v' = (v +! d) in
          if StringGrid.inside g v' &&
             g.StringGrid.!(v') <> '#' then
            f (v', 1)
        ) 
  end
  module GA=GraphAlgo(G)
  module HPos = Hashtbl.Make(struct
      type t = position
      let equal = (=)
      let hash = Hashtbl.hash
    end)
  let man_dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
  let compute_location_graph grid locs = 
    let graph = ~%[] in
    locs |> List.iteri (fun i (c1, p1) -> 
        locs |> List.iteri (fun j (c2, p2) -> 
            if j > i then
              let path = GA.astar (module HPos) grid ~h:(man_dist p2) p1 (fun p -> p = p2) in
              let dist = List.length path - 1 in
              graph.%{c1} <- (c2, dist) :: (graph.%?{c1} or []);
              graph.%{c2} <- (c1, dist) :: (graph.%?{c2} or []);
          ));
    graph

  let enumerate_paths ?(part2=false) f graph locations =
    let len = List.length locations in
    let rec loop v path n =
      if n = len then 
        let path = if part2 then ('0',(List.assoc '0' graph.%{v}))::path
          else path in
        f (List.rev path)
      else
        graph.%{v} |> List.iter (fun (v', d) -> 
            if not (List.exists (fun (w, _) -> w = v') path) then
              loop v' ((v', d)::path) (n+1)
          )
    in
    loop '0' [('0',0)] 1


  let pp_path fmt l = 
    let total = ref 0 in
    List.iter (fun (c, d) -> total := !total + d; Format.fprintf fmt "%c(%d) " c d) l;
    Format.fprintf fmt ": %d" !total


  let solve part2 () =
    let grid = read_input () in
    let locs = locations grid in
    let min_len = ref max_int in
    let graph = compute_location_graph grid locs in
    enumerate_paths ~part2 (fun p -> 
        if false then Format.printf "%a\n%!" pp_path p;
        min_len := min !min_len Iter.(sum (snd list) int p)
      ) graph locs;
    Solution.printf "%d" !min_len

  let solve_part1 = solve false
  let solve_part2 = solve true
end

let () = Solution.register_mod (module S)