open Utils

module S =
struct
  let name = Name.mk "s22"
  let compare_pair (x1,y1) (x2, y2) = 
    let c = Int.compare x1 x2 in if c <> 0 then c else Int.compare y1 y2 
  module Cmap = Map.Make(struct type t = Grid.position
      let compare = compare_pair

    end)

  let read_input () =
    Input.read_line () |> ignore;
    Input.read_line () |> ignore;
    let max_x = ref min_int in
    let max_y = ref min_int in
    let empty = ref (-1, -1) in
    let t = ref Cmap.empty in 
    Input.fold_scan "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%"
      (fun () x y total used _avail _usep -> 
         t := Cmap.add (x, y) (used, total) !t;
         if used = 0 then (assert (!empty = (-1,-1)); empty := (x, y));
         max_x := max !max_x x;
         max_y := max !max_y y;
      ) ();
    !t,!max_x + 1,!max_y + 1, !empty

  let viable g =
    let count = ref 0 in
    g |> Cmap.iter (fun (x1, y1) (used1, total1) ->
        if used1 <> 0 then
          g |> Cmap.iter (fun (x2, y2) (used2, total2) ->
              if used1 <= (total2-used2) && (x1 <> x2 || y1 <> y2)  then
                incr count
            )
      );
    !count

  let solve_part1 () =
    let g, width, height, empty = read_input () in
    let n = viable g in
    Solution.printf "%d" n


  module Config : sig 
    type v = {
      empty : (int * int);
      data : (int * int);
      map : (int * int) Cmap.t;
    }
    include GRAPH with type v := v
    val graph : t
    module V : Hashtbl.HashedType with type t = v
    val init : (int * int) -> (int * int) -> (int*int) Cmap.t -> v
    val is_final : v -> bool
    val h : v -> int
    val pp : int -> int -> Format.formatter -> v -> unit
  end = struct
    type t = unit
    let graph = ()
    type v = 
      {
        empty : (int * int);
        data : (int * int);
        map : (int * int) Cmap.t;
      }
    module V = struct 
      type t = v
      let hash = Hashtbl.hash
      let equal v1 v2 =
        compare_pair v1.empty v2.empty = 0 &&
        compare_pair v1.data v2.data = 0 &&
        Cmap.equal (fun p1 p2 -> compare_pair p1 p2 = 0) v1.map v2.map
    end
    let init empty data map = { empty; data; map}
    let is_final v = compare_pair v.data (0,0) = 0
    let iter_vertices () (_f : v -> unit) = assert false
    (* 
      ...
      ._.
      ...
    *)
    let h v = 
      let xe, ye = v.empty in
      let xd, yd = v.data in
      abs (xd-xe) + abs (yd-ye)
    let iter_succ () v f =
      let _, etotal = Cmap.find v.empty v.map in
      Grid.dir4 |> List.iter (fun d ->
          let new_empty = Grid.(v.empty +! d) in
          match Cmap.find_opt new_empty v.map with
            None -> ()
          | Some (used, total) ->
            if used <= etotal then (* can exchange with hole *)
              let new_data = if new_empty = v.data then v.empty else v.data in
              let new_v = {
                empty = new_empty;
                data = new_data;
                map = Cmap.add new_empty (0, total) (Cmap.add v.empty (used, etotal) v.map)
              } in
              f (new_v, 1)
        )
    let pp width height fmt v =
      let _, etotal = Cmap.find v.empty v.map in
      let dused, _ = Cmap.find v.data v.map in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          let pos = x, y in 
          if pos = v.data then Format.printf "D"
          else if pos = v.empty then Format.printf " "
          else 
            let used, total = Cmap.find (x, y) v.map in
            (* Check for every cell, we can move it in the hole *)
            if used <= etotal && dused <= total then Format.printf "." else Format.printf "#"
        done;
        Format.printf "\n%!";
      done
  end
  module HV = Hashtbl.Make(Config.V)
  module Algo = GraphAlgo(Config)

  let dist (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)
  let shortest_path_opt map width height empty =
    (* By plotting the grid, we see there is a wall between the 
       initial empty space and the data, with a hole on the far left.
       A shorter version is:
       ..................D
       ...................
       .##################
       ...................
       ......... .........
       ...................

    We also remark that everything but the # is viable, that is we can move
    the hole anywhere (except on #) and we can move D anywhere except on #
    So the total distance is :
    - the manathan distance to go from the intial empty cell to the hole in the wall
       ..................D
       ...................
       _##################
       .x.................
       .xxxxxxxxx.........
       ...................

    - the manathan distance to bring the hole to D
       .................xD
       xxxxxxxxxxxxxxxxxx.
       x##################
       ...................
       ...................
       ...................

    - the (width - 2) x 5 (it takes 5 moves to advance
      D one square to the left and have the hole behind
      it again).
       ....D_  ....D.  ....D.  ....D.  ..._D. ...D_.
       ......  ....._  ...._.  ..._..  ...... ...... 
    *)
    let hole = (0, 3) in (* looked at the map *)
    let d1 = dist hole empty in
    let d2 = dist hole (width-1, 0) in
    let d3 = 5 * (width-2) in
    d1 + d2 + d3
  let solve_part2 () =
    let map, width, height, empty = read_input () in
    let n = shortest_path_opt map width height empty in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)