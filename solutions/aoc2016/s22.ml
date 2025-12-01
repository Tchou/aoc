open Utils
open Syntax

module S =
struct
  let name = Name.mk "s22"

  module Config =
  struct
    open Grid
    type t = { width : int; height : int; data : StringGrid.t }
    type info = { empty : int; movable : int; fixed : int }
    let of_table w h t =
      let e = ref 0 in
      let m = ref 0 in
      let f = ref 0 in
      let g =
        StringGrid.init h (fun y ->
            String.init w (fun x ->
                let used, _ = t.%{x, y} in
                if used = 0 then (incr e;' ') else
                if List.exists (fun (i, j) ->
                    let _, total = t.%?{x+i, y+j} or (0,min_int) in
                    if used <= total then
                      (if used >= 100 then Printf.printf "%d, %d, %d, %d, %d, %d\n"
                           x y i j used total;
                       true)
                    else false
                  ) Grid.dir4 then (incr m;'.') else (incr f;'#')
              ))
      in
      g, { empty = !e; movable = !m; fixed = !f }
    let pp fmt = StringGrid.iter_lines (Format.fprintf fmt "%s\n")
  end
  let read_input () =
    Input.read_line () |> ignore;
    Input.read_line () |> ignore;
    let max_x = ref min_int in
    let max_y = ref min_int in
    let t = ~%[] in 
    let g = Input.fold_scan "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%"
        (fun () x y total used _avail _usep -> 
           t.%{x, y} <- (used, total);
           max_x := max !max_x x;
           max_y := max !max_y y;
        ) ()
    in
    Config.of_table (!max_x + 1) (!max_y + 1) t

  let solve_part1 () =
    let g, info = read_input () in
    let n = Config.((info.movable - info.empty) * (info.movable-1)) in
    Format.printf "%d, %d, %d\n%!" info.empty info.movable info.fixed; 
    Format.printf "%a\n" Config.pp g;
    Solution.printf "%d\n" n

  let solve_part2 () = ()
  (*  type data = { id : int; used : int }
      type node = { data : data; avail : int }


      module Config =
      struct
      type 'a h = { hash : int; data : 'a }
      type line = node array h
      type v = { hole : int * int; grid : line array h }
      type t = v
      let iter_vertices _ _ = assert false

      let equal_line l1 l2 =
        l1 == l2 || begin
          l1.hash == l2.hash &&
          Array.for_all2 (=) l1.data l2.data
        end
      let equal t1 t2 =
        t1 == t2 || begin
          t1.hole = t2.hole && 
          t1.grid.hash == t2.grid.hash &&
          Array.for_all2 equal_line t1.grid.data t2.grid.data
        end

      let hash { hole = (x, y); grid } = x + (y lsl 4) + y + (grid.hash lsl 8 + grid.hash)

      let get t (x, y) =
        t.grid.data.(x).data.(y)

      let set t (x, y) n =
        let ngrid = Array.copy t.grid.data in
        let nline = Array.copy ngrid.(x).data in
        nline.(y) <- n;
        let nhline = { hash = Hashtbl.hash nline; data = nline } in
        ngrid.(x) <- nhline;
        let hash = Array.fold_left (fun acc nh -> acc+(nh.hash lsl 4)+nh.hash) 0 ngrid in
        let hole = if n.data.used = 0 then (x, y) else t.hole in
        { hole; grid = { hash; data = ngrid } }

      let dummy_data = { id = -1; used = -1 }
      let dummy_node = { avail = -1; data = dummy_data }
      let create w h =
        let ngrid = Array.init w (fun _ -> { hash = -1; data = Array.make h dummy_node }) in
        let hngrid = { hash = -1; data = ngrid } in
        { hole = (-1, -1); grid = hngrid }

      let swap_with_hole t other_pos =
        let hole_node = get t t.hole in
        let other_node = get t other_pos in
        if hole_node.avail < other_node.data.used then None
        else
          let hole_node' = { avail = hole_node.avail + hole_node.data.used - other_node.data.used; 
                             data = other_node.data } in
          let other_node' = { avail = other_node.avail + other_node.data.used - hole_node.data.used;
                              data = hole_node.data} in
          Some (set (set t other_pos other_node') t.hole hole_node')

      let width t = Array.length t.grid.data
      let height t = Array.length t.grid.data.(0).data

      let iter_next f t =
        let w = width t in
        let h = height t in
        Grid.dir4 |> List.iter (fun d ->
            let (x, y) as pos = Grid.(t.hole +! d) in
            if x >= 0 && x < w && y >= 0 && y < h then
              Option.iter f (swap_with_hole t pos)
          )

      let iter_succ (_g:t) conf f =
        iter_next (fun v -> f (v, 1)) conf
      end


      let read_input () =
      Input.read_line () |> ignore;
      Input.read_line () |> ignore;
      let hole = ref (-1, -1) in
      let max_x = ref min_int in
      let max_y = ref min_int in
      let g = Input.fold_scan "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%"
          (fun acc x y _total used avail _usep -> 
             let id = (x lsl 16) lor y in
             max_x := max !max_x x;
             max_y := max !max_y y;
             ((x, y),{ data = { id; used }; avail }):: acc) []
      in
      let config = Config.create (!max_x + 1) (!max_y + 1) in
      List.fold_left (fun acc (pos, node) -> Config.set acc pos node) config g

      let is_viable (a,b) =
      (a.data.used > 0) &&
      (a.data.used <= b.avail)

      let enum_nodes a =  
      let w = Config.width a in
      let h = Config.height a in
      let rec loop_y i j () = 
        if j < h then  Seq.Cons(Config.get a (i, j), loop_y i (j+1))
        else loop_x (i+1) ()
      and loop_x i () =
        if i < w then loop_y i 0 ()
        else
          Seq.Nil
      in
      loop_x 0

      let count_viable g =
      g 
      |> Iter.(pairs enum_nodes ~refl:false)
      |> Iter.(count_if Fun.id is_viable)

      let solve_part1 () =
      let config = read_input () in
      let n = count_viable config in
      Solution.printf "%d" n

      let rec hash_seq acc n s =
      if n = 0 then acc else
        match s () with
          Seq.Nil -> acc
        | Seq.Cons(x, ss) ->
          hash_seq (acc + Hashtbl.hash x) (n-1) ss

      module H = Hashtbl.Make (Config)


      let mk_id x y = (x lsl 16) lor y
      let pp fmt config =
      let w = Config.width config in
      let h = Config.height config in
      let final_id = mk_id (w-1) 0 in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let node = Config.get config (x, y) in
          if (x, y) = config.hole then Format.printf "_ "
          else if (mk_id x y) = final_id then Format.printf "G "
          else Format.printf ". "
        done;
        Format.printf "\n%!"
      done;
      Format.printf "--\n%!"

      let bfs start_config final =
      let w = Config.width start_config in
      let queue = Queue.create () in
      let max_round = ref min_int in
      let () = Queue.add (0,start_config) queue in
      let visited = H.create 16 in
      let () = H.add visited start_config () in
      let rec loop () =
        if Queue.is_empty queue then -1 else
          let round, conf = Queue.pop queue in
          let d = Config.get conf (0,0) in
          if conf.hole = final then round
          else
            let () = if round > !max_round then begin
                Format.printf "ROUND: %d\n%!" round;
                max_round := round;
              end in
            let () = if false then Format.printf "ROUND: %d\n%a\n" round pp conf in
            let () =
              conf |> Config.iter_next (fun conf' ->
                  if not (H.mem visited conf') then 
                    let () = H.add visited conf' () in
                    Queue.add (round+1, conf') queue
                )
            in loop ()
      in
      loop ()

      let dfs start_config final =
      let visited = H.create 16 in
      let min_round = ref max_int in
      let rec loop round config =
        if config.Config.hole = final then begin
          min_round := min round !min_round;
          Format.printf "Found new solution of length %d/%d\n%!" round !min_round;      
        end
        else if round < !min_round then
          config |> Config.iter_next (fun conf' ->
              if not (H.mem visited conf') then begin
                H.add visited conf' ();
                loop (round+1) conf';
                H.remove visited conf'
              end
            )
      in
      loop 0 start_config;
      !min_round

      let pp_pair fmt (x, y) = Format.fprintf fmt "(%d, %d)" x y
      let find start final =
      let is_final conf = 
        let () = if false then Format.printf "HOLE AT: %a vs %a\n%!" pp_pair conf.Config.hole pp_pair final in
        conf.Config.hole = final in
      let module G = GraphAlgo(Config) in
      let h config =
        let x, y = config.Config.hole in
        let x0, y0 = final in
        abs ((x0 - x)) + abs (y0 - y)
      in
      let path = G.astar (module H) ~h start start is_final in
      List.length path



      let solve_part2 () =
      let config = read_input () in
      let w = Config.width config in
      let final1 = (w-2, 0) in
      let final2 =  (w-1, 1) in
      let n1 = find config final1 in
      let n2 = find config final2 in
      Solution.printf "%d, %d\n" n1 n2
  *)
end

let () = Solution.register_mod (module S)