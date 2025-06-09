open Utils
module S =
struct
  let name = Name.mk "s22"

  type data = { id : int; used : int }
  type node = { data : data; avail : int }


  module Config =
  struct
    type 'a h = { hash : int; data : 'a }
    type line = node array h
    type t = { hole : int * int; grid : line array h }

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

  end


  module Grd = Map.Make(struct type t = int * int let compare = compare end)
  type grid = node Grd.t

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
           if used = 0 then hole := (x, y);
           Grd.add (x, y) { data = { id; used }; avail } acc) Grd.empty
    in
    !hole, !max_x + 1, !max_y + 1, g

  let is_viable (a,b) =
    (a.data.used > 0) &&
    (a.data.used <= b.avail)

  let enum_nodes a =  
    let w = Array.length a in
    let h = Array.length a.(0) in
    let rec loop_y i a_i j () = 
      if j < h then  Seq.Cons(a_i.(j), loop_y i a_i (j+1))
      else loop_x (i+1) ()
    and loop_x i () =
      if i < w then loop_y i a.(i) 0 ()
      else
        Seq.Nil
    in
    loop_x 0

  let count_viable g =
    g 
    |> Grd.to_seq
    |> Seq.map snd
    |> Iter.(pairs Fun.id ~refl:false)
    |> Iter.(count_if Fun.id is_viable)

  let solve_part1 () =
    let _hole,_w,_h, grid = read_input () in
    let n = count_viable grid in
    Solution.printf "%d" n

  type config = (int * int) * grid * int * int

  let rec hash_seq acc n s =
    if n = 0 then acc else
      match s () with
        Seq.Nil -> acc
      | Seq.Cons(x, ss) ->
        hash_seq (acc + Hashtbl.hash x) (n-1) ss

  module H = Hashtbl.Make (struct
      type t = config
      let hash (p, g, _, _) =
        hash_seq (Hashtbl.hash p) 64 (Grd.to_seq g)

      let equal (p1, g1, _, _) (p2, g2, _, _) =
        (p1 = p2) && Grd.equal (=) g1 g2
    end)


  let swap n1 n2 =
    let m1 = { avail = n1.avail + n1.data.used - n2.data.used; data=n2.data } in
    let m2 = { avail = n2.avail + n2.data.used - n1.data.used; data=n1.data } in
    m1, m2

  let iter_next f (hole, grid, w, h) =
    Grid.dir4 |> List.iter (fun p ->
        let (x, y) as new_hole = Grid.(hole +! p) in
        if x >= 0 && x < w && y >= 0 && y < h then
          let hole_node = Grd.find hole grid in
          let new_node = Grd.find new_hole grid in
          if new_node.data.used <= hole_node.avail then
            let grid = Grd.remove hole (Grd.remove new_hole grid) in
            let hole_node', new_node' = swap hole_node new_node in
            let grid = Grd.add hole hole_node'
                (Grd.add new_hole new_node' grid) in
            f (new_hole, grid, w, h)
      )

  let mk_id x y = (x lsl 16) lor y
  let pp fmt (hole, grid, w, h) =
    let final_id = mk_id (w-1) 0 in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let node = Grd.find (x, y) grid in
        if (x, y) = hole then Format.printf "_ "
        else if (mk_id x y) = final_id then Format.printf "G "
        else Format.printf ". "
      done;
      Format.printf "\n%!"
    done;
    Format.printf "--\n%!"

  let bfs hole w h grid =
    let final_id = (Grd.find (w-1, 0) grid).data.id in
    let init = hole, grid, w, h in
    let queue = Queue.create () in
    let max_round = ref min_int in
    let () = Queue.add (0,init) queue in
    let visited = H.create 16 in
    let () = H.add visited init () in
    let rec loop () =
      if Queue.is_empty queue then -1 else
        let round, ((_, grid, _, _) as conf) = Queue.pop queue in
        let d = Grd.find (0,0) grid in
        if d.data.id = final_id then round
        else
          let () = if round > !max_round then begin
              Format.printf "ROUND: %d\n%!" round;
              max_round := round;
            end in
          let () = if false then Format.printf "ROUND: %d\n%a\n" round pp conf in
          let () =
            conf |> iter_next (fun conf' ->
                if not (H.mem visited conf') then 
                  let () = H.add visited conf' () in
                  Queue.add (round+1, conf') queue
              )
          in loop ()
    in
    loop ()

  let solve_part2 () =
    let hole,w,h, grid = read_input () in
    let n = bfs hole w h grid in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)