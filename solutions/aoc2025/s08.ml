open Utils
open Syntax
module S =
struct
  let name = Name.mk "s08"

  module V3 = struct
    type t = { x : int; y : int ; z : int }

    let sq x = x * x
    let dist2 v1 v2 =
      sq (v1.x - v2.x) + sq (v1.y - v2.y) + sq (v1.z - v2.z)

    let compare v1 v2 = compare v1 v2
    let cmp_dist (v1, v2) (v3, v4) =  compare (dist2 v1 v2) (dist2 v3 v4)
    let mk x y z = { x; y; z}
  end
  let read_input () =
    Input.list_scan "%d,%d,%d" V3.mk

  module UF = (* poor mans' union find *)
  struct
    type t = { count_by_ids : int Array.t; 
               ids : (V3.t, int) Hashtbl.t;
               mapping : int Array.t;
               mutable num_ids : int }

    let mk l =
      let p = List.mapi (fun i v -> (v, i)) l in
      let ids = ~% p in
      let count_by_ids = List.map (fun  (v, _) -> 1) p |> Array.of_list in 
      let len = Array.length count_by_ids in
      { ids; count_by_ids; 
        mapping = Array.init len Fun.id;
        num_ids = len }

    let rec get_id t id =
      let id' = t.(id) in
      if id' = id then id 
      else
        let n = get_id t id' in
        t.(id) <- n; n
    let id t v = get_id t.mapping t.ids.%{v}
    let merge t id1 id2 =
      let l1 = t.count_by_ids.(id1) in
      let l2 = t.count_by_ids.(id2) in
      t.count_by_ids.(id1) <- l1 + l2;
      t.count_by_ids.(id2) <- 0;
      t.num_ids <- t.num_ids - 1;
      t.mapping.(id2) <- id1
    let ids_by_count t =
      let a = Array.copy t.count_by_ids in
      Array.sort (fun n1 n2 -> Int.compare n2 n1) a;
      a

  end
  let prepare_curcuits l =
    let map = UF.mk l in
    let a = Array.of_list l in
    let len = Array.length a in
    let pairs = Array.make ((len*(len-1)) / 2) (a.(0), a.(0)) in
    let k = ref 0 in
    for i = 0 to len - 1 do
      for j = i + 1 to len - 1 do 
        pairs.(!k) <- a.(i), a.(j);
        incr k;
      done;
    done;
    Array.sort V3.cmp_dist pairs;
    map, pairs

  let merge_circuits l =
    let map, pairs = prepare_curcuits l in
    for i = 0 to 999 do
      let v1, v2 = pairs.(i) in
      let id1 = UF.id map v1 in 
      let id2 = UF.id map v2 in
      if id1 <> id2 then UF.merge map id1 id2;
    done;
    let a = UF.ids_by_count map in
    a.(0) * a.(1) * a.(2)


  let merge_until l =
    let map, pairs = prepare_curcuits l in
    let rec loop i =
      let v1, v2 = pairs.(i) in
      let id1 = UF.id map v1 in 
      let id2 = UF.id map v2 in
      if id1 = id2 then loop (i+1) else
        let () = UF.merge map id1 id2 in
        if map.UF.num_ids = 1 then (v1, v2)
        else loop (i+1)
    in
    let V3.{x = x1;_}, V3.{x = x2;  _} = loop 0 in
    x1 * x2

  let solve merge () =
    let l = read_input () in
    let n = merge l in 
    Solution.printf "%d" n
  let solve_part1 = solve merge_circuits  
  let solve_part2 = solve merge_until

end

let () = Solution.register_mod (module S)