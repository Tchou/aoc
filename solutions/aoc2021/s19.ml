open Utils
open Syntax

module S =
struct
  let name = Name.mk "s19"

  (* basic idea:
    for each sensor s1:
      for each sensor s2:
        get a beacon p1 from s1
        compute relative1, the set of the relative positions
        of beacons of s1 w.r.t to p1
        for each point p2 from s2
          compute relative2, the set of the relative positions
          of beacons of s2 w.r.t to p2
          for each possible symmetry of the cube:
            apply the symmetry to relative2 to obtain tr2
              if the intersection of tr2 and relative1 has more
                than 12 elements, we know that p1 and p2
                represent the same beacon, seen from 2 sensors s1 and s2, break
                we can deduce the position of s2 w.r.t s1 and the transformation to apply.
  *)

  module Point = struct
    type t = { x : int; y : int; z : int }
    let compare = compare
    let zero = {x=0;y=0;z=0}
    let add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y; z = p1.z +p2.z}
    let neg p = {x = -p.x; y = -p.y; z = -p.z }
    let sub p1 p2 = add p1 (neg p2)

    let get o i =
      let ai = abs i in
      let s = i / abs i in
      let v =
        match ai with
          1 -> o.x
        | 2 -> o.y
        | _ -> o.z
      in s * v
    let x90 {x;y;z} = { x; y = z; z = -y }
    let y90 {x;y;z} = { x = -z; y; z = x }
    let z90 {x;y;z} = { x = y; y = -x; z }
    let default_map = { x = -3; y = 2; z = 1}

    (* so that all_transforms.(0) is the identity. *)
    let mk_fun map p =  {x = get p map.x;
                         y = get p map.y;
                         z = get p map.z}
    let cube_rotations default_map =
      let l = ref [] in
      let map = ref default_map in
      let rotations =[|x90; y90|] in
      for _= 0 to 2 do
        for i = 0 to 1 do
          for _ = 0 to 3 do
            map := z90 !map;
            l:= (mk_fun !map) :: !l;
          done;
          map := rotations.(i) !map;
        done;
      done;
      Array.of_list !l

    let all_transforms = cube_rotations default_map

  end
  module PointSet = Set.Make (Point)

  type scanner = { id : int;
                   beacons : PointSet.t }
  let load_input () =
    let scanners, id, _ =
      Input.fold_lines (fun (acc, id, beacons) line ->
          if line = "" then
            {id; beacons}::acc, (-1),PointSet.empty
          else if String.starts_with ~prefix:"---" line then
            Scanf.sscanf line "--- scanner %d ---"
              (fun id -> (acc, id, PointSet.empty))
          else
            Scanf.sscanf line "%d,%d,%d" (fun x y z ->
                (acc, id,
                 PointSet.add Point.{x;y;z} beacons))
        ) ([], -1,PointSet.empty)
    in
    assert (id = -1);
    scanners |> List.rev |> Array.of_list

  let apply l p = List.fold_left (fun acc f -> f acc) p l
  let dfs scanners graph =
    let beacons = ref PointSet.empty in
    let visited = ~%[] in
    let scanner_pos = ref [(0, Point.zero)] in
    let rec loop scanner tr =
      if not (visited %? scanner.id) then begin
        visited.%{scanner.id}<- ();
        PointSet.iter (fun p ->
            let p = apply tr p in
            beacons := PointSet.add p !beacons)
          scanner.beacons;
        match graph.%?{scanner.id} with
          None -> ()
        | Some l ->
          List.iter
            (fun (id, i, delta) ->
               scanner_pos := (id, apply tr delta) :: !scanner_pos;
               let f p = Point.(add (all_transforms.(i) p)) delta in
               loop scanners.(id) (f::tr))
            l
      end
    in
    loop scanners.(0) [];
    !beacons,!scanner_pos

  let solve () =
    let scanners = load_input () in
    let cache = ~%[] in
    let visited = ~%[] in
    let graph = ~%[] in
    let find_common_beacons s1 s2 =
      try
        let b1 = scanners.(s1).beacons in
        b1
        |> PointSet.iter (fun p1 ->
            let relative1 =
              let*% () = cache.%?{s1,p1,0} in
              let r = PointSet.map (Point.sub p1) b1 in
              cache.%{s1,p1,0} <- r;r
            in
            let b2 = scanners.(s2).beacons in
            b2
            |> PointSet.iter (fun p2 ->
                let relative2 =
                  let*% () = cache.%?{s2,p2,0} in
                  let r = PointSet.map (Point.sub p2) b2 in
                  cache.%{s2,p2,0} <- r;r
                in
                for i = 0 to Array.length Point.all_transforms - 1 do
                  let tr_rel2 =
                    let*% () = cache.%?{s2,p2,i} in
                    let r = PointSet.map Point.all_transforms.(i) relative2 in
                    cache.%{s2,p2,i} <- r;r
                  in
                  let s = PointSet.inter relative1 tr_rel2 in
                  let nums = PointSet.cardinal s in
                  if nums >= 12 then begin
                    let tp2 = Point.all_transforms.(i) p2 in
                    let delta = Point.sub p1 tp2 in
                    graph.%{s1} <- (s2, i, delta) :: (graph.%?{s1} or []);
                    (* if I apply transformation i
                               to s2 coordinates and add delta
                               I get s1 coordinates.
                               delta is the s1-coordinates of 2 sensor.
                    *)
                    raise Exit
                  end;
                done));false
      with Exit -> true
    in
    let rec dfs_loop s1 =
      if not (visited %? s1) then begin
        visited.%{s1} <- ();
        for s2 = 0 to Array.length scanners - 1 do
          if s1 <> s2 && not (visited %? s2) then
            if find_common_beacons s1 s2 then
              dfs_loop s2;
        done
      end
    in dfs_loop 0;
    dfs scanners graph

  let solve_part1 () =
    let beacons, _ = solve () in
    Solution.printf "%d" (PointSet.cardinal beacons)
  let solve_part2 () =
    let _, scanner_pos = solve () in
    let max_dist = ref 0 in
    let tab = Array.of_list scanner_pos in
    for i = 0 to Array.length tab - 1 do
      for j = i + 1 to Array.length tab - 1 do
        let d = Point.sub (snd tab.(i)) (snd tab.(j)) in
        max_dist := max !max_dist (abs d.x + abs d.y + abs d.z);
      done
    done;
    Solution.printf "%d" !max_dist
end

let () = Solution.register_mod (module S)