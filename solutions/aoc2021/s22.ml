open Utils
open Syntax

module S =
struct
  let name = Name.mk "s22"

  (* We use Cartesian products of interval to represent cuboids *)

  module Interval =
  struct
    type t = { inf : int; sup : int } (* [inf, sup)*)
    let of_int i = { inf = i; sup = i + 1 }
    let compare = compare
    let pp fmt t =
      let open Format in
      if t.inf = t.sup - 1 then
        fprintf fmt "%d" t.inf
      else
        fprintf fmt "[%d..%d)" t.inf t.sup
    let check t = if t.inf < t.sup then Some t else None
    let cap t1 t2 =
      check { inf = (max t1.inf t2.inf);
              sup = (min t1.sup t2.sup) }
    let cup t1 t2 =
      let t1, t2 = if t2.inf > t1.inf then t1, t2 else t2, t1 in
      if t1.sup >= t2.inf then
        [{t1 with sup = max t2.sup t1.sup}]
      else
        [t1; t2]

    let diff t1 t2 =
      match
        (check { t1 with sup = min t1.sup t2.inf }),
        (check { t1 with inf = max t1.inf t2.sup })
      with
        Some t, None | None, Some t -> [ t ]
      | None, None -> []
      | Some t1, Some t2 -> cup t1 t2

  end

  module Cube = struct
    type t = { x : Interval.t; y : Interval.t; z : Interval.t; }
    let x_ t = t.x
    let y_ t = t.y
    let z_ t = t.z
    let compare (c1:t) c2 = compare c1 c2

    let pp fmt t =
      Format.fprintf fmt "{ x = %a; y = %a; z = %a }"
        Interval.pp t.x Interval.pp t.y Interval.pp t.z
    let make x1 x2 y1 y2 z1 z2 =
      let open Interval in
      { x = { inf = x1; sup = x2 + 1};
        y = { inf = y1; sup = y2 + 1};
        z = { inf = z1; sup = z2 + 1}; }

    (* Set operations must return disjoint cubes. *)
    let cap cube1 cube2 =
      let open Interval in
      let*& x = cap cube1.x cube2.x in
      let*& y = cap cube1.y cube2.y in
      let*& z = cap cube1.z cube2.z in
      Some {x;y;z} (* None ⇒ empty intersection *)

    let cons lx ly lz acc =
      match lx, ly, lz with
        _::_, _::_, _::_ ->
        List.fold_left (fun acc x ->
            List.fold_left (fun acc y ->
                List.fold_left (fun acc z ->
                    {x;y;z}::acc
                  ) acc lz
              ) acc ly
          ) acc lx
      | _ -> acc

    let icap a b = Option.to_list (Interval.cap a b)
    (* We use the identity
       (X1,Y1)\(X2,Y2) = (X1\X2, Y1) U (X1, Y1\Y2),
       but modified so that the unions are disjoint:
       (X1\X2, Y1&Y2) U (X1&X2, Y1\Y2) U (X1\X2, Y1\Y2)
       and extended to the third dimension (by replacing
       Y1 = (Y1', Z1) and Y2 = (Y2', Z2))
    *)
    let diff ({ x=x1; y=y1; z=z1 } as c1) ({ x=x2; y=y2; z=z2 } as c2) =
      match cap c1 c2 with
        None -> [ c1 ]
      | Some c ->
        let open Interval in
        let ix = [c.x] and dx = diff x1 x2 in
        let iy = [c.y] and dy = diff y1 y2 in
        let iz = [c.z] and dz = diff z1 z2 in
        []
        |> cons dx dy dz |> cons dx dy iz
        |> cons dx iy dz |> cons dx iy iz
        |> cons ix dy dz |> cons ix dy iz
        |> cons ix iy dz

    let[@tail_mod_cons] rec merge_list l1 l2=
      match l1, l2 with
        [], l | l, [] -> l
      | x1 :: ll1, x2 :: ll2 -> x1::x2:: merge_list ll1 ll2

    (* Adds an arbitrary cube to a list of disjoint cubes. *)
    let add l cube1 =
      (* l: list of cubes to test against cube1
         finished: cubes we know cannot intersect cube1
         acc: cubes we have already processed
         todo: cubes that were created by splitting cube1 or
               one of its predecessor and that we need to process
               afterwards. *)
      let rec loop cube1 l finished active todo =
        match l with
          [] -> cube1::finished, active, todo 
        (* we have confronted cube1 against everything, we are done with it *)
        | cube2 :: ll ->
          let active = cube2 :: active in (* cube2 must be kept in the active list*)
          match diff cube1 cube2 with
            [] -> finished, merge_list active ll, todo
          | cube1 :: todo1 ->
            loop cube1 ll finished active (merge_list todo1 todo)
      in
      let rec repeat acc disjoint todo =
        match todo with
          [] -> merge_list acc disjoint
        | cube1 :: todo1 ->
          let ndisjoint, nacc, ntodo = loop cube1 acc disjoint [] todo1 in
          repeat nacc ndisjoint ntodo
      in
      repeat l [] [cube1]

    let reduce l = (* we merge adjacent cuboids to reduce the size of the list *)
      let table = ~%[] in
      let merge_intervals mk key lint acc =
        let rec loop lint acc =
          match lint with
            [] -> acc
          | [ i ] -> (mk key i)::acc
          | i1 :: i2 :: llint ->
            match Interval.cup i1 i2 with
              [ i ] -> loop (i::llint) acc
            | _ -> loop(i2::llint) ((mk key i1)::acc)
        in
        loop (List.sort Interval.compare lint) acc
      in
      let reduce_dir getk geti mk l =
        Hashtbl.clear table;
        List.iter (fun cube ->
            let key = getk cube in
            let int = geti cube in
            table.%{key} <- int :: (table.%?{key} or [])
          )l;
        Hashtbl.fold (merge_intervals mk) table []
      in
      let pair f g = fun x -> (f x, g x) in
      l
      |> reduce_dir (pair x_ y_) z_ (fun (x,y) z -> {x;y;z})
      |> reduce_dir (pair x_ z_) y_ (fun (x,z) y -> {x;y;z})
      |> reduce_dir (pair y_ z_) x_ (fun (y,z) x -> {x;y;z})

    let add l cube1 = reduce (add l cube1)

    (* Removing is easy since diff produces disjoints unions
       and the list l is of disjoints cuboids. *)
    let remove l cube =
      List.fold_left (fun acc c ->
          merge_list (diff c cube) acc) [] l
    let size {x;y;z} =
      let open Interval in
      let sx = x.sup - x.inf in
      let sy = y.sup - y.inf in
      let sz = z.sup - z.inf in
      sx * sy * sz
  end
  let count l =
    l
    |> List.map Cube.size
    |> Iter.(sum list int)
  let bounded_int int ibound =
    match Interval.cap int ibound with
      Some i when i = int -> true
    | _ -> false

  let bounded_cube cube ibound =
    bounded_int cube.Cube.x ibound &&
    bounded_int cube.Cube.y ibound &&
    bounded_int cube.Cube.z ibound

  let solve bound =
    let ibound = Interval.{inf = -bound; sup=bound+1 } in
    Input.fold_scan "%[onf] x=%d..%d,y=%d..%d,z=%d..%d"
      (fun acc s x1 x2 y1 y2 z1 z2 ->
         let cube = Cube.make x1 x2 y1 y2 z1 z2 in
         if bounded_cube cube ibound then
           if s = "on" then Cube.add acc cube
           else Cube.remove acc cube
         else acc) []
    |> count
    |> Solution.printf "%d"

  let solve_part1 () = solve 50
  let solve_part2 () = solve (max_int - 1) 

end
let () = Solution.register_mod (module S)