open Utils
open Syntax
module S =
struct
  let name = Name.mk "s20"
  module V3 = struct
    type t = { x : Z.t; y : Z.t ; z : Z.t }
    let zero = Z.{ x = zero; y = zero; z = zero}
    let mdist v1 v2 =
      let open Z in
      abs (v1.x - v2.x) + abs (v1.y - v2.y) + abs (v1.z - v2.z)

    let compare v1 v2 =
      let c = Z.compare v1.x v2.x in
      if c <> 0 then c else
        let c = Z.compare v1.y v2.y in
        if c <> 0 then c else
          Z.compare v1.z v2.z

    let pp fmt v = 
      let open Z in Format.printf "<%a,%a,%a>" pp_print v.x pp_print v.y pp_print v.z
  end
  type p = { p : V3.t; v : V3.t ; a : V3.t }
  let read_input () =
    Input.list_scan "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>"
      (fun px py pz vx vy vz ax ay az ->
         let open Z in
         { p = V3.{x= of_int px; y = of_int py; z = of_int pz};
           v = V3.{x= of_int vx; y = of_int vy; z = of_int vz};
           a = V3.{x= of_int ax; y = of_int ay; z = of_int az}}
      ) |> List.mapi (fun i x -> (i, x))

  (*
    If we compute the position as a function of the time:
    v1.x = v0.x + a0.x
    p1.x = p0.x + v0.x + a0.x 

    v2.x = v1.x + a0.x = v0.x + 2a0.x
    p2.x = p1.x + v2.x = p0.x + v0.x + a0.x + v0.x + 2a0.x = p0.x + 2v0.x + 3a0.x

    v3.x = v2.x + a0.x = v0.x + 3a0.x
    p3.x = p2.x + v3.x = p0.x + 2v0.x +3a0.x + v0.x + 3a0.x = p0.x + 3v0.x + 6a0.x

    vt.x = v0.x + ta0.x
    pt.x = p0.x + tv0.x + T(t)a0.x where T(t) is the triangular number: t(t+1)/2
  *)

  let move t o =
    let open Z in
    let tr = (t * (t + one)) / (of_int 2) in
    let v v0 a0 = v0 + t*a0 in
    let p p0 v0 a0 = p0 + t*v0 + tr * a0 in
    V3.{ o with 
         p = { x = p o.p.x o.v.x o.a.x;
               y = p o.p.y o.v.y o.a.y;
               z = p o.p.z o.v.z o.a.z };
         v = { x = v o.v.x o.a.x;
               y = v o.v.y o.a.y;
               z = v o.v.z o.a.z };
       }
  let closest k l =
    let cmp (_, o1) (_, o2) = 
      let m1 = V3.(mdist o1.p zero) in
      let m2 = V3.(mdist o2.p zero) in
      Z.compare m1 m2
    in
    let l = List.map (fun (i, p) -> i, move Z.(of_int k) p) l in
    Iter.(min list ~compare:cmp l)


  let closest l = closest 1_000_000_000 l |> fst
  let solve_part1 () =
    let l = read_input () in
    let n = closest l in
    Solution.printf "%d" n

  (*
  Two particles colide whenever 
   p0.x + tv0.x + (t(t+1)/2)a0.x = p'0.x + tv'0.x + (t(t+1)/2)a'0.x
   ( for .z, .y, .z )

   a0 t^2/2 + a0 t/2 + v0 t + p0

   a t^2/2 + (a/2+v) t + p = a' t^2/2 + (a'/2+v') t + p'

   (a-a')/2 t^2 + ((a-a')/2 + v - v') t + (p-p') = 0

   Δ = (a-a')^2/4(v-v')^2 - 4(a-a')(p-p')/2

   t = -b ± sqrt(Δ) /(a-a')

   We are only interested in solutions where:
   t >= 0
   t is an integer
  *)
  let solve_coord a a' v v' p p' =
    let a = Z.to_float a in
    let a' = Z.to_float a' in
    let v = Z.to_float v in
    let v' = Z.to_float v' in
    let p = Z.to_float p in
    let p' = Z.to_float p' in
    let aa = (a -. a') /. 2.0 in
    let bb = aa +. (v -. v') in
    let cc = p -. p' in
    let res = 
      if aa = 0.0 then if bb = 0.0 then [] else [ (~-.cc /. bb) ] else
        let delta = bb ** 2.0 -. 4.0 *. aa *. cc in
        if delta < 0.0 then []
        else 
          let a2 = aa *. 2.0 in
          if delta = 0.0 then [ ~-. bb /. a2 ]
          else
            let sdeltaf = sqrt delta in
            [
              (~-. bb +. sdeltaf)/. a2 ;
              (~-. bb -. sdeltaf)/. a2
            ]
    in
    List.filter_map (fun f -> if f >= 0.0 && Float.is_integer f
                      then Some (int_of_float f) else None) res

  let (let**) l f =
    match l with
      [] -> ()
    | x -> f x

  let count_collisions l =
    let ll = List.map snd l |> Array.of_list in
    let h_time = ~%[] in
    for i = 0 to Array.length ll - 1 do
      let u = ll.(i) in
      for j = i+1 to Array.length ll - 1 do
        let v = ll.(j) in
        let** lx = solve_coord u.a.x v.a.x u.v.x v.v.x u.p.x v.p.x in
        let** ly = solve_coord u.a.y v.a.y u.v.y v.v.y u.p.y v.p.y in
        let** lz = solve_coord u.a.z v.a.z u.v.z v.v.z u.p.z v.p.z in
        lx |> List.iter (fun tx -> 
            ly |> List.iter (fun ty -> 
                lz |> List.iter (fun tz -> 
                    if tx = ty && ty = tz then
                      h_time.%{tx} <-(i,j)::(h_time.%?{tx} or [])
                  )));
      done;
    done;
    let h = ~%[] in
    let rec loop ls =
      match ls with
        [] -> List.length l - Hashtbl.length h 
      | (t, ll) :: lss ->
        (* Remove particles that disappeared at an earlier time *)
        let ll = List.filter (fun (i, j)-> not (h %? i || h %? j)) ll in
        (* Add the remaining ones to the table of destroyed particles *)
        List.iter (fun (i,j) -> h.%{i} <- (); h.%{j} <- ()) ll;
        loop lss
    in
    loop (Iter.items h_time |> List.of_seq |> List.sort Compare.fst)

  let solve_part2 () =
    let l = read_input () in
    let n = count_collisions l in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)