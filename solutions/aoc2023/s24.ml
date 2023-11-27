open Utils
module S =
struct
  let name = Name.mk "s24"

  type t = { x : float; y : float; z : float }
  let load_input () =
    Input.fold_scan "%f, %f, %f @ %f, %f, %f"
      (fun acc x1 y1 z1 x2 y2 z2 ->
         ({x=x1;y=y1;z=z1}, {x=x2;y=y2;z=z2})::acc)
      []
    |> List.rev |> Array.of_list

  (* parametric equations :
        x = x0 + vx0*t
        y = y0 + vy0*t
        z = z0 + vz0*t

        x = x1 + vx1*s
        y = y1 + vy1*s
        z = z1 + vz1*s

     We solve for t:
      x0 + vx0*t = x1 + vx1*s
      y0 + vy0*t = y1 + vy1*s
      z0 + vz0*t = z1 + vz1*s

      t = ((x1 - x0) + vx1*s)/vx0
      t = ((y1 - y0) + vy1*s)/vy0
      t = ((z1 - z0) + vz1*s)/vz0

      => s = [vy0 (x0-x1) - vx0 * (y0-y1)]/(vx1vy0 - vx0vy1)

      s = ((x0 - x1) + vx0*t)/vx1
      s = ((y0 - y1) + vy0*t)/vy1

      => t = [vy1 (x0-x1) - vx1 (y0 - y1)]/(vx1vy0 - vx0 vy1)
  *)
  let pp ppf v =
    Ansi.fprintf ppf "{x=%f; y=%f; z=%f}" v.x v.y v.z

  let solve_eqn
      ({x=x0;y=y0;_}, {x=vx0;y=vy0;_})
      ({x=x1;y=y1;_}, {x=vx1;y=vy1;_}) =
    let d = vx1*.vy0 -. vx0*.vy1 in
    let dx = x0-.x1 in
    let dy = y0 -. y1 in
    let t = (vy1 *. dx -. vx1 *. dy)/. d in
    let s = (vy0 *. dx -. vx0 *. dy)/. d in
    t, s, { x= x0 +. vx0 *. t; y = y0 +. vy0 *. t; z = Float.nan }

  let comp inf sup arr  =
    let count = ref 0 in
    for i = 0 to Array.length arr - 1 do
      for j = i + 1 to Array.length arr - 1 do
        let p0, v0 = arr.(i) in
        let p1, v1 = arr.(j) in
        let t0, s1, i01  = solve_eqn (p0, v0) (p1, v1) in
        let ok =
          Float.is_finite t0 &&
          Float.is_finite s1 &&
          t0 >= 0.0 &&
          s1 >= 0.0 &&
          i01.x >= inf && i01.x <= sup &&
          i01.y >= inf && i01.y <= sup
        in
        if ok then incr count;
        Ansi.printf "(p0=%a, v0=%a) (p1=%a, v1=%a) => t0=%f, s1=%f at %a (%b)\n"
          pp p0 pp v0 pp p1 pp v1
          t0 s1 pp i01 ok;
      done;
    done;
    !count

  let solve_part1 () =
    load_input ()
    |> comp (float 200000000000000) (float 400000000000000)
    |> Ansi.printf "%d\n"

  (* Equation of our rock:
     x = xr t + vxr
     y = yr t + vyr
     z = zr t + vzr

     Consider a hailstone H1:
     x = x1 s + vx1
     y = y1 s + vy1
     z = z1 s + vz1

     Both intersects when

     Vr + R t  = V1 + H1 s
     where R = (x,y,z), Vr = (xr,yr,zr) ...

     We also that t = s since the rock hits the hailstone meaning they are
     at the same place at the same time.

     Vr + R t  = V1 + H1 t
     (Vr - V1) = (H1 - R) t

  *)
  (**** *)
  (* Gauss-Jordan elimination, textbook. *)

  let swap matrix i j =
    let temp = matrix.(i) in
    matrix.(i) <- matrix.(j);
    matrix.(j) <- temp

  let mult matrix i factor =
    for j = 0 to Array.length matrix.(i) - 1 do
      matrix.(i).(j) <- matrix.(i).(j) *. factor
    done

  let add_rows matrix i j factor =
    for k = 0 to Array.length matrix.(i) - 1 do
      matrix.(j).(k) <- matrix.(j).(k) +. factor *. matrix.(i).(k)
    done

  let print_matrix matrix =
    Array.iter (fun row ->
        Array.iter (fun x -> Printf.printf "%8.3f " x) row;
        print_endline ""
      ) matrix

  let gauss_jordan matrix =
    let rows = Array.length matrix in
    let cols = Array.length matrix.(0) in

    for k = 0 to rows - 1 do
      (* Find pivot for column k *)
      let pivot_row = ref k in
      for i = k + 1 to rows - 1 do
        if abs_float matrix.(i).(k) > abs_float matrix.(!pivot_row).(k) then
          pivot_row := i
      done;

      swap matrix k !pivot_row;

      let pivot = matrix.(k).(k) in
      mult matrix k (1.0 /. pivot);

      for i = 0 to rows - 1 do
        if i <> k then
          add_rows matrix k i (-.matrix.(i).(k))
      done;
    done

  (* we only need to solve for three points. If we find 3 points such that the
     rock intersects their trajectories, all others must be alligned since we
     assume the problem to have a single solution.
  *)
  let solve tab =
    let p1, v1 = tab.(0) in
    let p2, v2 = tab.(1) in
    let p3, v3 = tab.(2) in
    let matrix =
      [|
        [| -.(v1.y -. v2.y); v1.x -. v2.x; 0.; p1.y -. p2.y; -.(p1.x -. p2.x); 0.;
           (p1.y *. v1.x -. p2.y *. v2.x) -. (p1.x *. v1.y -. p2.x *. v2.y) |];
        [| -.(v1.y -. v3.y); v1.x -. v3.x; 0.; p1.y -. p3.y; -.(p1.x -. p3.x); 0.;
           (p1.y *. v1.x -. p3.y *. v3.x) -. (p1.x *. v1.y -. p3.x *. v3.y) |];

        [|0.; -.(v1.z -. v2.z); v1.y -. v2.y;  0.; p1.z -. p2.z; -.(p1.y -. p2.y);
          (p1.z *. v1.y -. p2.z *. v2.y) -. (p1.y *. v1.z -. p2.y *. v2.z) |];
        [|0.; -.(v1.z -. v3.z); v1.y -. v3.y;  0.; p1.z -. p3.z; -.(p1.y -. p3.y);
          (p1.z *. v1.y -. p3.z *. v3.y) -. (p1.y *. v1.z -. p3.y *. v3.z); |];

        [|-.(v1.z -. v2.z); 0.; v1.x -. v2.x;  p1.z -. p2.z; 0.; -.(p1.x -. p2.x);
          (p1.z *. v1.x -. p2.z *. v2.x) -. (p1.x *. v1.z -. p2.x *. v2.z)
        |];
        [|-.(v1.z -. v3.z); 0.; v1.x -. v3.x;  p1.z -. p3.z; 0.; -.(p1.x -. p3.x);
          (p1.z *. v1.x -. p3.z *. v3.x) -. (p1.x *. v1.z -. p3.x *. v3.z)
        |]
      |]
    in
    let () = gauss_jordan matrix in
    (* solution are the last column of first 3 rows: *)
    let last row = row.(Array.length row - 1) in
    let s1 = last matrix.(0) in
    let s2 = last matrix.(1) in
    let s3 = last matrix.(2) in
     (s1 +. s2 +. s3)
  let solve_part2 () =
    load_input ()
    |> solve
    |> Ansi.printf "%.0f\n"
end

let () = Solution.register_mod (module S)