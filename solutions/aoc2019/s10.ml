open Utils
module S =
struct
  let name = Name.mk "s10"

  let read_input () =
    Input.fold_lines (fun acc s -> s::acc) []
    |> List.rev
    |> Array.of_list


  (* simple slope equality, rewritten to avoid division by 0
     and to ensure we only check points in the same direction.
  *)
  let colinear (x0, y0) (x, y) (xo, yo) =
    (y - y0) * (xo - x) = (yo - y) * (x - x0) &&
    (x-x0) * (xo-x0) >= 0 &&
    (y-y0) * (yo-y0) >= 0

  let remove_colinears (x0, y0) other_points =
    let rec loop l =
      match l with
        [] -> []
      | (x, y):: ll ->
        let fll = List.filter (fun p -> not (colinear (x0, y0) (x, y) p)) ll in
        (x, y)::loop fll
    in
    loop other_points
  let count_max_visible grid =
    let points = ref [] in
    for y = 0 to Array.length grid - 1 do
      for x = 0 to String.length grid.(0) -1 do
        if grid.(y).[x] = '#' then points := (x, y)::!points
      done;
    done;
    let max_count = ref 0 in
    let max_coords = ref (-1, -1) in
    let points = List.rev !points in
    points
    |> List.iter (fun (x0, y0) ->
        let sorted_points =
          List.sort (fun (x1, y1) (x2, y2) ->
              compare (abs (x1 - x0) + abs (y1 - y0))
                (abs (x2 - x0) + abs (y2 - y0))
            ) points
        in
        let remaining = remove_colinears (x0, y0) (List.tl sorted_points) in
        let num_visible = List.length remaining in
        if num_visible > !max_count then begin
          max_count := num_visible;
          max_coords := (x0, y0);
        end
      );
    !max_count, !max_coords, points


  let solve_part1 () =
    let grid = read_input () in
    let n, _, _ = count_max_visible grid in
    Solution.printf "%d" n

  (* Angle between V1 and V2, with origin V0,
     clockwise in degrees.
  *)
  let angle (x0, y0) (x1, y1) (x2, y2) =
    let xv1 = float (x1 - x0) in
    let xv2 = float (x2 - x0) in
    let yv1 = float (y1 - y0) in
    let yv2 = float (y2 - y0) in
    let ar = atan2 (xv1 *. yv2 -. xv2 *. yv1) (xv1 *. xv2 +. yv1 *. yv2) in
    mod_float (360.0 -. (ar *. -180. /. Float.pi)) 360.0

  let norm (x0, y0) (x1, y1) =
    let xv1 = float (x1 - x0) in
    let yv1 = float (y1 - y0) in
    sqrt (xv1 *. xv1 +. yv1 *. yv1)

  let vaporize round ((x0, y0) as origin) all_points =
    let sort_points points =
      all_points
      |> List.map (fun p -> (angle origin (x0, y0-1) p, norm origin p), p)
      |> List.sort Compare.fst
      |> List.map snd
    in
    let sorted_points = match sort_points all_points with
        [] -> assert false
      | p :: ll -> assert (p = origin); ll
    in
    let rec loop points i acc =
      match points, acc with
      | [], [] -> (-1, -1)
      | [], _ -> loop (sort_points acc) i []
      | (x, y):: ll, _ ->
        if i = round then (x, y)
        else
          let others, fll = List.partition (colinear origin (x,y)) ll in
          loop fll (i+1) (List.rev_append others acc)
    in
    loop sorted_points 1 []

  let solve_part2 () =
    let grid = read_input () in
    let _, ((x0, y0) as origin), all_points = count_max_visible grid in
    let x, y = vaporize 200 origin all_points in
    Solution.printf "%d" (x*100+y)

end

let () = Solution.register_mod (module S)