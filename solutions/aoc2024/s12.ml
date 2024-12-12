open Utils
open Syntax
module S =
struct
  let name = Name.mk "s12"

  let read_input () =
    Input.fold_lines (fun acc e -> e :: acc) []
    |> List.rev
    |> Array.of_list

  let hdirs = [ (1,0); (-1, 0)]
  let vdirs = [ (0, 1); (0, -1)]
  let dirs = vdirs @ hdirs
  let (+!) (a, b) (c, d) = (a+c, b+d)

  let valid grid x y =
    x >= 0 && y >= 0 &&
    let h = Array.length grid in
    y < h &&
    let w = String.length grid.(0) in
    x < w

  let in_plot p1 l =
    l |> List.exists (fun p2 ->
        dirs |> List.exists (fun d ->
            p1 = p2 +! d
          )
      )

  let is_on_side grid x y a =
    not (valid grid x y) || grid.(y).[x] <> a
  let perimeter grid l a =
    l |> List.fold_left  (fun acc p ->
        dirs |> List.fold_left (Agg.Left.sum (fun d ->
            let x, y = p +! d in
            int_of_bool (is_on_side grid x y a)
          )) acc) 0

  (* for the side, find the number of corners, that is the number
     of points that have two neighbors one each vertical/horizontal
     which is not in the surface (external) or which has both
     neighbor in the surface but not the diagonal (internal)

      XXXX        1XX2   1X2X
      X.X         X.X    X X
      XXX         1XX    1X2
        X           X      X
        X           2      X



  *)
  let ext_corners = [ (-1,0), (0, -1); (0,-1), (1, 0); (1,0), (0, 1); (0, 1),(-1, 0)]
  let int_corners = List.map (fun (p1, p2) -> (p1, p2), (p1+! p2)) ext_corners

  let sides grid l a =
    let is_external p (d1, d2) =
      let (x1, y1) as p1 = p +! d1 in
      let (x2, y2) as p2 = p +! d2 in
      is_on_side grid x1 y1 a && is_on_side grid x2 y2 a
    in
    let is_internal p ((d1, d2), d3) =
      let (x1, y1) as p1 = p +! d1 in
      let (x2, y2) as p2 = p +! d2 in
      let (x3, y3) as p3 = p +! d3 in
      not (is_on_side grid x1 y1 a) &&
      not (is_on_side grid x2 y2 a) &&
      ((valid grid x3 y3) && grid.(y3).[x3] <> a)
    in
    let count_corners p test corners =
      corners |> List.fold_left (Agg.Left.sum (fun d -> int_of_bool(test p d))) 0
    in
    l
    |> List.fold_left (Agg.Left.sum (fun p ->
        let ce = count_corners p is_external ext_corners in
        let ci = count_corners p is_internal int_corners in
        ci + ce
      )) 0


  let map_plots grid =
    let map = ~%[] in
    for y = 0 to Array.length grid - 1 do
      for x = 0 to String.length grid.(y) - 1 do
        let c = grid.(y).[x] in
        let candidates = map.%?{c} or [] in
        let all_in, others = List.partition (in_plot (x, y)) candidates in
        map.%{c} <- ((x,y)::(List.concat all_in))::others
      done
    done;
    map

  let count_plots perim grid map =
    Hashtbl.fold (fun a ll acc ->
        ll
        |> List.fold_left (Agg.Left.sum (fun l ->
            (perim grid l a) * (List.length l)
          )) acc
      ) map 0

  let solve perim =
    let grid = read_input () in
    let map = map_plots grid in
    let n = count_plots perim grid map in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
  let solve_part1 () =
    solve perimeter
  let solve_part2 () =
    solve sides

end

let () = Solution.register_mod (module S)