open Utils
open Syntax
module S =
struct
  let name = Name.mk "s12"
  module G = Grid.StringGrid

  let hdirs = Grid.[east; west ]
  let vdirs = Grid.[south; north]

  let in_plot p1 l =
    l |> List.exists (fun p2 ->
        Grid.(dir4 |> List.exists (fun d ->
            p1 = p2 +! d
          ))
      )

  let is_on_side grid p a =
    not (G.inside grid p) || grid.G.!(p) <> a
  let perimeter grid l a =
    l |> List.fold_left  (fun acc p ->
        Grid.dir4 |> List.fold_left (Agg.Left.sum (fun d ->
            let q = Grid.(p +! d) in
            int_of_bool (is_on_side grid q a)
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
  let ext_corners = Grid.[ west,north; north, east; east, south; south, west ]
  let int_corners = List.map (fun (p1, p2) -> (p1, p2), Grid.(p1+! p2)) ext_corners

  let sides grid l a =
    let open Grid in
    let is_external p (d1, d2) =
      let p1 = p +! d1 in
      let p2 = p +! d2 in
      is_on_side grid p1 a && is_on_side grid p2 a
    in
    let is_internal p ((d1, d2), d3) =
      let p1 = p +! d1 in
      let p2 = p +! d2 in
      let p3 = p +! d3 in
      not (is_on_side grid p1 a) &&
      not (is_on_side grid p2 a) &&
      ((G.inside grid p3) && grid.G.!(p3) <> a)
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
    grid
    |> G.iter (fun p c ->
        let candidates = map.%?{c} or [] in
        let all_in, others = List.partition (in_plot p) candidates in
        map.%{c} <- (p::(List.concat all_in))::others
      );
    map

  let count_plots perim grid map =
    Hashtbl.fold (fun a ll acc ->
        ll
        |> List.fold_left (Agg.Left.sum (fun l ->
            (perim grid l a) * (List.length l)
          )) acc
      ) map 0

  let solve perim =
    let grid = G.read () in
    let map = map_plots grid in
    let n = count_plots perim grid map in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
  let solve_part1 () =
    solve perimeter
  let solve_part2 () =
    solve sides

end

let () = Solution.register_mod (module S)