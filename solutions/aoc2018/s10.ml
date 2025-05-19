open Utils
open Syntax
module S =
struct
  let name = Name.mk "s10"

  type point = { p : Grid.position; v : Grid.position }
  let read_input () =
    Input.list_scan "position=< %d, %d> velocity=< %d, %d>"
      (fun x y vx vy -> {p=x, y; v=vx,vy})

  let move point =
    { point with p = Grid.(point.p +! point.v)}

  let test_vertical_align threshold points =
    let cache_x = ~%[] in
    let points' =
      List.fold_left (fun acc ({p=(x,y); _ } as point) ->
          cache_x.%{x} <- 1 + (cache_x.%?{x} or 0);
          (move point)::acc
        ) [] points
    in
    let m = Hashtbl.fold (fun _ v acc -> if v > 4 then acc+v else acc) cache_x 0 in
    (float m > threshold), points'

  let draw_message _ points =
    let x_min = ref max_int in
    let x_max = ref min_int in
    let y_min = ref max_int in
    let y_max = ref min_int in
    let cache = ~%[] in
    List.iter (fun {p=(x, y); _ } ->
        x_min := min x !x_min;
        x_max := max x !x_max;
        y_min := min y !y_min;
        y_max := max y !y_max;
        cache.%{x, y} <- ();
      ) points;
    for y = !y_min to !y_max do
      for x = !x_min to !x_max do
        if cache %? (x, y) then Ansi.(Solution.printf "%a %a" bg green clear color)
        else Solution.printf " "
      done;
      Solution.printf "\n"
    done
  let find_message draw k points =
    let threshold = k *. float (List.length points) in
    let rec loop k points =
      let found, points' = test_vertical_align threshold points in
      if found then draw k points else
        loop (k+1) points'
    in
    loop 0 points

  let solve draw =
    let points = read_input () in
    find_message draw 0.85 points

  let solve_part1 () = solve draw_message

  let solve_part2 () = solve (fun n _ -> Solution.printf "%d" n)
end

let () = Solution.register_mod (module S)