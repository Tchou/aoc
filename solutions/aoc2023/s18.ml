open Utils
module S =
struct
  let name = Name.mk "s18"
(*
Use the shoelace formula, but tweak it since it does not account
for the borders. For instance:
#######
#.....#
###...#
  #...#
  #...#
  #####
R 6 (#000000)
D 5 (#000000)
L 4 (#000000)
U 3 (#000000)
L 2 (#000000)
U 2 (#000000)

Here, the shoelace computes 24, but the right and bottom borders
are unnaccounted for. To compensate we add half the perimeter + 1.
*)
  let dir_char dir =
    match dir with
    | 'U' -> (0, 1)
    | 'D' -> (0, -1)
    | 'L' -> (-1, 0)
    | 'R' -> (1, 0)
    | _ -> assert false
  let dir_int dir =
    match dir with
    | 0 -> (1, 0)
    | 1 -> (0, -1)
    | 2 -> (-1, 0)
    | 3 -> (0, 1)
    | _ -> assert false

  let load_input () =
    Input.fold_scan "%c %d (#%x)" 
      (fun acc dir len rgb ->
         ((dir_char dir, len), (dir_int (rgb mod 16), rgb / 16))::acc) []

  let advance (x, y) (i, j) len =
    assert (i = 0 || j = 0);
    (x + i*(len), y + j*(len))

  let make_rect take l =
    List.fold_left (fun (accl, accp) e ->
        let d, l = take e in
        match accl with
          p :: _ -> (advance p d l)::accl, accp+l
        | _ -> accl, accp) ([(0,0)],0) l

  let shoelace l =
    let rec loop l ((x2, y2) as last) acc =
      match l with
        [] -> acc
      | [ (x1, y1) ] -> (acc + x1 * y2 - x2 * y1)
      | (x1, y1) :: ((x2, y2) :: _ as ll) ->
        loop ll last (acc + x1 * y2 - x2 * y1)
    in
    (* p0 is already in the list *)
    match l with
      _ :: p1 :: _ -> (abs (loop l p1 0))/2
    | _ -> 0

  let solve take =
    let input = load_input () in
    let rects, perim = make_rect take input in
    let area = shoelace rects in
    Ansi.printf "%d\n" (area + perim / 2 + 1)
  let solve_part1 () = solve fst

  let solve_part2 () = solve snd
end

let () = Solution.register_mod (module S)