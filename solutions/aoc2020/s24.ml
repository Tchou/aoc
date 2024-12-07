open Utils
open Syntax
module S =
struct
  let name = Name.mk "s24"
  (* https://en.wikipedia.org/wiki/Hexagonal_Efficient_Coordinate_System *)

  module HCES =
  struct
    type dir = E | SE | SW | W | NW | NE

    let opposite = function
        E -> W
      | SE -> NW
      | SW -> NE
      | W -> E
      | NW -> SE
      | NE -> SW

    let decode txt i =
      match txt.[i] with
      'e' -> E, i+1
      | 'w' -> W, i+1
      | 's' -> if txt.[i+1] = 'w' then SW, i+2 else SE, i+2
      | 'n' -> if txt.[i+1] = 'w' then NW, i+2 else NE, i+2
      | _ -> failwith "decode"

    type t = int * int * int

    let move (a, r, c) dir =
      match dir with
        E -> (a, r, c+1)
      | SE -> (1-a, r+a, c+a)
      | SW -> (1-a, r+a, c-(1-a))
      | W -> (a, r, c-1)
      | NW -> (1-a, r - (1-a), c-(1-a))
      | NE -> (1-a, r - (1-a), c+a)

    let all_dirs = [ E; SE; SW; W; NW; NE ]
  end

  let read_dirs s =
    let rec loop len i acc =
      if i >= len then List.rev acc
      else
        let d, i = HCES.decode s i in
        loop len i (d::acc)
    in
    loop (String.length s) 0 []

  let read_input () =
    Input.fold_lines (fun acc s ->
        (read_dirs s) ::acc) []
    |> List.rev

  let flip_tiles dirs =
    let floor = ~%[] in
    let rec loop l =
      match l with
        [] -> ()
      | moves :: ll ->
        let dest = List.fold_left HCES.move (0,0,0) moves in
        floor.%{dest} <- not (floor.%?{dest} or false);
        loop ll
    in
    loop dirs;
    floor

  let evolve floor =
    let new_floor = ~%[] in
    let todo_white_tiles = ~%[] in
    let count_black_around mark_white coord =
      List.fold_left (Agg.Left.sum (fun d ->
          let n = HCES.move coord d in
          let col_n =
            if not (floor %? n) then begin
              if mark_white then todo_white_tiles.%{n} <- ();
              false
            end else floor.%{n}
          in
          int_of_bool col_n
        )) 0 HCES.all_dirs
    in
    Hashtbl.iter (fun coord black ->
        if black then
          let nn_black = count_black_around true coord in
          new_floor.%{coord} <- nn_black = 1 || nn_black = 2
        else
          let nn_black = count_black_around false coord in
          new_floor.%{coord} <- nn_black = 2
      ) floor;
    Hashtbl.iter (fun coord () ->
        assert (not (new_floor %? coord));
        let nn_black = count_black_around false coord in
        new_floor.%{coord} <- nn_black = 2
      ) todo_white_tiles;
    new_floor

  let count_black_tiles floor =
    Hashtbl.fold (fun _ b acc -> acc + int_of_bool b) floor 0

  let evolve_days n floor =
    let floor = ref floor in
    for _ = 1 to n do
      let f = evolve !floor in
      floor := f;
    done;
    !floor
  let solve part2 =
    let dirs = read_input () in
    let floor = flip_tiles dirs in
    let floor = if part2 then evolve_days 100 floor else floor in
    let n = count_black_tiles floor in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part1 () = solve false
  let solve_part2 () = solve true

end

let () = Solution.register_mod (module S)