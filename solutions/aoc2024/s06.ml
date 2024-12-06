open Utils
open Syntax
module S =
struct
  let name = Name.mk "s06"

  let rec find_char s l =
    match l with
      [] -> None
    | c :: ll -> match String.index_opt s c with
        Some i -> Some i
      | None -> find_char s ll

  let read_input () =
    let start = ref (0,0) in
    let y = ref 0 in
    let grid = Input.fold_lines (fun acc line ->
        let () =
          match String.index_opt line '^' with
            Some x -> start := (x, !y)
          | None -> ()
        in incr y;
        line::acc
      ) []
               |> List.rev
               |> Array.of_list
    in
    !start, grid
  let valid grid x y =
    let h = Array.length grid in
    let w = String.length grid.(0) in
    x >= 0 && x < w && y >= 0 && y < h

  let print_grid fmt (grid, visited) =
    Array.iteri (fun j s ->
        String.iteri (fun i c ->
            let c = if visited %? (i, j) then 'X' else c in
            Format.fprintf fmt "%c" c
          ) s;
        Format.fprintf fmt "\n") grid

  let walk start grid =
    let x, y = start in
    let visited = ~%[] in
    let rec loop i j x y =
      visited.%{x, y} <- ();
      let xi = x + i in
      let yj = y + j in
      if not (valid grid xi yj) then begin
        Format.printf "%a\n%!" print_grid (grid, visited);
        Hashtbl.length visited
      end
      else
        let i, j, x, y =
          if grid.(yj).[xi] = '#' then
            -j, i, x, y
          else i, j,xi, yj
        in
        loop i j x y
    in
    loop 0 (-1) x y

  let is_cycle stone start grid =
    let stone_x, stone_y = stone in
    let x, y = start in
    let visited = ~%[] in
    let rec loop i j x y =
      let key = x, y, i, j in
      if visited %? key then true, visited
      else begin
        visited.%{key} <- ();
        let xi = x + i in
        let yj = y + j in
        if not (valid grid xi yj) then false, visited
        else
          let i, j, x, y =
            if (xi = stone_x && yj = stone_y) || grid.(yj).[xi] = '#' then
              -j, i, x, y
            else i, j, xi, yj
          in
          loop i j x y
      end
    in
    loop 0 (-1) x y

  let count_cycles start grid =
    let h = Array.length grid in
    let w = String.length grid.(0) in
    let clean_path = ~%[] in
    let dirs = [ (1, 0); (-1, 0); (0, 1);(0, -1)] in
    let count = ref 0 in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        if grid.(y).[x] = '.' && (
            Hashtbl.length clean_path = 0 ||
            List.exists (fun (i, j) -> clean_path %? (x, y, i, j)) dirs)
        then
          let b, path = is_cycle (x, y) start grid in
          if b then incr count
          else if Hashtbl.length clean_path = 0 &&
                  not (List.exists (fun (i, j) -> clean_path %? (x, y, i, j)) dirs)
          then
            Hashtbl.iter (fun key () -> clean_path.%{key} <- ()) path
      done;
    done;
    !count

  let solve_part1 () =
    let start, grid = read_input () in
    let n = walk start grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
  let solve_part2 () =
    let start, grid = read_input () in
    let n = count_cycles start grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

end

let () = Solution.register_mod (module S)