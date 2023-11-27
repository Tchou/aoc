open Utils
open Syntax
module S =
struct
  let name = Name.mk "s10"

  module Maze = struct
    type t = bytes array
    let north = (-1, 0)
    let south = (1, 0)
    let east = (0, 1)
    let west = (0, -1)
    let dir_of_pipe = function
        '|' -> [ north; south ]
      | '-' -> [ west; east ]
      | 'L' -> [ east; north ]
      | 'J' -> [ west; north ]
      | '7' -> [ west; south ]
      | 'F' -> [ east; south ]
      | '.' -> []
      | c -> failwith (Format.sprintf "GOT: %c\n" c)
    let all_dirs = [north;east;south;west]
    let oposite_dir (i, j) = (-i, -j)
    let follow_pipe p d =
      let dop = oposite_dir d in
      match dir_of_pipe p with
        [d1; d2] -> if dop = d1 then d2 else d1
      | _ -> assert false
    let (+>) (r, c) (i, j) = (r+i, c+j)
    let valid_coord grid r c =
      r >= 0 && c >= 0 &&
      r < Array.length grid &&
      c < String.length (grid.(r))
    let (.@()) grid (r, c) = grid.(r).[c]
    let guess_start_dir grid (r, c) =
      all_dirs
      |> List.filter_map (fun d ->
          let ri, cj = (r,c) +> d in
          if valid_coord grid ri cj then
            let p = grid.(ri).[cj] in
            let dnext = dir_of_pipe p in
            let dop = oposite_dir d in
            if List.exists (fun ddn -> ddn = dop) dnext then
              Some d
            else None
          else None)

    let load () =
      let start = ref (-1, -1) in
      let maze,_ =
        Input.fold_lines (fun (acc,row) line ->
            let () =
              match String.index_opt line 'S' with
                None -> ()
              | Some col -> start := (row, col)
            in
            ((line)::acc, row + 1)) ([], 0)
      in
      !start, (Array.of_list (List.rev maze))

    let sym_dir dir odir =
      if dir = north then '^'
      else if dir = south then 'V'
      else if odir = north then '^'
      else if odir = south then 'V'
      else '#'

    let simplify maze start =
      let out_maze = Array.map (fun s ->
          Bytes.make (String.length s) '.') maze
      in
      let rec loop n ((r,c) as cell) dir odir =
        out_maze.(r).$[c] <- sym_dir dir odir;
        let ncell = cell +> dir in
        if ncell <> start then
          let pipe = maze.@(ncell) in
          let ndir = follow_pipe pipe dir in
          loop (n+1) ncell ndir dir
        else n
      in
      match guess_start_dir maze start with
        d1::d2::_ ->
        let n = loop 0 start d1 (oposite_dir d2) in
        (n+1)/2, Array.map Bytes.to_string out_maze
      | _ -> assert false

    let count_cells maze =
      let count = ref 0 in
      let prev_sym = ref 'X' in
      let map = ~%[] in
      let inside = ref false in
      for r = 0 to Array.length maze - 1 do
        let row = maze.(r) in
        let () =
          inside := false;
          match String.(index_opt row '^', index_opt row 'V') with
            Some _, None -> prev_sym := 'V'
          | None, Some _ -> prev_sym := '^'
          | Some i1, Some i2 -> prev_sym := if i1 < i2 then 'V' else '^'
          | _ -> ()
        in
        if !prev_sym != 'X' then
          for c = 0 to String.length row - 1 do
            match row.[c] with
              ('V' | '^') as s when s != !prev_sym ->
              prev_sym := s;
              inside := not !inside;
            | '.' when !inside -> incr count; map.%{r,c} <-()
            | _ -> ()
          done;
      done;
      !count, map
  end
  let solve_part1 () =
    let start, maze = Maze.load () in
    let n,_ = Maze.simplify maze start in
    Ansi.printf "%d\n" n

  let solve_part2 () =
    let start, maze = Maze.load () in
    let _, out_maze = Maze.simplify maze start in
    let n, map = Maze.count_cells out_maze in
    Array.iteri (fun r l ->
        String.iteri (fun c s ->
            if map %? (r,c) then
              Ansi.(printf "%a*%a" bfg cyan clear color)
            else
              Ansi.printf "%c" s
          ) l;
        Ansi.printf "\n"
      ) out_maze;
    Ansi.printf "%d\n" n
end

let () = Solution.register_mod (module S)