open Utils
open Syntax

module S =
struct
  let name = Name.mk "s11"

  let dirs = [
    (-1,-1); (-1,0); (-1, 1);
    (0,-1);          (0, 1);
    (1,-1); (1,0);   (1, 1);
  ]

  let (+>) (r, c) (i, j) = r+i, c+j

  let is_valid grid (r, c) =
    r >= 0 && c >= 0 && r < Array.length grid && c < Bytes.length grid.(r)

  let (.@()) grid (r, c) = grid.(r).$[c]
  let (.@()<-) grid (r, c) v = grid.(r).$[c] <- v


  let count_occuppied_adjacent grid pos =
    List.fold_left (fun acc d ->
        let pos' = pos +> d in
        if is_valid grid pos' && grid.@(pos') = '#' then 1 + acc
        else acc ) 0 dirs

  let rec find_first_in_diagonal grid pos d =
    if is_valid grid pos then
      match grid.@(pos) with
        '#' -> 1
      | 'L' -> 0
      | '.' -> find_first_in_diagonal grid (pos +> d) d
      | _ -> assert false
    else 0
  let count_occuppied_diagonal grid pos =
    List.fold_left (fun acc d ->
        let pos' = pos +> d in
        let n = find_first_in_diagonal grid pos' d in
        acc + n) 0 dirs

  let count_occuppied grid =
    let n = ref 0 in
    for r = 0 to Array.length grid - 1 do
      for c = 0 to Bytes.length grid.(r) - 1 do
        if grid.@(r, c) = '#' then incr n;
      done;
    done;
    !n


  let load_input () =
    Input.fold_lines (fun acc s -> (Bytes.of_string s) :: acc) []
    |> List.rev |> Array.of_list

  let update count_occuppied_adjacent num_adjacents grid dest =
    let untouched = ref 0 in
    let height = Array.length grid in
    let width = Bytes.length grid.(0) in
    for r = 0 to height - 1 do
      for c = 0 to width - 1 do
        let pos = r, c in
        let b = grid.@(pos) in
        if b <> '.' then
          let n = count_occuppied_adjacent grid pos in
          if b = 'L' && n = 0 then dest.@(pos) <- '#'
          else if b = '#' && n >= num_adjacents then dest.@(pos) <- 'L'
          else (dest.@(pos)<-b;incr untouched)
        else incr untouched
      done;
    done;
    !untouched = width * height

  let stabilize count_occuppied_adjacent num_adjacents grid =
    let grid' = Array.map Bytes.copy grid in
    let rec loop grid grid' =
      let stable = update count_occuppied_adjacent num_adjacents grid grid' in
      if not stable then loop grid' grid else
        count_occuppied grid'
    in
    loop grid grid'

  let solve_part1 () =
    load_input ()
    |> stabilize count_occuppied_adjacent 4
    |> Solution.printf "%d"

  let solve_part2 () =
      load_input ()
      |> stabilize count_occuppied_diagonal 5
      |> Solution.printf "%d"
end

let () = Solution.register_mod (module S)