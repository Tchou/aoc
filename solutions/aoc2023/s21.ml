open Utils
open Syntax

(*
Tricky... the real input has several key properties (which the example does not have)
- the number of steps is 65 + 202300 * 131.
- it takes 65 steps to go throuh the first grid, then 131 to traverse each grid.
  this is because there are no rocks in line below above left and right of S, and
  there is also an empty border around the grid.


 Then (with some help) we know that the result fits a quadratic equation:
 ax^2 + bx + c

*)

module S =
struct
  let name = Name.mk "s21"

  let load_input () =
    let start = ref (-1, -1) in
    let row = ref 0 in
    let lines = Input.fold_lines (fun acc s ->
        begin match String.index_opt s 'S' with
            Some col -> start := (!row, col)
          | None -> ()
        end;
        incr row;
        s::acc
      ) []
    in
    Array.of_list (List.rev lines), !start

  let dirs = [(0,1); (0,-1); (1,0); (-1, 0)]
  let (+>) (r, c) (i, j) = (r+i, c + j)
  let (.@()) grid (r, c) = grid.(r).[c]
  let is_valid grid (r, c) =
    r >= 0 && c >= 0 && r < Array.length grid &&
    c < String.length grid.(r) && grid.@(r, c) <> '#'

  module PosSet = Set.Make (struct type t = int * int let compare = compare end)
  let rec walk grid n positions =
    if n = 0 then positions else
      let positions' =
        PosSet.fold (fun pos acc ->
            List.fold_left (fun acc d ->
                let pos' = pos +> d in
                if is_valid grid pos' then PosSet.add pos' acc
                else acc) acc dirs)
          positions PosSet.empty
      in
      walk grid (n-1) positions'

  let solve_part1 () =
    let grid, start = load_input () in
    walk grid 64 (PosSet.singleton start)
    |> PosSet.cardinal
    |> Solution.printf "%d"

  let emod a b = ((a  mod b) + b ) mod b
  let ediv a b = if a >= 0 then a / b else ((a+1) /b) - 1

  let count_walk grid n set =
    let height = Array.length grid in
    let width = String.length grid.(0) in
    let rec loop n set =
      if n > 0 then begin
        PosSet.fold (fun pos acc ->
            List.fold_left (fun acc d ->
                let r, c = pos +> d in
                let r_mod = emod r height in
                let c_mod = emod c width in
                if is_valid grid (r_mod, c_mod) then
                  PosSet.add (r,c) acc
                else acc) acc dirs) set PosSet.empty
        |> loop (n-1)
      end
      else set
    in
    loop n set

    let split grid set =
      let table = ~%[] in
      let width = String.length grid.(0) in
      let height = Array.length grid in
      PosSet.iter (fun (r, c) ->
          let rg = ediv r height in
          let cg = ediv c width in
          table.%{rg, cg} <- PosSet.add (r, c) (table.%?{rg, cg} or PosSet.empty)
        ) set(*;
               Hashtbl.iter (fun (i, j) s ->
               Ansi.printf "(%d, %d) => %d\n%!" i j (PosSet.cardinal s))
               table;
               Ansi.printf "---\n%!"*)

  let solve_part2 () =
    let grid, start = load_input () in
    let table = PosSet.singleton start in
    let x1 = 0 in
    let table = count_walk grid 65 table in
    (*let () = split grid table in*)
    let y1 = PosSet.cardinal table in
    let x2 = 1 in
    let table = count_walk grid 131 table in
    (*let () = split grid table in*)
    let y2 = PosSet.cardinal table in
    let x3 = 2 in
    let table = count_walk grid 131 table in
    (*let () = split grid table in*)
    let y3 = PosSet.cardinal table in
    let f x =
      ((x-x2) * (x-x3)) / ((x1-x2) * (x1-x3)) * y1 + ((x-x1) * (x-x3)) / ((x2-x1) * (x2-x3)) * y2 + ((x-x1) * (x-x2)) / ((x3-x1) * (x3-x2)) * y3
    in
    let res0 = f 202300 in
    Solution.printf "%d" res0
end

let () = Solution.register_mod (module S)