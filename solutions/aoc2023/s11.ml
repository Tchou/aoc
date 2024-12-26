open Utils
open Syntax

module S =
struct
  let name = Name.mk "s11"

  let expand table len factor =
    let distances = Array.make len 0 in
    let total = ref 0 in
    for i = 0 to len - 1 do
      distances.(i) <- !total;
      total := !total + if table %? i then 1 else factor;
    done;
    distances

  let load_input factor =
    let rows = ~%[] in
    let columns = ~%[] in
    let galaxies = ref [] in
    let num_cols = ref (-1) in
    let num_rows =
      Input.fold_lines (fun row line ->
          if !num_cols < 0 then
            num_cols := String.length line;
          String.iteri (fun col c ->
              if c = '#' then begin
                galaxies := (row,col) :: !galaxies;
                rows.%{row} <- ();
                columns.%{col} <- ();
              end;
            ) line;
          (row+1)
        ) 0
    in
    let rows = expand rows num_rows factor in
    let columns = expand columns !num_cols factor in
    Array.of_list !galaxies,
    rows,
    columns

  let compute_distances (galaxies, rows, columns) =
    let total = ref 0 in
    let len = Array.length galaxies in
    for i = 0 to len - 1 do
      let r1, c1 = galaxies.(i) in
      for j = i+1 to len - 1 do
        let r2, c2 = galaxies.(j) in
        let dr = rows.(r1) - rows.(r2) in
        let dc = columns.(c1) - columns.(c2) in
        total := !total + (abs dr) + abs (dc)
      done;
    done;
    !total
  let solve factor =
    load_input factor
    |> compute_distances
    |> Solution.printf "%d"

  let solve_part1 () = solve 2
  let solve_part2 () = solve 1_000_000
end

let () = Solution.register_mod (module S)