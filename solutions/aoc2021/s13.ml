open Utils
open Syntax

module S =
struct
  let name = Name.mk "s13"


  let apply_fold_y grid n =
    let ngrid = ~%[] in
    Hashtbl.iter (fun (x, y) () ->
        let y = if y <= n then y else 2*n - y in
        ngrid.%{x, y} <- ())
      grid;
    ngrid

  let apply_fold_x grid n =
    let ngrid = ~%[] in
    Hashtbl.iter (fun (x, y) () ->
        let x = if x <= n then x else 2*n - x in
        ngrid.%{x, y} <- ())
      grid;
    ngrid

  let read_points () =
    let grid = ~%[] in
    InputUntil.fold_fields ',' (fun () -> function
          [sx; sy] ->
          grid.%{int_of_string sx, int_of_string sy} <- ();
          true, ()
        | _ -> false,()) ();
    grid
  let read_instructions () =
    Input.fold_scan "fold along %[^=]=%d" (fun acc s n ->
        (if s = "x" then `X n else `Y n)::acc)
      []
    |> List.rev

  let dump_grid grid =
    let max_x, max_y =
      Hashtbl.fold (fun (x, y) () (mx, my) ->
          (max mx x, max my y)) grid (0, 0)
    in
    for y = 0 to max_y do
      for x = 0 to max_x do
        if grid %? (x, y) then
          Ansi.(Solution.printf "%a %a" bbg green clear color)
        else
          Solution.printf " "
      done;
      Solution.printf "\n%!"
    done
  let solve_part1 () =
    let grid = read_points () in
    let ngrid =
      match read_instructions () with
        `X n :: _ -> apply_fold_x grid n
      | `Y n :: _ -> apply_fold_y grid n
      | _ -> grid
    in
    Solution.printf "%d" (Hashtbl.length ngrid)
  let solve_part2 () =
    let grid = read_points () in
    let instrs = read_instructions () in
    let ngrid = List.fold_left (fun grid ->
        function `X n -> apply_fold_x grid n
               | `Y n -> apply_fold_y grid n) grid instrs
    in
    dump_grid ngrid
end

let () = Solution.register_mod (module S)