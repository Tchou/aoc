open Utils
module S =
struct
  let name = Name.mk "s06"

  type instr = On | Off | Toggle
  (*  First implemented with Arrays, and tested that values never go above 255,
      so reverted to a byte implemention
  *)
  let read_coord c =
    match String.split_on_char ',' c with
      [ x; y ] -> int_of_string x, int_of_string y
    | _ -> assert false
  let read_input () =
    Input.list_fields ' ' (function 
          ["turn"; "on"; c1; "through"; c2 ] -> On, read_coord c1, read_coord c2
        | ["turn"; "off"; c1; "through"; c2 ] -> Off, read_coord c1, read_coord c2
        | ["toggle"; c1; "through"; c2 ] -> Toggle, read_coord c1, read_coord c2
        | _ -> assert false
      )

  let make_grid () = Grid.BytesGrid.init 1000 (fun _ -> Bytes.make 1000 '\x00')

  let apply_range grid f (x1, y1) (x2, y2) =
    let open Grid.BytesGrid in
    for y = y1 to y2 do
      for x = x1 to x2 do
        let c = Char.code (grid.!!(x, y)) in
        let c' = f c in
        if c <> c' then grid.!(x, y) <- Char.unsafe_chr (c') (* avoid writing when unecessary *)
      done;
    done


  let eval_cmd1 = function 
      On -> (fun _ -> 1)
    | Off -> (fun _ -> 0)
    | Toggle -> (fun n -> 1-n)

  let eval_cmd2 = function 
      On -> (fun n -> n+1)
    | Off -> (fun n -> if n > 0 then n - 1 else 0)
    | Toggle -> (fun n -> n+2)

  let apply_commands eval_cmd grid cmd =
    cmd |> List.iter (fun (i, c1, c2) ->
        apply_range grid (eval_cmd i) c1 c2
      )

  let count_on grid =
    let count = ref 0 in
    Grid.BytesGrid.iter (fun _ c -> count := !count + (Char.code c)) grid;
    !count

  let solve eval_cmd ()= 
    let cmds = read_input () in
    let grid = make_grid () in
    let () = apply_commands eval_cmd grid cmds in
    let n = count_on grid in
    Solution.printf "%d" n

  let solve_part1 = solve eval_cmd1
  let solve_part2 = solve eval_cmd2

end

let () = Solution.register_mod (module S)