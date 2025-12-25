open Utils
module S =
struct
  let name = Name.mk "s18"

  module BG = Grid.BytesGrid

  let read_input () = BG.read ()

  let pp fmt g =
    g
    |> BG.iter_lines (Format.(fprintf fmt "%a\n" pp_print_bytes))
  let animate part2 steps grid =
    let grid_out = BG.copy grid in
    let grid_in = BG.copy grid in
    let last_x = BG.width grid - 1 in
    let last_y = BG.height grid - 1 in
    let rec loop n grid_in grid_out =
      if n = 0 then grid_in else
        begin
          grid_in 
          |> BG.(iter (fun ((x, y) as pos) c ->
              if part2 && (x = 0 || x = last_x) && (y = 0 || y = last_y) then
                grid_out.!(pos) <- '#'
              else
                let on =
                  Grid.dir8 |> List.fold_left (fun acc d ->
                      let pos' = Grid.(pos +! d) in
                      if inside grid_in pos' && grid_in.!(pos') = '#'
                      then acc+1
                      else acc
                    ) 0 
                in
                grid_out.!(pos) <-
                  if (c = '#' && (on = 2 || on = 3))
                  || (c = '.' && on = 3) then '#'
                  else '.'
            ));
          loop (n-1) grid_out grid_in
        end 
    in loop steps grid_in grid_out

  let count_on grid =
    let total = ref 0 in
    BG.iter (fun _ c -> if c = '#' then incr total) grid;
    !total
  let solve part2 () =
    let grid = read_input () in
    let out_grid = animate part2 100 grid in
    let n = count_on out_grid in
    Solution.printf "%d" n

  let solve_part1 = solve false
  let solve_part2 = solve true
end

let () = Solution.register_mod (module S)