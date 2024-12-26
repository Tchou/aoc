open Utils
module S =
struct
  let name = Name.mk "s04"
  module G = Grid.StringGrid

  let iter_all_dirs grid n p f =
    G.iter8 (fun _ _ d ->
        let pi = ref p in
        try
          for k = 1 to n do
            pi := Grid.(!pi +! d);
            if G.inside grid !pi then
              f k !pi
          done;
        with Exit -> ()) grid p

  let count_xmas grid =
    let count = ref 0 in
    G.iter (fun p c ->
        if c = 'X' then
          iter_all_dirs grid 3 p
            (fun i q ->
               match i, grid.G.!(q) with
                 1, 'M' | 2, 'A' -> ()
               | 3, 'S' -> incr count
               | _ -> raise Exit
            )
      ) grid;
    !count

  let test_oposites_corner grid p d f =
    let open Grid in
    let p1 = p +! d in
    let p2 = p +! (opposite d) in
    G.inside grid p1 && G.inside grid p2 && (f p1 p2 || f p2 p1)

  let count_2mas grid =
    let count = ref 0 in
    let test_ms p q = grid.G.!(p) = 'M' && grid.G.!(q) = 'S' in
    G.iter (fun p c ->
        if c = 'A' &&
           test_oposites_corner grid p Grid.north_west test_ms &&
           test_oposites_corner grid p Grid.south_west test_ms
        then incr count
      ) grid;
    !count

  let solve count  =
    let grid = G.read () in
    let n = count grid in
    Solution.printf "%d" n

  let solve_part1 () =
    solve count_xmas

  let solve_part2 () =
    solve count_2mas
end

let () = Solution.register_mod (module S)