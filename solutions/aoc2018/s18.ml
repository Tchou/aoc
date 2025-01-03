open Utils
module S =
struct
  module G = Grid.BytesGrid
  let name = Name.mk "s18"
  let read_input () = G.read ()

  let simulate n grid =
    let dest = G.copy grid in
    let rec loop k grid dest =
      if k = n then grid else begin
        grid
        |> G.iter (fun pos c ->
            let open_ = ref 0 in
            let lumber = ref 0 in
            let tree = ref 0 in
            G.iter8 (fun _ d _ ->
                match d with
                  '.' -> incr open_
                | '#' -> incr lumber
                | '|' -> incr tree
                | _ -> assert false
              ) grid pos;
            let nc =
              match c with
                '.' when !tree >= 3 -> '|'
              |'|' when !lumber >= 3 -> '#'
              | '#' when !lumber = 0 || !tree = 0 -> '.'
              | _ -> c
            in
            dest.G.!(pos) <- nc
          );
        loop (k+1) dest grid
      end
    in
    loop 0 (G.copy grid) dest

  let score grid =
    let lumber = ref 0 in
    let tree = ref 0 in
    grid |> G.iter (fun _ c ->
        match c with
          '#' -> incr lumber;
        | '|' -> incr tree
        | _ -> ());
    !lumber * !tree
  let solve_part1 () =
    let grid = read_input () in
    let n = score (simulate 10 grid) in
    Solution.printf "%d" n

  let find_cycle n grid =
    let lam, mu, grid' = Misc.find_cycle G.equal (simulate 1) grid in
    let rem = (n - mu) mod lam in
    simulate rem grid'
    |> score

  let solve_part2 () =
    let grid = read_input () in
    let n = find_cycle 1000000000 grid in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)