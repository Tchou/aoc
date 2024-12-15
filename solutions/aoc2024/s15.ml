open Utils
module Make (V : sig val animate : float val random : bool val skip : int end) =
struct
  let name = Name.mk "s15"
  module G = Grid.BytesGrid
  let sym_dirs = Grid.[ '>',east; '<',west; 'v',south; '^',north]
  let pretty_dirs = Grid.[ north,"▲"; east, "▶"; west , "◀"; south, "▼"  ]
  let read_input () =
    let grid = G.read_until (function "" -> true | _ -> false) in
    let start = G.find (function '@' -> true | _ -> false) grid in
    let moves =
      Input.fold_lines (fun acc s ->
          String.fold_left (fun a c -> (List.assoc c sym_dirs)::a) acc s
        ) []
      |> List.rev
    in
    grid, start, moves

  let move1 grid p d =
    let open Grid in
    let rec loop r =
      match grid.G.!(r) with
        '#' -> raise Exit
      | 'O' | '@' |']'| '['->
        let nr = r +! d in
        loop nr;
        grid.G.!(nr) <- grid.G.!(r)
      | '.' -> ()
      | _ -> assert false
    in
    try loop p; grid.G.!(p) <- '.'; p +! d
    with Exit -> p

  let pp_grid robot fmt grid =
    G.iter_lines (fun b ->
        Bytes.iter (fun c ->
            let open Ansi in
            let s, f, color =
              match c with
                '.' -> "·", bfg, black
              | '#' -> "🮖", fg, red
              | 'O' -> "■", bfg, yellow
              | '[' -> "🟨", fg, white
              | ']' -> "", fg, white
              | '@' -> robot, bfg, cyan
              | _ -> assert false
            in printf "%a%s" f color s
          ) b;
        Ansi.(printf "%a\n%!" clear color)) grid
  let fill_screen () = for _ = 0 to 100 do Ansi.printf "\n%!" done

  let do_animate = Float.is_finite V.animate
  let count = ref 0
  let simulate move grid p0 moves =
    let open Ansi in
    if do_animate then fill_screen ();
    Seq.fold_left (fun p d ->
        let p' = move grid p d in
        if do_animate && !count mod (V.skip+1) = 0 then begin
          printf "%a%!MOVE %s:\n%a\n%!" clear cursor (List.assoc d pretty_dirs) (pp_grid (List.assoc d pretty_dirs)) grid;
          if V.animate >= 0.0 then Unix.sleepf V.animate
        end;
        incr count;
        p'
      ) p0 moves

  let score grid =
    let total = ref 0 in
    G.iter (fun (x, y) c ->
        if c = 'O' || c = '[' then
          total := !total + 100 * y + x
      ) grid;
    !total

  let random_dir () = Some (List.nth Grid.dir4 (Random.int 4))

  let solve_part1 () =
    let grid, start, moves = read_input () in
    let _ = simulate move1 grid start
        (if V.random then Seq.of_dispenser random_dir else List.to_seq moves)
    in
    let n = score grid in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)


  let zoom grid =
    grid |> G.map_lines (fun b ->
        b
        |> Bytes.to_seq
        |> List.of_seq
        |> List.map (function
              '@' -> [ '@'; '.' ]
            | 'O' -> [ '[';']']
            | '.' -> [ '.'; '.']
            | '#' -> [ '#'; '#']
            | _ -> assert false)
        |> List.flatten
        |> List.to_seq
        |> Bytes.of_seq
      )

  (*
     [][][][]
      [][][]
       [][]
        []
   Half boxes can only be found on the vertical axis
  *)
  let comp_top ((x1, y1), _) ((x2, y2), _) =
    let c = compare y1 y2 in
    if c = 0 then compare x1 x2 else c
  let comp_down x y = comp_top y x

  let vmove grid ((x, y) as p) d =
    assert (fst d = 0);
    let open Grid in
    let rec loop ((x, y) as r) = (* collec the box to be moved, x,y is the left coordinate of a box*)
      (* look at destination *)
      let xd, yd = r +! d in
      match grid.G.!(xd, yd),grid.G.!(xd + 1, yd) with
      | '#', _ | _, '#' -> (* we are blocked *) raise Exit
      | ('[' as c1 , ']')
      | (']' as c1 , '.')
      | ('.' as c1 , '[') -> (* a box above a box *)
        let xd' = if c1 = '[' then xd else if c1 = ']' then xd - 1 else xd + 1 in
        ((x,y), (xd, yd))  :: loop (xd', yd)
      | ']','[' ->
        let l1 = loop (xd-1, yd) in
        let l2 = loop (xd+1, yd) in
        ((x,y),(xd, yd))::(l1@l2)
      | _ -> [(x,y),(xd,yd)]
    in
    let xb, yb = p +! d in
    match grid.G.!(xb, yb) with
      '.' -> grid.G.!(xb, yb) <- '@'; grid.G.!(x, y) <- '.'; (xb, yb)
    |'#' -> x, y
    | '[' | ']' as c -> begin
        try
          let to_move = loop (xb - (if c = ']' then 1 else 0), yb) in
          let comp = if snd d < 0 then comp_top else comp_down in
          let to_move = List.sort_uniq comp to_move in
          List.iter (fun ((x, y), (xd, yd)) ->
              grid.G.!(xd, yd) <- '[';
              grid.G.!(xd+1, yd) <- ']';
              grid.G.!(x, y) <- '.';
              grid.G.!(x+1, y) <- '.';
            )  to_move;
          grid.G.!(xb, yb) <- '@';
          grid.G.!(xb + (if c = '[' then 1 else -1), yb) <- '.';
          grid.G.!(x, y) <- '.';
          (xb, yb)
        with Exit -> (x, y)
      end
    | _ -> assert false

  let move2 grid p d =
    let v = fst d = 0 in
    if fst d = 0 then vmove grid p d
    else move1 grid p d

  let solve_part2 () =
    let grid, (x0, y0), moves = read_input () in
    let grid = zoom grid in
    let _ = simulate move2 grid (x0*2, y0)
        (if V.random then Seq.of_dispenser random_dir else List.to_seq moves)
    in
    let n = score grid in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

end
module S = Make (struct let animate = Float.nan let skip = 0 let random = false end)
module SA = Make (struct let animate = 0.1 let skip = 0 let random = false end)
module SR = Make (struct let animate = 0.1 let skip = 0 let random = true end)
module SRS = Make (struct let animate = -0.5 let skip = 5000 let random = true end)

let () = Solution.register_mod (module S)
let () = Solution.register_mod ~variant:"animate" (module SA)
let () = Solution.register_mod ~variant:"animate_random" (module SR)
let () = Solution.register_mod ~variant:"animate_random_skip" (module SRS)
