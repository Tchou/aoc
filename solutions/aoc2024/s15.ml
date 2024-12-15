open Utils
open Syntax
module S =
struct
  let name = Name.mk "s15"

  let sym_dirs = [ '>',(1,0); '<',(-1, 0); 'v',(0, 1); '^',(0, -1)]
  let dirs = List.map snd sym_dirs
  let (+!) (a, b) (c, d) = (a+c, b+d)
  let (-!) (a, b) (c, d) = (a-c, b-d)

  let valid grid x y =
    x >= 0 && y >= 0 &&
    let h = Array.length grid in
    y < h &&
    let w = Bytes.length grid.(0) in
    x < w

  let read_input () =
    let y0 = ref (-1) in
    let x0 = ref (-1) in
    let grid =
      InputUntil.fold_lines (fun acc s ->
          if s = "" then (false,acc) else begin
            if (!x0 < 0 ) then begin
              incr y0;
              x0 := (String.index_opt s '@') or (-1);
            end;
            true, (Bytes.of_string s)::acc
          end) []
      |> List.rev
      |> Array.of_list
    in
    let moves =
      Input.fold_lines (fun acc s ->
          String.fold_left (fun a c -> (List.assoc c sym_dirs)::a) acc s
        ) []
      |> List.rev
    in
    grid, (!x0, !y0), moves

  let move1 grid ((x, y) as p) ((i, j) as d) =
    let rec loop ((x, y) as r) =
      match grid.(y).$[x] with
        '#' -> raise Exit
      | 'O' | '@' |']'| '['->
        let xi, yj = r +! d in
        loop (xi, yj);
        grid.(yj).$[xi] <- grid.(y).$[x]
      | '.' -> ()
      | _ -> assert false
    in
    try loop p; grid.(y).$[x] <- '.'; p +! d
    with Exit -> p

  let pp_grid fmt grid =
    Array.iter (fun b ->
        Bytes.iter (fun c ->
            let open Ansi in
            let f, color =
              match c with
                '.' -> fg, white
              | '#' -> fg, white
              | 'O' | ']' | '[' -> bfg, yellow
              | '@' -> bbg, cyan
              | _ -> assert false
            in printf "%a%c%a" f color c clear Ansi.color
          ) b;
        Ansi.(printf "%a\n%!" clear color)) grid
  let animate = ref false
  let empty_screen () = for _ = 0 to 100 do Ansi.printf "\n%!" done
  let simulate move grid p0 moves =
    if !animate then Ansi.(empty_screen ();
                           printf "%a\n%!START:\n%a\n--\n%!" clear screen pp_grid grid;
                           Unix.sleepf 0.25);
    List.fold_left (fun p d ->
        let p' = move grid p d in
        if !animate then Ansi.(empty_screen ();
                               printf "%a\n%!MOVE (%d, %d):\n%a\n--\n%!" clear screen (fst d) (snd d) pp_grid grid;
                               Unix.sleepf 0.25);
        p'
      ) p0 moves

  let score grid =
    let total = ref 0 in
    for y = 0 to Array.length grid - 1 do
      for x = 0 to Bytes.length grid.(y) - 1 do
        let c = grid.(y).$[x] in
        if c = 'O' || c = '[' then
          total := !total + 100 * y + x
      done
    done;
    !total

  let solve_part1 () =
    let grid, start, moves = read_input () in
    let _ = simulate move1 grid start moves in
    let n = score grid in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)


  let zoom grid =
    grid
    |> Array.map (fun b ->
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
    let rec loop ((x, y) as r) = (* collec the box to be moved, x,y is the left coordinate of a box*)
      (* look at destination *)
      let xd, yd = r +! d in
      match grid.(yd).$[xd],grid.(yd).$[xd + 1] with
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
    match grid.(yb).$[xb] with
      '.' -> grid.(yb).$[xb] <- '@'; grid.(y).$[x] <- '.'; (xb, yb)
    |'#' -> x, y
    | '[' | ']' as c -> begin
        try
          let to_move = loop (xb - (if c = ']' then 1 else 0), yb) in
          let comp = if snd d < 0 then comp_top else comp_down in
          let to_move = List.sort_uniq comp to_move in
          List.iter (fun ((x, y), (xd, yd)) ->
              grid.(yd).$[xd] <- '[';
              grid.(yd).$[xd+1] <- ']';
              grid.(y).$[x] <- '.';
              grid.(y).$[x+1] <- '.';
            )  to_move;
          if c = '[' then begin
            grid.(yb).$[xb] <- '@';
            grid.(yb).$[xb+1] <- '.';
          end else begin
            grid.(yb).$[xb] <- '@';
            grid.(yb).$[xb-1] <- '.';
          end;
          grid.(y).$[x]<- '.';
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
    let _ = simulate move2 grid (x0*2, y0) moves in
    let () = Ansi.(printf "%a\n%!" pp_grid grid) in
    let n = score grid in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

end

let () = Solution.register_mod (module S)
module SA = struct
  let name = S.name
  let solve_part1 () = S.animate := true;
    S.solve_part1 ()
  let solve_part2 () = S.animate := true;
    S.solve_part2 ()
end
let () = Solution.register_mod  (module S)
let () = Solution.register_mod ~variant:"animate" (module SA)
