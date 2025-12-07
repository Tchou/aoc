open Utils
open Grid
module S (A : sig val animate : bool end)=
struct
  let name = Name.mk "s07"
  let read_input () =
    BytesGrid.read ()

  module CoordSet = Set.Make(struct type t = Grid.position let compare = compare end)
  let step grid set i splitters =
    let splitters = ref splitters in
    let new_set =
      CoordSet.fold (fun pos acc ->
          assert (snd pos = i);
          let below = pos +! south in 
          if not (BytesGrid.inside grid below) then acc
          else 
          if BytesGrid.(grid.!(below) = '^') then begin
            splitters := CoordSet.add below !splitters;
            [ below +! west; below +! east]
            |> List.filter (BytesGrid.inside grid)
            |> CoordSet.of_list
            |> CoordSet.union acc
          end else CoordSet.add below acc
        ) set CoordSet.empty
    in
    new_set, !splitters

  let pp_line fmt (step, grid, y, set, prev_set, current) =
    let l = BytesGrid.get_line grid y in
    let is_current (x, y) =
      match current with
      | None -> true
      | Some pos' -> pos' = (x, y) || pos' = (x+1, y) || pos' = (x-1, y)
    in 
    let has_active_spliter pos dir =
      let spos = pos +! dir in
      BytesGrid.(inside grid spos && 
                 grid.!(spos) = '^' &&
                 CoordSet.mem (spos +! north) prev_set)
    in
    Bytes.iteri (fun x c -> 
        let pos = x, y in
        let ac_color, splitter, tu_d_, tu_br, t__br, tulb_, t_lb_, tulbr, t_lbr, tul_r, tu__r, tul__ = 
          if step = 0 then 
            Ansi.white, "^", ".", ".", ".", ".", ".", ".", ".", "^", "^", "^"
          else if step = 1 && is_current pos then
            Ansi.yellow, "^","╹", "┗","╺","┛","╸","┻","━","┻","┗","┛"
          else if step = 2 && is_current pos then
            Ansi.yellow, "^","╽", "┢","┎","┧","┒","╁","┰","┴","└","┘"
          else
            (if is_current pos && current <> None then Ansi.yellow else Ansi.green),  "▲","│", "├","┌","┤","┐","┼","┬","┴","└","┘"
        in
        if c = 'S' then (* the starting point *)
          Ansi.(fprintf fmt "%aS%a" bfg (if step = 3 then green else yellow)  clear color)
        else if c = '^' then (* a splitter *)
          if has_active_spliter (x, y) (0,0) then
            let s = 
              match CoordSet.mem ((x, y) +! west) set, CoordSet.mem ((x, y) +! east) set with
              | true, true -> tul_r
              | false, true -> tu__r
              | true, false -> tul__
              | false, false -> tul_r
            in 
            Ansi.(fprintf fmt "%a%s%a" bfg ac_color s clear color)
          else 
            Ansi.(fprintf fmt "%a%s%a" bfg (if step = 3 || current <> None then red else yellow) splitter clear color)
        else if CoordSet.mem pos set then (* this is a beam *)
          let s = match has_active_spliter pos west, has_active_spliter pos east with
              false, false -> tu_d_
            | false, true -> if CoordSet.mem (pos +! north) prev_set then tu_br else t__br
            | true, false -> if CoordSet.mem (pos +! north) prev_set then tulb_ else t_lb_
            | true, true -> if CoordSet.mem (pos +! north) prev_set then tulbr else t_lbr
          in
          Ansi.(fprintf fmt "%a%s%a" bfg ac_color s clear color)
        else if c = '.' then
          if step = 0 then
            Ansi.(fprintf fmt "%a.%a"  bfg black  clear color)
          else
            Ansi.(fprintf fmt "%a%!" move_cursor (y+1, x+2))
        else assert false
      ) l
  let pp_full grid =
    Ansi.(printf "%a%a%a%!" move_cursor (1,1)
            clear screen hide_cursor ());
    for y = 0 to BytesGrid.height grid - 1 do
      Ansi.printf "%a\n%!"
        pp_line (0, grid, y, CoordSet.empty, CoordSet.empty, None);
    done;
    Ansi.(printf "%a%!" move_cursor (1,1))

  let count_splits grid = 
    let start = BytesGrid.find ((=) 'S') grid in
    assert (snd start = 0);
    if A.animate then pp_full grid;
    let height = BytesGrid.height grid in
    let rec loop set i splitters prev_set =
      if i >= height || CoordSet.is_empty set then splitters
      else
        let () = if A.animate then
            for step = 1 to 3 do
              Ansi.(printf "%a%a\n%!" move_cursor (i+1, 1)
                      pp_line (step, grid, i, set, prev_set, None));
              Unix.sleepf 0.02;
            done
        in
        let new_set, splitters = step grid set i splitters in
        loop new_set (i+1) splitters set
    in
    loop (CoordSet.singleton start) 0 CoordSet.empty CoordSet.empty
    |> CoordSet.cardinal


  let solve_part1 () =
    let grid = read_input () in 
    let n = count_splits grid in
    Solution.printf "%d" n

  let delay = 0.00

  (* rewrite as a recursive function + memo *)
  let count_timelines grid =     
    let start = BytesGrid.find ((=) 'S') grid in
    let prev_set = ref CoordSet.empty in
    assert (snd start = 0);
    if A.animate then pp_full grid;
    let last = BytesGrid.height grid - 1 in
    let memo = Hashtbl.create 16 in
    let dirs = [| west; east |] in
    let rec loop ((_, h) as pos) = 
      match Hashtbl.find_opt memo pos with
        Some n -> n
      | None -> 
        let nset = CoordSet.add pos !prev_set in
        let res =
          let () = if A.animate then
              for step = 1 to 3 do
                Ansi.(printf "%a%a" move_cursor (h+1, 1)
                        pp_line (step, grid, h, nset, !prev_set, Some pos));
              done
          in
          let () = prev_set := nset in
          if h = last then 1
          else
            let below = pos +! south in
            if BytesGrid.(grid.!(pos)) <> '^' then
              loop below
            else
              dirs |>
              Array.fold_left (fun acc dir -> 
                  let pos' = pos +! dir in
                  if BytesGrid.inside grid pos' then loop pos' + acc else acc) 0
        in
        let () = if A.animate then
            Ansi.(printf "%a%a\n%!" move_cursor (h+1, 1)
                    pp_line (3, grid, h, nset, !prev_set, None));
        in
        Hashtbl.add memo pos res; res
    in
    let n = loop start in
    if A.animate then Ansi.(printf "%a%!" move_cursor (last+2, 1));
    n

  let solve_part2 () =
    let grid = read_input () in 
    let n = count_timelines grid in
    Solution.printf "%d" n

end
module S_noanim = S(struct let animate = false end)
module S_anim = S(struct let animate = true end)

let () = Solution.register_mod (module S_noanim)
let () = Solution.register_mod ~variant:"animate" (module S_anim)