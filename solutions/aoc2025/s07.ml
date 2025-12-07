open Utils
open Grid
module S (A : sig val animate : bool end)=
struct
  let name = Name.mk "s07"
  let read_input () =
    BytesGrid.read ()

  let step grid w y =
    let total = ref 0 in
    let open BytesGrid in
    if y > 0 then
      for x = 0 to w - 1 do
        let pos = x, y in
        let c = grid.!(pos) in
        let above = grid.!(pos +! north) in
        if above = 'S' || above = '|' then 
          if c = '.' then grid.!(pos) <- '|'
          else if c = '^' then begin
            grid.!(x - 1, y) <- '|';
            grid.!(x + 1, y) <- '|';
            incr total;
          end;
      done;
    !total

  let pp_line fmt (step, grid, y, current) =
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
                 grid.!(spos +! north) = '|')
    in
    let start, stop = match current with None -> 0, Bytes.length l - 1 | Some (i, _) -> i, i in
    let start = max 0 (start - 5) in
    Ansi.(fprintf fmt "%a" move_cursor (y+1, start+1));
    for x = start to stop do
      let c = Bytes.get l x in
      let pos = x, y in
      let ac_color, splitter, tu_d_, tu_br, t__br, tulb_, t_lb_, tulbr, t_lbr, tul_r, tu__r, tul__ = 
        if step = 0 then 
          Ansi.white, "┴", ".", ".", ".", ".", ".", ".", ".", "┴", "┴", "┴"
        else if step = 1 && is_current pos then
          Ansi.yellow, "┴","╹", "┗","╺","┛","╸","┻","━","┻","┗","┛"
        else if step = 2 && is_current pos then
          Ansi.yellow, "┴","╽", "┢","┎","┧","┒","╁","┰","┴","└","┘"
        else
          (if is_current pos && current <> None && step < 4 then Ansi.yellow else Ansi.green),  
          "▲","│", "├","┌","┤","┐","┼","┬","┴","└","┘"
      in
      match c with
      'S' -> Ansi.(fprintf fmt "%aS%a" bfg ac_color clear color)
      |'^' -> (* a splitter *)
        if has_active_spliter (x, y) (0,0) then
          let s = 
            match BytesGrid.(grid.!((x, y) +! west) = '|',  grid.!((x, y) +! east) = '|') with
            | true, true -> tul_r
            | false, true -> tu__r
            | true, false -> tul__
            | false, false -> tul_r
          in 
          Ansi.(fprintf fmt "%a%s%a" bfg ac_color s clear color)
        else 
          Ansi.(fprintf fmt "%a%s%a" bfg (if step >= 3 || current <> None then red else yellow) splitter clear color)
      |'|' -> (* this is a beam *)
        let beam_above = BytesGrid.(grid.!(pos +! north)) = '|' in
        let s = match has_active_spliter pos west, has_active_spliter pos east with
            false, false -> tu_d_
          | false, true -> if beam_above then tu_br else t__br
          | true, false -> if beam_above then tulb_ else t_lb_
          | true, true -> if beam_above then tulbr else t_lbr
        in
        Ansi.(fprintf fmt "%a%s%a" bfg ac_color s clear color)
      | '.' ->
        if step = 0 then
          Ansi.(fprintf fmt "%a.%a"  bfg black  clear color)
        else
          Ansi.(fprintf fmt "%a%!" move_cursor (y+1, x+2))
      | _ ->  assert false
    done


  let animate_line delay step grid y current =
    if step = 0 || step = 4 then
      Ansi.(printf "%a%a%!" move_cursor (y+1, 1)
              pp_line (step, grid, y, current))
    else
      for step = 1 to 3 do 
        Ansi.(printf "%a%a%!" move_cursor (y+1, 1)
                pp_line (step, grid, y, current));
        Unix.sleepf delay
      done
  let pp_full grid =
    Ansi.(printf "%a%a%a%!" move_cursor (1,1)
            clear screen hide_cursor ());
    for y = 0 to BytesGrid.height grid - 1 do
      animate_line 0.0 0 grid y None;
    done;
    Ansi.(printf "%a%!" move_cursor (1,1))

  let count_splits grid = 
    if A.animate then pp_full grid;
    let w = BytesGrid.width grid in
    let last = BytesGrid.height grid - 1 in
    let total = ref 0 in
    for y = 0 to last do
      total := !total + step grid w y;
      if A.animate then animate_line 0.03 3 grid y None;
    done;
    if A.animate then Ansi.(printf "%a%!" move_cursor (last+2,1));
    !total

  let solve_part1 () =
    let grid = read_input () in 
    let n = count_splits grid in
    Solution.printf "%d" n

  (* rewrite as a recursive function + memo *)
  let count_timelines grid =     
    let start = BytesGrid.find ((=) 'S') grid in
    let w = BytesGrid.width grid in 
    let h = BytesGrid.height grid in
    let memo = Array.make_matrix h w (-1) |> IntGrid.of_matrix in
    let last = h - 1 in
    let delay = 1. /. (float (w*h)) in
    if A.animate then pp_full grid;
    let dirs = [| west; east |] in
    let rec loop ((_, h) as pos) =
      let n = IntGrid.(memo.!(pos)) in
      if n >= 0 then n else begin
        if BytesGrid.(grid.!(pos) = '.') then BytesGrid.(grid.!(pos) <- '|');
        if A.animate then animate_line delay 3 grid h (Some pos);
        let res = 
          if h = last then 1
          else begin
            let below = pos +! south in
            if BytesGrid.(grid.!(pos)) <> '^' then loop below
            else
              dirs |>
              Array.fold_left (fun acc dir -> 
                  let pos' = pos +! dir in
                  if BytesGrid.inside grid pos' then 
                    loop pos' + acc
                  else acc) 0
          end
        in
        if A.animate then animate_line delay 4 grid h (Some pos);
        IntGrid.(memo.!(pos) <- res); res
      end
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