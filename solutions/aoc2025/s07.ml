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

  let pp_line step grid y set prev_set =
    let ac_color, splitter, tu_d_, tu_br, t__br, tulb_, t_lb_, tulbr, t_lbr, tul_r, tu__r, tul__ = 
      if step = 0 then 
        Ansi.white, "^", ".", ".", ".", ".", ".", ".", ".", "^", "^", "^"
      else if step = 1 then
        Ansi.yellow, "^","╹", "┗","╺","┛","╸","┻","━","┻","┗","┛"
      else if step = 2 then
        Ansi.yellow, "^","╽", "┢","┎","┧","┒","╁","┰","┴","└","┘"
      else
        Ansi.green,  "▲","│", "├","┌","┤","┐","┼","┬","┴","└","┘"
    in
    let l = BytesGrid.get_line grid y in
    let has_active_spliter pos dir =
      let spos = pos +! dir in
      BytesGrid.(inside grid spos && 
                 grid.!(spos) = '^' &&
                 CoordSet.mem (spos +! north) prev_set)
    in
    Bytes.iteri (fun x c -> 
        let pos = x, y in
        if c = 'S' then
          Ansi.(printf "%aS%a" bfg (if step = 3 then green else white)  clear color)
        else if CoordSet.mem pos set then (* this is a beam *)
          let s = match has_active_spliter pos west, has_active_spliter pos east with
              false, false -> tu_d_
            | false, true -> if CoordSet.mem (pos +! north) prev_set then tu_br else t__br
            | true, false -> if CoordSet.mem (pos +! north) prev_set then tulb_ else t_lb_
            | true, true -> if CoordSet.mem (pos +! north) prev_set then tulbr else t_lbr
          in Ansi.(printf "%a%s%a" bfg ac_color s clear color)
        else if c = '^' then
          if has_active_spliter (x, y) (0,0) then
            let s = 
              match CoordSet.mem ((x, y) +! west) set, CoordSet.mem ((x, y) +! east) set with
              | true, true -> tul_r
              | false, true -> tu__r
              | true, false -> tul__
              | false, false -> assert false
            in 
            Ansi.(printf "%a%s%a" bfg ac_color s clear color)
          else 
            Ansi.(printf "%a%s%a" bfg (if step = 3 then red else cyan) splitter clear color)
        else
          Ansi.(printf "%a%c%a"  bfg black c clear color)
      ) l;
    Ansi.printf "\n%!"

  let pp_full grid =
    for y = 0 to BytesGrid.height grid - 1 do
      pp_line 0 grid y CoordSet.empty CoordSet.empty;
    done

  let count_splits grid = 
    let start = BytesGrid.find ((=) 'S') grid in
    assert (snd start = 0);
    let () = if A.animate then begin 
        Ansi.(printf "%a%!" move_cursor (1,1));
        Ansi.(printf "%a%!" clear screen);
        pp_full grid;
        Ansi.(printf "%a%!" move_cursor (1,1))
      end
    in
    let height = BytesGrid.height grid in
    let rec loop set i splitters prev_set =
      if i >= height || CoordSet.is_empty set then splitters
      else
        let () = if A.animate then begin
            (*Ansi.(printf "%a%!" clear line);*)
            pp_line 1 grid i set prev_set;
            Unix.sleepf 0.05;
            Ansi.(printf "%a%!" move_cursor (i+1, 1));
            pp_line 2 grid i set prev_set;
            Unix.sleepf 0.05;
            Ansi.(printf "%a%!" move_cursor (i+1, 1));
            pp_line 3 grid i set  prev_set;
            Unix.sleepf 0.05
          end
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

  (* rewrite as a recursive function + memo *)
  let count_timelines grid =     
    let start = BytesGrid.find ((=) 'S') grid in
    assert (snd start = 0);
    let last = BytesGrid.height grid - 1 in
    let memo = Hashtbl.create 16 in
    let rec loop ((_, h) as pos) = 
      if h = last then 1
      else
        match Hashtbl.find_opt memo pos with
          Some n -> n
        | None -> 

          let below = pos +! south in
          if BytesGrid.(grid.!(pos)) <> '^' then
            loop below 
          else
            let left = below +! west in
            let n1 = if BytesGrid.inside grid left then loop left else 0 in 
            let right = below +! east in
            let n2 = if BytesGrid.inside grid right then loop right else 0 in 
            let r = n1 + n2 in
            Hashtbl.add memo pos r; r
    in
    loop start

  let solve_part2 () =
    let grid = read_input () in 
    let n = count_timelines grid in
    Solution.printf "%d" n

end
module S_noanim = S(struct let animate = false end)
module S_anim = S(struct let animate = true end)

let () = Solution.register_mod (module S_noanim)
let () = Solution.register_mod ~variant:"animate" (module S_anim)