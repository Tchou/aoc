open Utils
open Syntax
module S (X:sig val animate : bool end)=
struct
  let name = Name.mk "s17"

  let read_input () =
    let clay = Hashtbl.create 16 in
    let x_max = ref min_int in
    let x_min = ref max_int in

    let y_max = ref min_int in
    let y_min = ref max_int in

    Input.fold_lines (
      fun () l ->
        Scanf.sscanf l "%[xy]=%d, %[xy]=%d..%d"
          (fun c n1 _ n2 n3 ->
             let mk = if c = "x" then (fun a b -> a, b)
               else (fun a b -> b, a)
             in
             for i = n2 to n3 do
               let x, y = mk n1 i in
               clay %+ (x, y);
               x_max := max x !x_max;
               x_min := min x !x_min;

               y_max := max y !y_max;
               y_min := min y !y_min;

             done
          )) ();
    clay, (!x_min, !x_max, !y_min, !y_max)

  let buff = Buffer.create 16
  let bfmt = Format.formatter_of_buffer buff
  let display (clay, water, stack_pos, (x_min, x_max, y_min, y_max)) pos =
    if X.animate then begin
      Buffer.clear buff;
      Format.pp_print_flush bfmt ();
      let _, yp = pos in
      Ansi.(fprintf bfmt "%a"  clear cursor);
      for y = max y_min (yp - 30) to min y_max (yp + 30) do
        for x = x_min to x_max do
          let p = (x, y) in
          let c, col =
            if water %? p then  "~", Ansi.cyan
            else if clay %? p then "▒", Ansi.yellow
            else if stack_pos %? p then "⁞", Ansi.cyan
            else " ", Ansi.white
          in Ansi.(fprintf bfmt "%a%s%a" bfg col c clear color);
        done;
        Ansi.fprintf bfmt "\n"
      done;
      Format.pp_print_flush bfmt ();
      Ansi.printf "%s%!" Buffer.(contents buff);
      Buffer.clear buff
    end

  let simulate part2 clay ((_, _, y_min, y_max) as dims) =
    let water = ~%[] in
    let falling = ~%[] in
    let display = display (clay, water, falling, dims) in
    let rec fall stack =
      match stack with
      |  [] -> Hashtbl.(length water + (if part2 then 0 else length falling))
      | pos :: stack' ->
        display pos;
        let (_, y) as dst = Grid.(pos +! south) in
        if falling %? pos then fall stack'
        else if y > y_max then (falling %+ pos; fall stack')
        else if not (clay %? dst || water %? dst) then (falling %+ pos;fall (dst::stack'))
        else spread pos stack'
    and spread pos stack =
      display pos;
      let rleft, accl = spread_dir pos [pos] Grid.west in
      let rright,acc = spread_dir pos accl Grid.east in
      match rleft, rright with
        None, None ->
        List.iter (fun p -> water %+ p; falling %- p) acc;
        water %+ pos;
        falling %- pos;
        spread Grid.(pos +! north) stack
      | o1, o2 ->
        List.iter ((%+) falling) acc;
        fall Option.((to_list o1)@(to_list o2)@stack)
    and spread_dir pos acc dir =
      let dst = Grid.(pos +! dir) in
      let dst' = Grid.(dst +! south) in
      if clay %? dst then None, acc
      else if not (clay %? dst'|| water %? dst') then Some dst, acc
      else (spread_dir dst (dst::acc) dir)
    in
    if X.animate then Ansi.(printf "%a%a%!" clear screen hide_cursor ());
    let r = fall [(500, y_min)] in
    if X.animate then Ansi.(printf "%a%!" show_cursor ());
    r

  let solve part2 =
    let clay, dims = read_input () in
    let n = simulate part2 clay dims in
    Solution.printf "%d" n

  let solve_part1 () = solve false
  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S(struct let animate = false end))
let () = Solution.register_mod ~variant:"animate" (module S(struct let animate = true end))
