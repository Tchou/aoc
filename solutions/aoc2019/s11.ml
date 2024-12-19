open Utils
open Syntax
module S =
struct
  let name = Name.mk "s11"

  let right (i, j) = (-j, i)
  let left (i, j) = (j, -i)
  let forward (x, y) (i, j) = (x+i, y+j)
  let paint code input =
    let map = ~%[(0,0),input] in
    let state = Intcode.make_state code in
    let rec loop pos dir state =
      let cur_color = map.%?{pos} or 0 in
      Queue.push cur_color state.Intcode.stdin;
      let need_input = Intcode.eval state in
      if need_input = `need_input then begin
        let new_color = Queue.pop state.Intcode.stdout in
        let turn = Queue.pop state.Intcode.stdout in
        if new_color <> cur_color then map.%{pos} <- new_color;
        let new_dir = if turn = 0 then left dir else right dir in
        let new_pos = forward pos new_dir in
        loop new_pos new_dir state
      end
    in
    loop (0, 0) (0, -1) state;
    map

  let solve_part1 () =
    let code = Intcode.read () in
    let h = paint code 0 in
    Ansi.(printf "%a%d%a\n" fg green (Hashtbl.length h) clear color)

  let render map =
    let min_x = ref 0 in
    let min_y = ref 0 in
    let max_x = ref 0 in
    let max_y = ref 0 in
    let () =
      Hashtbl.iter (fun (x, y) _ ->
          if x < !min_x then min_x := x;
          if y < !min_y then min_y := y;
          if x > !max_x then max_x := x;
          if y > !max_y then max_y := y;
        ) map;
    in
    for y = !min_y to !max_y do
      for x = !min_x to !max_x do
        let color = map.%?{x, y} or 0 in
        if color = 1 then Ansi.(printf "%a %a" bg green clear color)
        else Ansi.printf " "
      done;
      Ansi.printf "\n%!"
    done
  let solve_part2 () =
    let code = Intcode.read () in
    let map = paint code 1 in
    render map
end

let () = Solution.register_mod (module S)