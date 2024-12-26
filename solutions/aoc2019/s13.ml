open Utils
open Syntax
module S =
struct
  let name = Name.mk "s13"
  let count_blocks queue =
    let map = ~%[] in
    let len = Queue.length queue in
    assert (len mod 3 = 0);
    for _ = 0 to len/3 - 1 do
      let x = Queue.pop queue in
      let y = Queue.pop queue in
      let t = Queue.pop queue in
      map.%{x, y} <- t;
    done;
    Hashtbl.fold (fun _ t acc -> acc + int_of_bool (t = 2)) map 0
  let solve_part1 () =
    let code = Intcode.read () in
    let state = Intcode.make_state code in
    let _ = Intcode.eval state in
    let n = count_blocks state.stdout in
    Solution.printf "%d" n

  let play animate state =
    let screen = Array.init 100 (fun _ -> Bytes.make 100 '\x00') in
    let max_x = ref (-1) in
    let max_y = ref (-1) in
    let ball_x = ref (-1) in
    let ball_y = ref (-1) in
    let pad_x = ref (-1) in
    let num_blocks = ref 0 in
    let score = ref 0 in
    let display () =
      Ansi.(printf "%a%a" clear screen clear cursor);
      for j = 0 to !max_y do
        for i = 0 to !max_x do
          match Bytes.get_uint8 screen.(j) i with
            0 -> Ansi.(printf " " )
          | 1 -> Ansi.(printf "%a %a" bg white clear color)
          | 2 -> Ansi.(printf "%a %a" bg blue clear color)
          | 3 -> Ansi.(printf "%a %a" bg green clear color)
          | 4 -> Ansi.(printf "%a %a" bg yellow clear color)
          | _ -> assert false
        done;
        Ansi.printf "\n";
      done;
      Ansi.printf "SCORE:%d\n%!" !score;
    in
    if animate then for _ = 0 to 100 do Ansi.printf "\n%!" done;
    let rec loop () =
      if animate then display ();
      match Intcode.eval ~out_count:3 state with
        `full_output ->
        let x = Queue.pop state.stdout in
        let y = Queue.pop state.stdout in
        let t = Queue.pop state.stdout in
        if x = -1 && y = 0 then begin
          score := t;
          if !num_blocks != 0 then loop () else !score;
        end else begin
          let () = match t with
            | 0 when Bytes.get_uint8 screen.(y) x = 2 -> decr num_blocks
            | 2 -> incr num_blocks
            | 3 -> pad_x := x
            | 4 -> ball_x := x; ball_y := y
            | _ -> ()
          in
          max_y := max !max_y y;
          max_x := max !max_x x;
          Bytes.set_uint8 (screen.(y)) x t;
          loop ()
        end
      | `need_input ->
        let d =
          if !ball_x < !pad_x then -1
          else if !ball_x > !pad_x then 1
          else 0
        in
        Queue.push d state.stdin;
        if animate then Unix.sleepf 0.05;
        loop ()
      | `halt -> !score
    in
    loop ()

  let solve2 animate =
    let code = Intcode.read () in
    code.(0) <- 2;
    let state = Intcode.make_state code in
    let n = play animate state in
    Solution.printf "%d" n
  let solve_part2 () = solve2 false
end
module SA =
struct
  let name = S.name
  let solve_part1 = S.solve_part1
  let solve_part2 () = S.solve2 true
end
let () = Solution.register_mod (module S)
let () = Solution.register_mod ~variant:"animate" (module SA)