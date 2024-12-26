open Utils
open Syntax
module S =
struct
  let name = Name.mk "s03"

  let read_wire () =
    Input.read_line ()
    |> String.split_on_char ','
    |> List.map (fun s ->
        Scanf.sscanf s "%[DULR]%d" (fun ds n ->
            let d =
              match ds with
              |"D" -> (0,1)
              |"U" -> (0, -1)
              |"L" -> (-1, 0)
              |"R" -> (1, 0)
              | _ -> assert false
            in d, n
          ))
  let trace f l =
    List.fold_left (fun (x, y) ((i, j), n) ->
        for k = 1 to n do
          let xi = x + i * k in
          let yj = y + j * k in
          f (xi, yj)
        done;
        (x + i * n, y + j * n)
      ) (0, 0) l

  let run_wires wire1 wire2 =
    let cache1 = ~%[] in
    let step = ref 0 in
    let f1 ((x, y) as c) =
      incr step;
      cache1.%{c} <- !step in
    let _ = trace f1 wire1 in
    let min_dist = ref max_int in
    let fastest_inter = ref max_int in
    let () = step := 0 in
    let f2 ((x, y) as c) =
      incr step;
      if (x <> 0 || y <> 0) && cache1 %? c then
        let dist = abs x + abs y in
        if dist < !min_dist then min_dist := dist;
        let speed = cache1.%{c} + !step in
        if speed < !fastest_inter then fastest_inter := speed
    in
    let _ = trace f2 wire2 in
    !min_dist, !fastest_inter

  let solve f =
    let wire1 = read_wire () in
    let wire2 = read_wire () in
    let n = f (run_wires wire1 wire2) in
    Solution.printf "%d" n
  let solve_part1 () = solve fst
  let solve_part2 () = solve snd
end

let () = Solution.register_mod (module S)