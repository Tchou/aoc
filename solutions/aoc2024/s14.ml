open Utils
open Syntax
module S =
struct
  let name = Name.mk "s14"

  type t = { p : int * int; v : int * int }

  let move w h n ({p=x,y; v=vx,vy } as r) =
    let px = n * vx + x in
    let py = n * vy + y in
    let px = px mod w in
    let py = py mod h in
    let px = if px < 0 then w + px else px in
    let py = if py < 0 then h + py else py in
    { r with p = px, py }

  let quadrant w h {p=x,y; _ } =
    let w2 = w / 2 in
    let h2 = h / 2 in
    if x = w2 || y = h2 then 4 (* dummy quadrant *)
    else let i = if x < w2 then 0 else 1 in
      let j = if y < h2 then 0 else 1 in
      j * 2 + i

  let read_input () =
    Input.fold_scan "p=%d,%d v=%d,%d"
      (fun acc x y vx vy -> { p = x,y; v =vx, vy}::acc) []
    |> List.rev

  let simulate w h n = List.map (move w h n)
  let count w h l =
    let res = Array.make 5 0 in
    l
    |> List.iter (fun r -> let i = quadrant w h r in
                   res.(i) <- res.(i) + 1);
    res

  let print w h l =
    let t = ~% (List.map (fun r -> r.p, ()) l) in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        if t %? (x,y) then
          Ansi.(printf "%a %a" bg green clear color)
        else Ansi.printf " "
      done;
      Ansi.printf "\n%!";
    done

  let memo = Array.make (101*103) false
  let is_symmetric len w l =
    let count = ref 0 in
    Array.fill memo 0 (101*103) false;
    try
      List.iter (fun {p=(x,y) as p;_} ->
          let y101 = y * 101 in
          if Array.unsafe_get memo ((w-x-1) +  y101) then
            if !count >= len then raise Exit
            else incr count;
          if not (Array.unsafe_get memo (x +  y101)) then
            Array.unsafe_set memo (x + y101) true) l;
      false
    with Exit -> true

  let find_tree w h l num_robots =
    (* approximation, looking for 1/5th of the robots disposed in a symmetric fashion *)
    let rec loop l n =
      let l = simulate w h 1 l in
      if is_symmetric (num_robots/10) w l then n,l
      else
        loop  l (n+1)
    in
    loop l 1
  let solve_part1 () =
    let w = 101 in
    let h = 103 in
    let l = read_input () in
    let l = simulate w h 100 l in
    let res = count w h l in
    let n = res.(0) * res.(1) * res.(2) * res.(3) in
    Solution.printf "%d" n

  let solve_part2_gen display =
    let w = 101 in
    let h = 103 in
    let l = read_input () in
    let num_robots = List.length l in
    let n,l' = find_tree w h l num_robots in
    if display then print w h l';
    Solution.printf "%d" n
  let solve_part2 () = solve_part2_gen false
end

let () = Solution.register_mod (module S)
let () =
  let module SD =
  struct
    include S let solve_part2 () = solve_part2_gen true
  end
  in
  Solution.register_mod ~variant:"display" (module SD)