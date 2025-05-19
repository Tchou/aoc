open Utils
open Syntax
module S =
struct
  let name = Name.mk "s22"

  let read_input () =
    let module G = Grid.StringGrid in
    let g = G.read () in
    let w = G.width g in
    let h = G.height g in
    let map = ~%[] in
    G.iter (fun (i, j) c ->
        map.%{i - w/2, j - h/2} <- c) g;
    map

  let spread choice map n =
    let rec loop n pos dir count =
      if n = 0 then count else
        let c = try map.%{pos} with Not_found -> '.' in
        let c', dir', count' = choice c dir count in
        map.%{pos}<- c';
        loop (n-1) Grid.(pos +! dir') dir' count'
    in
    loop n (0,0) Grid.north 0

  let choice1 c dir count =
    if c = '#' then '.', Grid.right90 dir, count else '#', Grid.left90 dir, (count+1)


  let choice2 c dir count =
    match c with
    | '#' -> 'F', Grid.right90 dir, count
    | 'W' -> '#', dir, (count+1)
    | 'F' -> '.', Grid.(right90 (right90 dir)), count
    | _ (* '.' *) -> 'W', Grid.left90 dir, count

  let solve f k =
    let map = read_input () in
    let n = spread f map k in
    Solution.printf "%d" n

  let solve_part1 () = solve choice1 10000
  let solve_part2 () = solve choice2 10000000
end

let () = Solution.register_mod (module S)