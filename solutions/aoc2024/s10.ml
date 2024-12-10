open Utils
open Syntax
module S =
struct
  let name = Name.mk "s10"
  let read_input () =
    Input.fold_lines (fun acc s -> s::acc) []
    |> List.rev
    |> Array.of_list

  let dirs = [ (0,1); (1,0); (0,-1); (-1, 0)]
  let (+!) (a, b) (c, d) = (a+c, b+d)

  let succc c = Char.chr (1 + Char.code c)

  let valid w h (x,y) = x >= 0 && x < w && y >= 0 && y < h

  let iter_paths_from f grid p =
    let w = String.length grid.(0) in
    let h = Array.length grid in
    let rec loop p c =
      if c = '9' then f p else
        dirs
        |> List.iter (fun d ->
            let (xn, yn) as np = p +! d in
            let nc = succc c in
            if valid w h np && grid.(yn).[xn] = nc then
              loop np nc)
    in loop p '0'

  let count_paths uniq grid =
    let count = ref 0 in
    let f, reset =
      if uniq then
        let seen = ~%[] in
        (fun p -> if not (seen %? p) then
            let () = seen.%{p} <- () in
            incr count), (fun () -> Hashtbl.clear seen)
      else (fun _ -> incr count), ignore
    in
    let w = String.length grid.(0) in
    let h = Array.length grid in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        if grid.(y).[x] = '0' then
          iter_paths_from f grid (x, y);
        reset ()
      done;
    done;
    !count

  let solve uniq =
    let grid = read_input () in
    let n = count_paths uniq grid in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  let solve_part1 () = solve true
  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S)