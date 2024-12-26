open Utils
open Syntax
module S =
struct
  let name = Name.mk "s10"
  module G = Grid.StringGrid

  let succc c = Char.chr (1 + Char.code c)

  let iter_paths_from f grid p =
    let rec loop p c =
      if c = '9' then f p else
        Grid.dir4
        |> List.iter (fun d ->
            let np = Grid.(p +! d) in
            let nc = succc c in
            if G.inside grid np && grid.G.!(np) = nc then
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
    grid
    |> G.iter (fun pos c -> if c = '0' then iter_paths_from f grid pos; reset ());
    !count

  let solve uniq =
    let grid = G.read () in
    let n = count_paths uniq grid in
    Solution.printf "%d" n

  let solve_part1 () = solve true
  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S)