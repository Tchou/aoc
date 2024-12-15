open Utils
open Syntax
module S =
struct
  let name = Name.mk "s06"
  module G = Grid.StringGrid
  let read_input () =
    let grid = G.read () in
    let start = G.find (function '^' -> true | _ -> false) grid in
    start, grid

  let visited = ~%[]
  let explore pos grid =
    let rec loop p d path plen =
      let path, plen = if visited %? p then path, plen else begin
          visited.%{p}<-();
          (p, d)::path, 1+plen
        end
      in
      let p' = Grid.(p +! d) in
      if not (G.inside grid p') then path, plen
      else if grid.G.!(p') = '#' then
        loop p (Grid.right90 d) path plen
      else
        loop p' d path plen
    in
    Hashtbl.clear visited;
    loop pos Grid.north [] 0

  let visited = ~%[]
  let has_cycle pos_s pos dir grid =
    let rec loop d p  =
      let key = p, d in
      if visited %? key then true
      else begin
        visited.%{key} <- ();
        let p' = Grid.(p +! d) in
        if not (G.inside grid p') then false
        else if p' = pos_s || grid.G.!(p') = '#' then
          loop Grid.(right90 d) p
        else
          loop d p'
      end
    in
    Hashtbl.clear visited;
    loop dir pos

  let count_cycles pos grid =
    (* find the normal path *)
    let path, n = explore pos grid in
    (* if there is a cycle, it can only be found by puting the stone
       on one of the position of the normal path (and not on the start position)
       Also, there is no need start iterating from the start but only from the
       step before hitting the new stone.
    *)
    let count = ref 0 in
    List.iter (fun (pos_s, dir) ->
        if has_cycle pos_s Grid.(pos_s +! (opposite dir)) dir grid then incr count;
      ) (List.tl (List.rev path));
    !count

  let solve_part1 () =
    let start, grid = read_input () in
    let _, n = explore start grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
  let solve_part2 () =
    let start, grid = read_input () in
    let n = count_cycles start grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

end

let () = Solution.register_mod (module S)