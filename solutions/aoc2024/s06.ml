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

  let tdir = Array.of_list Grid.dir4
  let idx_dir = function
      (0,-1) -> 0
    | (1, 0) -> 1
    | (0, 1) -> 2
    | (-1, 0) -> 3
    | _ -> assert false
  let () = List.iter (fun p -> assert (p = tdir.(idx_dir p))) Grid.dir4

  let has_cycle visited pos_s pos idir grid =
    let rec loop id p  =
      let x, y = p in
      (visited.(y).(x) lsr id) land 1 = 1 || begin
        visited.(y).(x) <- visited.(y).(x) lor (1 lsl id);
        let p' = Grid.(p +! tdir.(id)) in
        if not (G.inside grid p') then false
        else if p' = pos_s || grid.G.!(p') = '#' then
          loop ((id + 1) land 0b11) p
        else
          loop id p'
      end
    in
    for i = 0 to Array.length visited - 1 do
      Array.fill visited.(i) 0 (Array.length visited.(i)) 0;
    done;
    loop idir pos

  let count_cycles pos grid =
    (* find the normal path *)
    let path, n = explore pos grid in
    (* if there is a cycle, it can only be found by puting the stone
       on one of the position of the normal path (and not on the start position)
       Also, there is no need start iterating from the start but only from the
       step before hitting the new stone.
    *)
    let count = ref 0 in
    let visited = Array.init (G.height grid) (fun _ -> Array.make (G.width grid) 0) in
    List.iter (fun (pos_s, dir) ->
        if has_cycle visited pos_s Grid.(pos_s +! (opposite dir)) (idx_dir dir) grid then incr count;
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