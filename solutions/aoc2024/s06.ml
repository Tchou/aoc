open Utils
open Syntax
module S =
struct
  let name = Name.mk "s06"

  let rec find_char s l =
    match l with
      [] -> None
    | c :: ll -> match String.index_opt s c with
        Some i -> Some i
      | None -> find_char s ll

  let read_input () =
    let start = ref (0,0) in
    let y = ref 0 in
    let grid =
      Input.fold_lines (fun acc line ->
          let () =
            match String.index_opt line '^' with
              Some x -> start := (x, !y)
            | None -> ()
          in incr y;
          line::acc
        ) []
      |> List.rev
      |> Array.of_list
    in
    !start, grid

  let valid grid x y =
    let h = Array.length grid in
    let w = String.length grid.(0) in
    x >= 0 && x < w && y >= 0 && y < h

  let visited = ~%[]
  let explore x y grid =
    let rec loop x y i j path plen =
      let path, plen = if visited %? (x, y) then path, plen else begin
          visited.%{x, y}<-();
          (x, y, i, j)::path, 1+plen
        end
      in
      let xi = x + i in
      let yj = y + j in
      if not (valid grid xi yj) then path, plen
      else if grid.(yj).[xi] = '#' then
        loop  x y (-j) i path plen
      else
        loop xi yj i j path plen
    in
    Hashtbl.clear visited;
    loop x y 0 (-1) [] 0

  let visited = ~%[]
  let has_cycle xs ys x y i j grid =
    let rec loop i j x y  =
      let key = x, y, i, j in
      if visited %? key then true
      else begin
        visited.%{key} <- ();
        let xi = x + i in
        let yj = y + j in
        if not (valid grid xi yj) then false
        else if (xi = xs && yj = ys) || grid.(yj).[xi] = '#' then
          loop (-j) i x y
        else
          loop i j xi yj
      end
    in
    Hashtbl.clear visited;
    loop i j x y

  let count_cycles x y grid =
    (* find the normal path *)
    let path, n = explore x y grid in
    (* if there is a cycle, it can only be found by puting the stone
       on one of the position of the normal path (and not on the start position)
       Also, there is no need start iterating from the start but only from the
       step before hitting the new stone.
    *)
    let count = ref 0 in
    List.iter (fun (xs, ys, i, j) ->
        if has_cycle xs ys (xs-i) (ys-j) i j grid then incr count;
      ) (List.tl (List.rev path));
    !count

  let solve_part1 () =
    let (x, y), grid = read_input () in
    let _, n = explore x y grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
  let solve_part2 () =
    let (x, y), grid = read_input () in
    let n = count_cycles x y grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

end

let () = Solution.register_mod (module S)