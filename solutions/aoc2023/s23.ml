open Utils
open Syntax
module S =
struct
  let name = Name.mk "s23"

  let load_input () =
    let lines = Input.fold_lines (fun acc s -> s::acc) [] in
    let grid = List.rev lines |> Array.of_list in
    let start = (0, String.index grid.(0) '.') in
    let dest= let l = Array.length grid in
      (l-1, String.index grid.(l-1) '.')
    in
    grid, start, dest

  let dirs = [(0,1); (0,-1); (1,0); (-1, 0)]

  let is_valid grid (r, c) =
    r >= 0 && c >= 0 && r < Array.length grid &&
    c < String.length grid.(c) && grid.(r).[c] <> '#'

  let next dirs grid (r, c) =
    List.filter_map
      (fun (i, j) -> let p = (r+i, c+j) in if is_valid grid p then Some p else None)
      dirs
  let next4 = next dirs

  let valid_dirs = function
      '>' -> [ (0,1) ]
    | '<' -> [ (0, -1) ]
    | '^' -> [ (-1, 0) ]
    | 'v' -> [ (1,0) ]
    | '.' -> dirs
    | _ -> assert false

  let next_part1 grid (r, c) =
    List.filter_map
      (fun (i, j) -> let p = (r+i, c+j) in if is_valid grid p then Some (p,1) else None)
      (valid_dirs grid.(r).[c])

  let ppv ppf (i, j) =
    Ansi.fprintf ppf "(%d, %d)" i j

  module Graph = Hashtbl.Make (
    struct
      type t = int * int
      let equal ((x1, y1) as p1) ((x2, y2) as p2) =
        p1 == p2 || x1 = x2 && y1 = y2

      let hash (x, y) = (x lsl 8) lor  y
    end
    )
  let path_finder graph (w,h) succ start dest =
    let max_len = ref 0 in
    let visited = Array.init h (fun _ -> Bytes.make w '\x00') in
    let rec loop n v =
      if v = dest then
        max_len := max n !max_len
      else
        List.iter (
          fun ((r, c) as w, cst) ->
            if visited.(r).$[c] = '\x00' then begin
              visited.(r).$[c] <- '\x01';
              loop (n+cst) w;
              visited.(r).$[c] <-'\x00'
            end) (succ graph v)
    in
    loop 0 start;
    !max_len

  let dims grid =
    (Array.length grid, String.length grid.(0))
  let solve_part1 () =
    let grid, start, dest = load_input () in
    path_finder grid (dims grid) next_part1 start dest
    |> Solution.printf "%d"

  let rec replace w n l =
    match l with
      [] -> [(w, n)]
    | (v, m):: ll ->
      if v = w then raise Exit
      else (v, m) :: replace w n ll

  let rec follow grid v origin n =
    let nexts = next4 grid v in
    match nexts with
      _::_::_::_ | [] -> v, n
    | [ w ] -> if w <> origin then
        follow grid w v (n+1)
      else v,n
    | [w1; w2] ->
      if w1 <> origin then follow grid w1 v (n+1)
      else follow grid (w2) v (n+1)

  let build_graph grid (w, h) start stop =
    let igraph = Array.make_matrix w h [] in
    let rec loop queue =
      if not (Queue.is_empty queue) then begin
        let (r, c) as v = Queue.take queue in
        let nexts = next4 grid v in
        List.iter (fun w ->
            let w',cst = follow grid w v 1 in
            try
              igraph.(r).(c) <- replace w' cst igraph.(r).(c);
              Queue.add w' queue
            with Exit -> ()
          ) nexts;
        loop queue
      end
    in
    let queue = Queue.create () in
    Queue.add start queue;
    loop queue;
    igraph

  let next_part2  t (r, c) = t.(r).(c)
  let solve_part2 () =
    let grid, start, stop = load_input () in
    let gdims = dims grid in
    let igraph = build_graph grid gdims start stop in
    let stop', c =
      match next_part2 igraph stop with
        [ x ] -> x (* avoid the last part of the path, since
                      it's fixed. *)
      | _ -> stop, 0
    in
    c + path_finder igraph gdims next_part2 start stop'
    |> Solution.printf "%d"

end

let () = Solution.register_mod (module S)