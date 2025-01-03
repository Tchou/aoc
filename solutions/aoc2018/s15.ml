open Utils
open Syntax
module G = Grid.BytesGrid
module S (X:sig val animate : bool end) =
struct
  let name = Name.mk "s15"

  type creature = {
    id : int; (* only used for pretty-printing *)
    kind : char;
    mutable pos : Grid.position;
    mutable hp : int;
  }

  let td_dirs = Grid.[north; west; east; south]
  module Gra : GRAPH with type t = G.t and type v = Grid.position =
  struct
    type v = Grid.position
    type t = G.t

    let iter_vertices g f = G.iter (fun p _ -> f p) g

    let iter_succ g v f =
      (* Carefully iterates in top-down order *)
      List.iter (fun d ->
          let p = Grid.(v +! d) in
          if G.inside g p then
            if g.G.!(p) = '.' then f (p, 1)
        ) td_dirs
  end
  module GrAlgo = GraphAlgo(Gra)

  let cmp_coords (x1, y1) (x2, y2) =
    let c = Int.compare y1 y2 in
    if c = 0 then Int.compare x1 x2 else c

  let cmp_paths p1 p2 = List.compare cmp_coords p1 p2
  let cmp_creatures c1 c2 = cmp_coords c1.pos c2.pos

  let order_creatures = List.sort cmp_creatures
  let enemy = function
    | 'G' -> 'E'
    | 'E' -> 'G'
    | _ -> assert false

  let pp_creature fmt c =
    Format.fprintf fmt "%c (%d) <%d, %d>" c.kind c.hp (fst c.pos) (snd c.pos)


  let display ?(victory=false) ?(delay=0.0) total round attack cr grid creatures =
    let y = ref 0 in
    let num_col = int_of_float (sqrt (float total)) in
    Ansi.(printf "%a" clear cursor);
    Ansi.printf "ROUND: %d, ELF ATTACK POWER: %d\n" round attack;
    G.iter_lines (fun b ->
        Bytes.iteri (fun x c ->
            let s, col = match c with
              'E' -> (if victory then "🯅 " else "🯆 "), Ansi.green
              | 'G' -> (if victory then "🯅 " else "🯆 "), Ansi.red
              | '#' -> "🮕🮕", Ansi.yellow
              | '.' -> "  ", Ansi.white
              | _ -> assert false
            in
            Ansi.(printf "%a%s%a" (if cr.pos = (x, !y) then bfg else fg) col s clear color)
          ) b; incr y; Ansi.printf "\n") grid;
    let map = ~% (creatures
                  |> Hashtbl.to_seq_values |> List.of_seq
                  |> List.sort (fun c1 c2 -> Int.compare c1.id c2.id)
                  |> List.map (fun c -> c.id, c))
    in
    for i = 0 to total - 1 do
      let s,hl,col =
        try let c = map.%{i} in
          let s = Ansi.sprintf "%a" pp_creature c in
          let pad = String.make (20 - String.length s) ' ' in
          s^pad, c.id = cr.id,
          Ansi.(if c.kind = 'E' then green else red)
        with Not_found -> "                    ", false, Ansi.white
      in
      if hl then
        Ansi.(printf "%a%s%a" bfg col s clear color)
      else Ansi.printf "%s" s ;
      if i mod num_col = (num_col - 1) then Ansi.printf "\n"
    done;
    Ansi.printf "\n--\n%!";
    Unix.sleepf delay

  let victory total round attack cr grid creatures =
    for i = 0 to 16 do
      display ~victory:(i mod 2 = 0) ~delay:0.35 total round attack cr grid creatures
    done
  let debug = false
  let game part2 power grid creatures =
    (* Auxiliary data structres *)
    let by_pos = ~%(creatures |> List.map (fun c -> c.pos, c)) in
    let total = Hashtbl.length by_pos in
    let in_range c = (* Returns the list of creatures in range, ordered
                        by increasing hp, and then by coordinates*)
      List.filter_map (fun d ->
          let n = Grid.(c.pos +! d) in
          if G.inside grid n && grid.G.!(n) = enemy c.kind then
            Some by_pos.%{n} else None) td_dirs
      |> List.sort (fun c1 c2 ->
          let x = Int.compare c1.hp c2.hp in
          if x <> 0 then x else cmp_creatures c1 c2)
    in
    let move c p =
      grid.G.!(c.pos) <- '.';
      by_pos %- c.pos;
      c.pos <- p;
      grid.G.!(c.pos) <- c.kind;
      by_pos.%{c.pos} <- c
    in
    let all_creatures () =
      by_pos |> Hashtbl.to_seq_values |> List.of_seq
      |> order_creatures
    in
    let find_path pos targets =
      let () = List.iter (fun t -> grid.G.!(t) <- '.') targets in
      let pmap = GrAlgo.dijkstra ~all_path:false grid pos targets in
      let () = List.iter (fun t -> grid.G.!(t) <- by_pos.%{t}.kind) targets in
      let ltargets = Hashtbl.fold (fun t (cost, paths) acc ->
          if cost != max_int then (cost, t, List.hd paths)::acc else acc ) pmap []
      in
      let stargets = List.sort (fun (c1, t1, _) (c2, t2, _) ->
          let c = Int.compare c1 c2 in
          if c <> 0 then c
          else cmp_coords t1 t2) ltargets
      in
      stargets
    in
    let read_order_path source cost target =
      td_dirs
      |> List.find_map (fun d ->
          let src = Grid.(source +! d) in
          if G.inside grid src && grid.G.!(src) = '.' then
            match find_path src [target] with
              (c, _, p)::_ -> if c+1 = cost then Some p else None
            | [] -> None
          else None
        ) |> Option.get
    in
    let rec loop k creatures =
      (* The creatures list is ordered top to bottom, left to right *)
      match creatures with
        c :: others ->
        if X.animate then display total k power c grid by_pos;
        begin match in_range c with
            e :: _ -> (* an enemy is in range, attack it *)
            attack k e others
          | [] -> (* No enemy, determine the paths *)
            let targets =
              Hashtbl.fold (fun p e acc ->
                  if e.kind = enemy c.kind then p::acc else acc) by_pos []
            in
            let stargets = find_path c.pos targets in
            match stargets with
              [] -> (* no enemy is reachable *)
              loop k others
            | (cost, target, path) :: _ ->
              (* at least one target (sorted in read_order) is reachable, take a single path
                 towards it
              *)
              match read_order_path c.pos cost target with
                [] -> assert false
              | first::_ -> begin

                  move c first;
                  match in_range c with
                    e :: _ ->
                    attack k e others
                  | [] -> loop k others
                end
        end
      | [] ->
        (* end of turn, collect all creatures from by_pos
                 in case some are dead, and go to the next turn. *)
        loop (k+1) (all_creatures())

    and attack k enemy others =
      enemy.hp <- enemy.hp - (if part2 then if enemy.kind = 'G' then power else 3 else 3);
      let others' =
        if enemy.hp > 0 then others else begin
          if part2 && enemy.kind = 'E' then raise Exit;
          by_pos %- enemy.pos;
          grid.G.!(enemy.pos) <- '.';
          List.filter (fun c -> by_pos %? c.pos) others
        end
      in
      let creatures = all_creatures () in
      if List.for_all (fun c -> c.kind = 'E') creatures ||
         List.for_all (fun c -> c.kind = 'G') creatures then
        let () =
          if X.animate then victory total k power (List.hd creatures) grid by_pos
        in
        (k + (if others' = [] then 1 else 0)) * List.fold_left (fun acc c -> c.hp + acc) 0 creatures
      else
        loop k others'

    in
    if X.animate then Ansi.(printf "%a" clear screen);
    loop 0 (order_creatures creatures)


  let read_input () =
    let g = G.read () in
    let creatures = ref [] in
    let id = ref 0 in
    G.iter (fun pos kind ->
        if kind = 'E' || kind = 'G' then begin
          creatures := {id = !id; kind; hp = 200; pos }::!creatures;
          incr id
        end
      ) g;
    g, order_creatures !creatures
  let solve_part1 () =
    let grid, creatures = read_input () in
    let n = game false 3 grid creatures in
    Solution.printf "%d" n


  let binary_search grid creatures =
    let orig_grid = G.copy grid in
    let copy_creatures l = List.map (fun c -> { c with hp = c.hp }) l in
    let orig_creatures = copy_creatures creatures in

    let rec loop low hi score =
      if low < hi - 1 then
        let mid = low + (hi-low)/2 in
        match
          try Some (game true mid (G.copy orig_grid) (copy_creatures creatures))
          with Exit -> None
        with
          None -> (* an elf died, too low*) loop mid hi score
        | Some score -> loop low mid score
      else
        score
    in
    (* doing more than 200 damages per hit is useless *)
    loop 3 200 (-1)

  let solve_part2 () =
    let grid, creatures = read_input () in
    let n = binary_search grid creatures in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S(struct let animate = false end))
let () = Solution.register_mod ~variant:"animate" (module S(struct let animate = true end))
