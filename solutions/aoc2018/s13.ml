open Utils
open Syntax
module S (X:sig val animate : bool end)=
struct
  module G = Grid.BytesGrid
  let name = Name.mk "s13"

  type cart = { mutable pos : Grid.position; mutable dir : Grid.dir; mutable turn : int }
  let turns = [| Grid.left90; Fun.id; Grid.right90 |]
  let sym_dirs = List.map2 (fun c d -> (c, d)) ['^';'>';'v';'<'] Grid.dir4
  let sym_repl = ['^','|'; '>', '-'; 'v','|'; '<', '-' ]
  let read_input () =
    let tracks = G.read () in
    let carts = ref [] in
    G.iter (fun pos c ->
        match List.assoc_opt c sym_dirs with
          None -> ()
        | Some dir ->
          carts := {pos;dir; turn=0}::!carts;
          tracks.G.!(pos) <- List.assoc c sym_repl ) tracks;
    tracks, !carts |> List.rev

  let cmp_carts {pos=(x1, y1);_} {pos=(x2, y2);_} =
    let c = Int.compare y1 y2 in
    if c != 0 then c else Int.compare x1 x2

  let curve c ((i, j) as d) =
    let open Grid in
    if c = '/' then
      if j <> 0 then right90 d
      else (* i <> 0 *) left90 d
    else (* c = '\' *)
    if j <> 0 then left90 d
    else right90 d

  let pretty_chars = ~%[
      '<',"◀"; '^',"▲"; '>',"▶"; 'v',"▼";
      '|', "║"; '+', "╬"; '-',"═"
    ]
  let pretty has_left c =
    match c with
      '/' -> if has_left then "╝" else "╔"
    | '\\' -> if has_left then "╗" else "╚"
    | _ -> try pretty_chars.%{c} with Not_found ->
      let s = String.make 1 c in
      pretty_chars.%{c} <- s;
      s

  let buf = Buffer.create 16
  let bfmt = Format.formatter_of_buffer buf
  let ftime = (1.0 /. 60.) *. 30.
  let display (crashes, tracks, carts) =
    Buffer.clear buf;
    Format.pp_print_flush bfmt ();
    let y = ref 0 in
    G.iter_lines (fun b ->
        Bytes.iteri (fun x c ->
            let is_cart, c =
              match List.find_opt (fun cart -> cart.pos = (x, !y)) carts with
                None -> false,c
              | Some cart ->
                true, List.find (fun (_, d) -> d = cart.dir) sym_dirs |> fst
            in
            let left = G.inside tracks (x-1, !y) &&
                       let x = tracks.G.!(x-1, !y) in x ='-'||x='+' in
            let s = pretty left c in
            let is_crash = List.exists (fun c -> c.pos = (x, !y)) crashes in
            if is_crash then Ansi.(fprintf bfmt "%a✖%a" bbg red clear color)
            else if is_cart then Ansi.(fprintf bfmt "%a%s%a" bfg cyan s clear color)
            else Ansi.fprintf bfmt "%s" s
          ) b;
        incr y;
        Ansi.fprintf  bfmt "\n") tracks;
    Format.pp_print_flush bfmt ();
    Ansi.(printf "%a%s\n%!" clear cursor (Buffer.contents buf));
    Unix.sleepf 0.0

  let has_collision c others =
    List.partition (fun o -> o.pos = c.pos) others

  let race anim part1 tracks carts =
    let rec loop carts moved =
      match carts with
        ({pos;dir;turn} as cart) :: others ->
        let c = tracks.G.!(pos) in
        let dir' =
          match c with
            '/' | '\\' -> curve c dir
          | '+' -> let f = turns.(cart.turn) in
            cart.turn <- (cart.turn + 1) mod 3;
            f dir
          | _ -> dir
        in
        let pos' = Grid.(pos +! dir') in
        (* move the car to a new position *)
        cart.pos <- pos'; cart.dir <- dir';
        begin (* check collision with remaining carts as well as those already moved
                 during the tick.*)
          match has_collision cart others, has_collision cart moved with
          (* no collision, continue *)
          | ([], _), ([], _) -> loop others (cart::moved)
          (* one collision, in part 1, stop *)
          | ((c::_, _),_)|(_, (c::_, _)) when part1 ->
            if anim then display ([c], tracks, carts@moved);
            c.pos
          (* several collisions in part 2, continue with the remaining carts *)
          | (col_todo, rem_todo), (col_moved, rem_moved) ->
            if anim then display (cart::col_todo@col_moved, tracks, rem_todo@rem_moved);
            loop rem_todo rem_moved
        end

      | [] -> (* end of the tick *)
        match moved with
          [ cart ] -> (* A single cart remains, can only happen in part 2 *)
          if anim then display ([], tracks, moved);
          cart.pos
        | _ ->
          (* some carts remain, sort them from top-left to bottom-right and continue *)
          if anim then display ([], tracks, moved);
          loop (List.sort cmp_carts moved) []
    in
    if anim then Ansi.(printf "%a" clear screen );
    loop carts []
  let solve part1 =
    let tracks, carts = read_input () in
    let x, y = race X.animate part1 tracks carts in
    Solution.printf "%d,%d" x y
  let solve_part1 () = solve true
  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S(struct let animate = false end))
let () = Solution.register_mod ~variant:"animate" (module S(struct let animate = true end))