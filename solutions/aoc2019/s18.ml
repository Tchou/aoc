open Utils
open Syntax
module S =
struct
  let name = Name.mk "s18"
  module G = Grid.BytesGrid
  module GridGraph : GRAPH with type t = G.t and type v = Grid.position * char = struct
    type t = G.t
    type v = Grid.position * char
    let iter_vertices grid f =
      G.iter (fun pos c ->
          if c <> '#' then f (pos, c))
        grid
    let iter_succ grid (v,_) f =
      Grid.dir4 |> List.iter (fun d ->
          let w = Grid.(v +! d) in
          if G.inside grid w then
            let c = grid.G.!(w) in
            if c <> '#' then f ((w, c), 1)
        )
  end
  module GAlgo = GraphAlgo(GridGraph)
  (*
  A config is 1 or 4 chars (8 to 32 bits) + 26 bit for the keys =
  at most 58 bits we can pack it in an integer.
  *)
  let key_mask = (1 lsl 26) - 1
  let get_keys v = v land key_mask
  let get_doors v = v land key_mask
  let get_distance v = v lsr 26

  let pack_dist_doors d doors = (d lsl 26) lor doors
  let pack_2chars c1 c2 = (Char.code c1 lsl 8) lor Char.code c2
  let unpack_2chars c = Char.unsafe_chr (0xff land (c lsr 8)), (Char.unsafe_chr (0xff land c))

  type graph = { distances : int array;
                 reachable : string array;
                 origin : int;
               }

  let pp_letters fmt l =
    let l = List.map (fun (_, c) -> String.make 1 c) l in
    Format.fprintf fmt "%s" String.(concat ", " l)

  let bit_fields base l =
    List.fold_left (fun acc x ->
        let b = Char.code x - Char.code base in
        let b = 1 lsl b in
        acc lor b
      ) 0 l
  let init part1 grid =
    let keys = ref [] in
    grid
    |> G.iter (fun ((x, y) as p) c ->
        if c >= 'a' && c <= 'z' then keys := (p,c) :: !keys
        else if c = '@' then
          if part1 then keys := (p,c) :: !keys
          else if grid.G.!(x, y-2) <> '>' then begin
            let open Grid in
            grid.G.!(p) <- '#';
            grid.G.!(p +! east) <- '#';
            grid.G.!(p +! north) <- '#';
            grid.G.!(p +! south) <- '#';
            grid.G.!(p +! west) <- '#';
            let q = p +! north_west in
            grid.G.!(q) <- '=';
            keys := (q,'=') :: !keys;
            let q = p +! north_east in
            grid.G.!(q) <- '>';
            keys := (q,'>') :: !keys;
            let q = p +! south_west in
            grid.G.!(q) <- '?';
            keys := (q,'?') :: !keys;
            let q = p +! south_east in
            grid.G.!(q) <- '@';
            keys := (q,'@') :: !keys;
          end);
    let keys = List.sort (Compare.snd) !keys in
    let graph = ~%[] in
    let rec loop = function
        [] -> ()
      | (_, c1) as v :: ll ->
        let t = GAlgo.dijkstra grid v ll in
        Hashtbl.iter (fun (_, c2) (d, path) ->
            let p = List.filter (fun (_, c) -> c >= 'A' && c <= 'Z') (List.flatten path) in
            let info = d, bit_fields 'A' (List.map snd p) in
            graph.%{pack_2chars c1 c2} <- info;
            graph.%{pack_2chars c2 c1} <- info;
          ) t;
        loop ll
    in loop keys;
    let distances = Array.make (256*256) (-1) in
    let () = Hashtbl.iter (fun k (d, doors) -> distances.(k) <- pack_dist_doors d doors) graph in
    {  distances;
       reachable =
         Array.init (if part1 then 1 else 4)
           (fun i ->
              let orig = Char.chr (Char.code '@' - i) in
              let keys = Hashtbl.fold (fun c (d,_) acc ->
                  let c1, c2 = unpack_2chars c in
                  if c1 < c2 && c1 = orig && c2 >= 'a' && c2 <= 'z' && d < max_int then c2::acc
                  else acc
                ) graph []
              in
              keys |> String.implode);
       origin = if part1 then 0x40 else 0x3d3e3f40;
    }

  module Config (S : sig val n : int end) : sig
    include GRAPH with type v = int and type t = graph
    val pack : int -> int -> int
    val targets : t -> v list
  end = struct
    type v = int
    type t = graph
    let dim = S.n
    let vertex_mask = 1 lsl (dim * 8) - 1
    let get_byte v i = (v lsr (i lsl 3)) land 0xff
    let set_byte v i b =
      let b = (0xff land b) lsl (i lsl 3) in
      let imask = (lnot (0xff lsl (i lsl 3))) land vertex_mask in
      let v = v land imask in
      v lor b
    let get_chars v = (v lsr 26) land vertex_mask
    let pack chars keys = ((chars land vertex_mask) lsl 26) lor keys
    let iter_vertices _ _ = assert false
    let iter_succ graph v f =
      let chars = get_chars v in
      let keys = get_keys v in
      for i = 0 to dim - 1 do
        let chari = Char.unsafe_chr (get_byte chars i) in
        let s = graph.reachable.(i) in
        for j = 0 to String.length s - 1 do
          let c = s.[j] in
          if c <> chari then
            let k = pack_2chars chari c in
            let d_doors = graph.distances.(k) in
            let doors = get_doors d_doors in
            if d_doors != -1 && (keys land doors) = doors then begin
              let nchar = set_byte chars i (Char.code c) in
              let nkeys = (1 lsl (Char.code c - Char.code 'a')) lor keys in
              let nv = (pack nchar nkeys, get_distance d_doors) in
              f nv
            end
        done
      done

    let targets graph =
      let all_keys =
        graph.reachable |> Array.fold_left (fun acc k -> (String.explode k) @ acc) []
      in
      let all_keys = List.sort_uniq Char.compare all_keys in
      let num_keys = List.length all_keys in (* remove the origins *)
      let full_keys = (1 lsl num_keys) - 1 in
      let targets = ref [] in
      let rec gen_targets i acc =
        if i >= dim then targets := (pack acc full_keys)::!targets
        else
          graph.reachable.(dim - i - 1)
          |> String.iter (fun c ->
              let nacc = (acc lsl 8) lor Char.code c in
              gen_targets (i+1) nacc
            )
      in
      gen_targets 0 0;
      !targets
  end
  let bit_set v =
    let v = ref v in
    let c = ref 0 in
    while !v <> 0 do
      v := !v land (!v - 1);
      incr c;
    done;
    !v
  let min_dist n graph =
    let module C = Config(struct let n = n end) in
    let module CAlgo = GraphAlgo(C) in
    let origin = C.pack graph.origin 0 in
    let paths = CAlgo.dijkstra ~first:true graph origin (C.targets graph)in
    let m = ref max_int in
    Hashtbl.iter (fun _ (d, _) -> if d < !m then m := d) paths;
    !m

  let solve part1 =
    let grid = G.read () in
    let graph = init part1 grid in
    let n = min_dist (if part1 then 1 else 4) graph in
    Solution.printf "%d" n

  let solve_part1 () = solve true
  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S)