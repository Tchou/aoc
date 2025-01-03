open Utils
open Syntax
module S =
struct
  let name = Name.mk "s20"

  module Re =
  struct
    type t =
      | Char of Grid.position
      | Concat of t list
      | Alt of t list (* Epsilon is Alt [] *)

    let dirs = Grid.['E', east; 'W', west; 'N', north; 'S', south ]
    let read s =
      let rec loop i stack =
        match s.[i], stack with
          '^', ([],[])::[] -> loop (i+1) stack
        | 'A'..'Z' as c, (cur,l)::stack' ->
          loop (i+1) (((Char (List.assoc c dirs))::cur,l)::stack')
        | '(', _ -> loop (i+1) (([],[])::stack)
        | '|', (cur,l)::stack' ->
          let re = Concat (List.rev cur) in
          let rel = if s.[i+1] = ')' then (Alt[])::re::l else re::l in
          loop (i+1) (([], rel)::stack')
        | ')', (cur, l)::(cur', l')::stack' ->
          let re = Alt ((Concat (List.rev cur))::l)in
          loop (i+1) ((re::cur', l')::stack')
        | '$', (cur, [])::[] -> Concat (List.rev cur)
        | _ -> assert false
      in
      loop 0 [([],[])]
  end

  let read_input () =
    let line = Input.read_line () in
    Re.read line

  let map_positions re =
    let graph = ~%[] in
    let rec loop pos re =
      let l =
        match re with
        | Re.Char dir ->
          let pos' = Grid.(pos +! dir) in
          graph.%{pos} <- pos':: (graph.%?{pos} or []);
          graph.%{pos'} <- pos:: (graph.%?{pos'} or []);
          [ pos' ]
        | Concat rel ->
          rel
          |> List.fold_left (fun acc_pos re ->
              acc_pos
              |> List.fold_left (fun acc p ->
                  List.rev_append (loop p re) acc) []
            ) [ pos]
        | Alt l ->
          List.fold_left (fun acc re' ->
              List.rev_append (loop pos re') acc) [] l
      in List.sort_uniq compare l
    in
    let l = loop (0,0) re in
    graph, l

  let bfs f graph =
    let queue = Queue.create () in
    let () = Queue.add (0,0) queue in
    let dist = ~%[ (0,0), 0 ] in
    while not (Queue.is_empty queue) do
      let v = Queue.pop queue in
      (f v dist.%{v});
      (graph.%?{v} or [])
      |> List.iter (fun w ->
          if not (dist %? w) then begin
            dist.%{w} <- dist.%{v} + 1;
            Queue.push w queue
          end
        );
    done

  let num_doors_max_path graph l =
    let map = ~%(List.map (fun x -> x,()) l) in
    let ndoors = ref 0 in
    bfs (fun v d ->
        if map %? v then
          ndoors := max !ndoors d;
      ) graph;
    !ndoors

  let more_than_1000_doors graph _ =
    let ndoors = ref 0 in
    bfs (fun v d -> if d >= 1000 then incr ndoors)
      graph;
    !ndoors

  let solve f =
    let re = read_input () in
    let graph, l = map_positions re in
    let n = f graph l in
    Solution.printf "%d" n

  let solve_part1 () = solve num_doors_max_path
  let solve_part2 () = solve more_than_1000_doors

end

let () = Solution.register_mod (module S)