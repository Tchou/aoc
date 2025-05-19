open Utils
open Syntax
module S =
struct
  let name = Name.mk "s12"
  let read_input () =
    let graph = ~%[] in
    Input.fold_scan "%d <-> %[0-9, ]"
      (fun () v s ->
         let l = String.split_on_char ',' s in
         let l = List.map (fun s -> int_of_string (String.trim s)) l in
         List.iter (fun w -> graph.%{v} <- w::(graph.%?{v} or [])) l
      ) ();
    graph

  let visit graph v =
    let visited = ~%[] in
    let rec loop v =
      visited.%{v} <- ();
      List.iter (fun w ->
          if not (visited %? w) then loop w) graph.%{v}
    in
    loop v;
    visited

  let solve_part1 () =
    let graph = read_input () in
    let n = Hashtbl.length (visit graph 0) in
    Solution.printf "%d" n

  let choose graph =
    let exception Found of int in
    try
      Hashtbl.iter (fun k _ -> raise (Found k)) graph;
      None
    with Found k -> Some k

  let count_groups graph =
    let rec loop count =
      match choose graph with
        None -> count
      | Some v ->
        let h = visit graph v in
        Hashtbl.iter (fun k () ->
            let l = graph.%{k} in
            List.iter (fun w -> 
                graph.%{w} <- List.filter ((<>) k) graph.%{w}) l;
            Hashtbl.remove graph k
          ) h;
        loop (count+1)
    in
    loop 0
  let solve_part2 () =
    let graph = read_input () in
    let n = count_groups graph in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)