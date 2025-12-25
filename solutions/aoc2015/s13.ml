open Utils
open Syntax
module S =
struct
  let name = Name.mk "s13"

  let read_input () =
    let cost = ~%[] in
    Input.fold_scan 
      "%s would %s %d happiness units by sitting next to %[^.]."
      (fun () from sign n to_ ->
         let n = if sign = "lose" then -n else n in
         cost.%{from} <- (to_,n) :: (cost.%?{from} or []);
      ) ();
    cost

  let enumerate cost =
    let length = Hashtbl.length cost in
    let m_cost = ref 0 in
    let rec loop v p p_len curr_cost first =
      if p_len = length then
        (let c = curr_cost + List.assoc first cost.%{v} + List.assoc v cost.%{first} in
         if c > !m_cost then m_cost := c)
      else
        cost.%{v} 
        |> List.iter (fun (w, c) ->
            if not (List.mem w p) then
              loop w (w::p) (p_len + 1) (curr_cost + c + List.assoc v cost.%{w}) first
          )
    in
    Hashtbl.iter (fun v _ -> loop v [v] 1 0 v) cost;
    !m_cost

  let add_myself graph =
    let others = graph |> Hashtbl.to_seq_keys |> List.of_seq in
    List.iter (fun o -> 
        graph.%{"me"} <- (o, 0) :: (graph.%?{"me"} or []);
        graph.%{o} <- ("me", 0) :: (graph.%?{o} or []);
      ) others; graph
  let solve patch () =
    let cost = read_input () in
    let n = enumerate (patch cost) in
    Solution.printf "%d" n
  let solve_part1 = solve Fun.id
  let solve_part2 = solve add_myself
end

let () = Solution.register_mod (module S)