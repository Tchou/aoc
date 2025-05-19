open Utils
open Syntax
module S =
struct
  let name = Name.mk "s07"

  let read_input () =
    let table = ~%[] in
    Input.fold_scan "%s (%d) %[->]%[a-z, ]" 
      (fun acc n w _ l ->
         table.%{n} <- (w, 
                        if l = "" then [] 
                        else l |> String.split_on_char ',' |> List.map String.trim)
      ) ();
    table

  type 'a tree = { name : string; value : 'a; children : 'a tree list }

  let build_tree table =
    let ntable = ~%[] in
    let stack = ref [] in
    let rec loop name =
      match ntable.%{name} with
        t -> t
      | exception Not_found ->
        let value, children = try table.%{name} with Not_found -> failwith name in
        let l = List.map loop children in
        let node = {name; value; children = l} in
        ntable.%{name} <- node;
        stack := name :: !stack;
        node
    in
    Hashtbl.iter (fun n _ -> 
        if not (ntable %? n) then ignore (loop n)) table;
    ntable.%{List.hd !stack}

  let solve_part1 () =
    let table = read_input () in
    let { name; _ } = build_tree table in
    Solution.printf "%s" name

  let rec compute_weight {name; value=w; children } =
    let w', l' = List.fold_left (fun (accw, accl) node ->
        let nw, node' = compute_weight node in
        (accw + nw, node'::accl)) (0, []) children
    in
    w+w', {name; value = (w, w+w'); children = List.rev l'}

  let total_weight n = snd n.value
  let weight n = fst n.value

  let fix_weight n1 n2 =
    weight n1 - (total_weight n1 - total_weight n2)

  let find_unballanced_level l =
    match List.sort (fun n1 n2 -> Int.compare (total_weight n1) (total_weight n2)) l with
      [ ] | [ _ ] | [ _; _ ] -> None
    | (n1 :: n2 :: _) as l ->
      if total_weight n1 <> total_weight n2 then Some (fix_weight n1 n2) else
        match List.rev l with
          n1 :: n2 :: _ when total_weight n1 <> total_weight n2 -> Some (fix_weight n1 n2)
        | _ -> None

  let find_unballanced t =
    let rec loop {name ; value=(w, total); children } =
      match List.find_map loop children with
        Some n -> Some n
      | None -> find_unballanced_level children
    in
    loop t 
  let solve_part2 () =
    let table = read_input () in
    let t = build_tree table in
    match find_unballanced (compute_weight t |> snd) with
      None -> assert false
    | Some n -> Solution.printf "%d" n

end

let () = Solution.register_mod (module S)