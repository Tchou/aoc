open Utils
open Syntax

module S =
struct
  let name = Name.mk "s19"

  type action =
      Accept
    | Reject
    | Goto of string

  let fields = ['x', 0; 'm', 1; 'a', 2; 's', 3]
  let parse_cond s =
    Scanf.sscanf s "%c%c%d"
      (fun n c d -> (List.assoc n fields),c,d)
  let parse_action s =
    match s with
      "A" -> Accept
    | "R" -> Reject
    | s -> Goto s

  let parse_rule s =
    match String.split_on_char ':' s with
      [ a ] -> (None, parse_action a)
    | [ c; a] -> (Some (parse_cond c), parse_action a)
    | _ -> assert false

  let load_rules () =
    let table = ~%[] in
    InputUntil.fold_lines
      (fun () -> function
           "" -> false, ()
         | s -> true,Scanf.sscanf s "%[^{]{%[^}]}"
                  (fun name r ->
                     let l = String.split_on_char ',' r in
                     let rules = List.map parse_rule l in
                     table.%{name} <- rules)) ();
    table

  let load_parts () =
    Input.fold_scan "{x=%d,m=%d,a=%d,s=%d}"
      (fun acc x m a s -> [|x;m;a;s|]::acc) []
    |> List.rev

  let eval table parts =
    let rec eval_cond p cond =
      match cond with
        None -> true
      | Some (i, c, n) ->
        (if c = '<' then (<) else (>)) p.(i) n
    in
    let find_action p rule =
      snd (List.find (fun (cond, _) -> eval_cond p cond) rule)
    in
    let rec eval_rule rule acc p   =
      match find_action p rule with
        Accept -> p :: acc
      | Reject -> acc
      | Goto n -> eval_rule table.%{n} acc p
    in
    List.fold_left (eval_rule table.%{"in"}) [] parts

  let compute_score =
    List.fold_left (Array.fold_left (+)) 0


  let solve_part1 () =
    let rule_table = load_rules () in
    let parts = load_parts () in
    eval rule_table parts
    |> compute_score
    |> Solution.printf "%d"


  let eval_constr table =
    let rec eval_cond cond =
      match cond with
        None -> None
      | Some (i, c, n) ->
        match c with
          '<' ->
          Some ((i,(1, n)),(i,(n, 4001)))
        | _ (* '>' *) ->
          Some ((i,(n+1, 4001)),(i,(1, n+1)))

    in
    let rec eval_rule rules acc path =
      match rules with
        [] -> acc
      | (cond, action) :: rrules  ->
        let path1, path2 =
          match eval_cond cond with
            None -> path, None
          | Some (c1, c2) -> c1::path, Some (c2::path)
        in
        let acc =
          match action with
            Accept -> (List.rev path1) :: acc
          | Reject -> acc
          | Goto n -> eval_rule table.%{n} acc path1
        in
        match path2 with
          None -> acc
        | Some path2 -> eval_rule rrules acc path2
    in
    eval_rule table.%{"in"} [] []

  let merge_intervals ints =
    let tab = Array.make 4 (1,4001) in
    List.iter (fun (i,(inf, sup)) ->
        let ainf, asup = tab.(i) in
        tab.(i) <- (max inf ainf, min sup asup)) ints;
    Array.fold_left (Agg.Left.prod (fun (i,j) -> j-i)) 1 tab

  let pp ppf l =
    let fields = List.map (fun (a,b) -> (b,a)) fields in
    List.iter (fun (i, c, b, n) ->
        Ansi.fprintf ppf "%c %s%c %d\n"
          (List.assoc i fields) (if b then "" else "!") c n 
      ) l;
    Ansi.fprintf ppf "--\n%!"
  let solve_part2 () =
    let rule_table = load_rules () in
    let _parts = load_parts () in
    eval_constr rule_table
    |> List.fold_left (Agg.Left.sum merge_intervals) 0
    |> Solution.printf "%d"

end

let () = Solution.register_mod (module S)