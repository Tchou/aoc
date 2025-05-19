open Utils
module S =
struct
  let name = Name.mk "s24"

(*
  Just need to precisely follow the rules of the game for part 1. For part two,
  we can do a binary search between starting between 0 and twice the maximal
  resistance of any group. One interesting trap, is that for some low values
  there can be ties:
  - no one gets selected during selection phase
  - no one gets killed during the attack phase

  if one of these conditions occurs, the game will loop indefinitely, since no
  progress is made.
*)

  type group = {
    id : int;
    kind : string;
    mutable units : int;
    hp : int;
    attack : int;
    attack_type : string;
    initiative : int;
    weakness : string list;
    immunity : string list
  }
  let pp_full fmt g =
    Format.fprintf fmt "{%s (%d): %d, %d, %d, %s, %d, W[%s], I[%s]}" g.kind g.id g.units g.hp g.attack g.attack_type g.initiative
      String.(concat "," g.weakness)
      String.(concat "," g.immunity)
  let pp fmt g =
    Format.fprintf fmt "{%s (%d): %d, %d, %d, %s, %d}" g.kind g.id g.units g.hp g.attack g.attack_type g.initiative

  let parse_wi prefix s =
    let len = String.length prefix in
    let s = String.sub s len (String.length s - len) in
    String.split_on_char ',' s
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
    |> List.sort compare

  let parse_weak_immune s =
    let open String in
    match String.split_on_char ';' s |> List.map trim with
      [s] when starts_with ~prefix:"weak to" s ->
      parse_wi "weak to" s, []
    | [s] when starts_with ~prefix:"immune to" s ->
      [], parse_wi "immune to" s
    | s1::s2::[] when starts_with ~prefix:"weak to" s1 ->
      parse_wi "weak to" s1,
      parse_wi "immune to" s2
    | s1::s2::[] when starts_with ~prefix:"immune to" s1 ->
      parse_wi "weak to" s2,
      parse_wi "immune to" s1
    | _ -> assert false

  let parse_group id kind s =
    let s, (weakness, immunity) =
      match String.split_on_char '(' s
            |> List.map (String.split_on_char ')')
            |> List.flatten
      with
        [ s ] -> s, ([], [])
      | s1 :: s2 :: s3::[] ->
        s1 ^ (String.trim s3), parse_weak_immune s2
      | _ -> assert false
    in
    Scanf.sscanf s
      "%d units each with %d hit points with an attack that does %d %s damage at initiative %d"
      (fun units hp attack attack_type initiative ->
         {id;kind; units; hp; attack; attack_type; initiative; weakness; immunity})

  let read_input () =
    let name1 = Input.read_line () |> String.split_on_char ':' |> List.hd in
    let id = ref 0 in
    let group1 =
      InputUntil.fold_lines (fun acc s ->
          incr id;
          if s = "" then (false, acc)
          else (true, (parse_group !id name1 s)::acc)) []
    in
    let () = id := 0 in
    let name2 = Input.read_line () |> String.split_on_char ':' |> List.hd in
    let group2 =
      Input.fold_lines (fun acc s ->
          incr id;
          if s = "" then acc
          else (parse_group !id name2 s)::acc) []
    in
    (name1, List.rev group1), (name2, List.rev group2)

  let effective_power g = g.units * g.attack

  let damage g def =
    if List.mem g.attack_type def.immunity then 0
    else
      let power = effective_power g in
      if List.mem g.attack_type def.weakness then 2*power
      else power

  let comp_targets g o1 o2 =
    let d1 = damage g o1 in
    let d2 = damage g o2 in
    let c = Int.compare d2 d1 in
    if c <> 0 then c else
      let e1 = effective_power o1 in
      let e2 = effective_power o2 in
      let c = Int.compare e2 e1 in
      if c <> 0 then c else
        Int.compare o2.initiative o1.initiative

  let select_target g others chosen =
    let others' = List.filter (fun o ->
        o.kind <> g.kind && (* choose an enemy *)
        o.units > 0 && (* which is alive *)
        damage g o > 0 && (* to which we can deal damages *)
        (* and which is not already chosen *)
        not (List.exists (function (_, Some o') -> o' = o | _ -> false) chosen)
      ) (List.fold_left (fun acc (x, _) -> x::acc) others chosen)
    in
    let others' = List.sort (comp_targets g) others' in
    match others' with
      [] -> None
    | o:: _ -> Some o

  let comp_selection g1 g2 =
    let c = Int.compare (effective_power g2) (effective_power g1) in
    if c <> 0 then c
    else Int.compare g2.initiative g1.initiative

  let target_selection groups =
    let groups = List.sort comp_selection groups in
    let rec loop l chosen =
      match l with
        [] -> List.rev chosen
      | g :: others ->
        if g.units = 0 then loop others chosen else
          let t = select_target g others chosen in
          loop others ((g,t)::chosen)
    in
    loop groups []


  let attack selection =
    let total_killed = ref 0 in
    selection
    |> List.sort (fun (g1,_) (g2, _) -> Int.compare g2.initiative g1.initiative)
    |> List.iter (function
          (_, None) -> ()
        | g, Some o ->
          let d = damage g o in
          let u = min (d / o.hp) o.units in
          if false then Format.printf "Group %a deals to group %a %d damages, kills %d units\n%!"
              pp g pp o d u;
          total_killed := !total_killed+u;
          o.units <- o.units - u);
    !total_killed

  let score groups =
    groups
    |> List.map (fun g -> g.units)
    |> Iter.(sum list int)

  let fight n1 n2 groups =
    let rec loop () =
      let selection = target_selection groups in
      if List.for_all (fun (_, o) -> o = None) selection then
        "TIE",-1
      else
        let total_killed = attack selection in
        if total_killed = 0 then "TIE", -1
        else
          let g1, g2 = List.partition (fun g -> g.kind = n1) groups in
          if List.for_all (fun g -> g.units = 0) g1 then
            n2, score g2
          else if List.for_all (fun g -> g.units = 0) g2 then
            n1,score g1
          else loop ()
    in
    loop ()

  let solve_part1 () =
    let (n1, g1), (n2, g2) = read_input () in
    let _winner, n = fight n1 n2 (g1@g2) in
    Solution.printf "%d" n

  let find_smallest_boost n1 n2 groups =
    let orig_groups = List.map (fun g -> {g with id = g.id}) groups in
    let max_power =
      2 * (List.map (fun g -> g.units * g.hp) groups
           |> Iter.(max list))
    in
    let rec loop low hi count =
      if low < hi - 1 then begin
        let mid = low + (hi - low)/2 in
        let groups = List.map (fun g ->
            let boost = if g.kind = n1 then mid
              else 0
            in
            {g with attack = g.attack + boost}
          ) orig_groups
        in
        let winner, _ = fight n1 n2 groups in
        if winner = n1 then
          let count' = List.filter_map (fun g -> if g.kind = n1
                                         then Some g.units else None) groups
                       |> Iter.(sum list int)
          in
          loop low mid count'
        else loop mid hi count
      end else count
    in
    loop 0 max_power (-1)

  let solve_part2 () =
    let (n1, g1), (n2, g2) = read_input () in
    let n = find_smallest_boost n1 n2 (g1@g2) in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)