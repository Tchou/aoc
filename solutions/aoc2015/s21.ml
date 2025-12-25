open Utils
module S =
struct
  let name = Name.mk "s21"

  let simulate bhp bdam barm php pdam parm =
    let b_attack = max 1 (bdam - parm) in
    let p_attack = max 1 (pdam - barm) in
    let rec loop bhp php =
      let bhp = bhp - p_attack in
      if bhp <= 0 then true
      else let php = php - b_attack in
        if php <= 0 then false
        else loop bhp php
    in loop bhp php

  let weapons = [
    ("Dagger", (8, 4, 0));
    ("Shortsword", (10, 5, 0));
    ("Warhammer", (25, 6, 0));
    ("Longsword", (40, 7, 0));
    ("Greataxe", (74, 8, 0))]
  let armors = [
    ("No Armor", (0, 0, 0));
    ("Leather", (13, 0, 1));
    ("Chainmail", (31, 0, 2));
    ("Splintmail", (53, 0, 3));
    ("Bandedmail", (75, 0, 4));
    ("Platemail", (102, 0, 5));
  ]
  let ring_list = [
    ("Damage+1", (25, 1, 0));
    ("Damage+2", (50, 2, 0));
    ("Damage+3", (100, 3, 0));
    ("Defense+1", (20, 0, 1));
    ("Defense+2", (40, 0, 2));
    ("Defense+3", (80, 0, 3))]
  let rings = 
    ring_list 
    |> List.fold_left (fun acc (r1, (c1, d1, a1)) ->
        ring_list 
        |> List.fold_left (fun acc (r2, (c2, d2, a2)) ->
            if r1 >= r2 then acc 
            else
              (r1 ^ "/" ^ r2, (c1+c2, d1+d2, a1+a2)) ::acc)
          acc)
      (("No Ring", (0,0,0)) :: ring_list)
    |> List.rev


  let explore check valid (bhp, bdam, barm) php =
    let config = [weapons; armors; rings ] in
    let rec loop cfg pdam parm cost =
      if valid cost then
        match cfg with
          [] -> check bhp bdam barm php pdam parm cost
        | (i::items)::ccfg ->
          let name, (c, d, a) = i in
          loop ccfg (pdam+d) (parm+a) (cost+c);
          loop (items::ccfg) pdam parm cost
        | _ -> ()
    in
    loop config 0 0 0
  let find_min_cost boss php =
    let min_cost = ref max_int in
    let f bhp bdam barm php pdam parm cost =
      if simulate bhp bdam barm php pdam parm then
        min_cost := cost
    in
    explore f (fun c -> c < !min_cost) boss php;
    !min_cost

  let maximize_loss boss php =
    let max_cost = ref 0 in
    let f bhp bdam barm php pdam parm cost =
      if cost > !max_cost && not (simulate bhp bdam barm php pdam parm) then
        max_cost := cost
    in
    explore f (fun _ -> true) boss php;
    !max_cost

  let read_input () =
    match
      Input.list_scan "%[^:]: %d" (fun _ i -> i)
    with 
      [hp; d; a] -> (hp, d, a)
    | _ -> assert false


  let solve f () =
    let boss = read_input () in
    let n = f boss 100 in
    Solution.printf "%d" n

  let solve_part1 = solve find_min_cost
  let solve_part2 = solve maximize_loss
end

let () = Solution.register_mod (module S)