open Utils
module S =
struct
  let name = Name.mk "s22"


  module Config  =
  struct
    type effect_ = Shield | Poison | Recharge
    type v = {
      player_turn : bool ; (* true : player *)
      player_hp : int;
      player_armor : int;
      player_mana : int;
      boss_hp : int;
      boss_damage : int;
      effects : (effect_ * int) list;
      used_mana : int
    }
    type t = bool (* hard mode *)

    let iter_vertices _b _f = ()

    let pp fmt v =
      let open Format in
      fprintf fmt "%s turn:\n" (if v.player_turn then "Player" else "Boss");
      fprintf fmt "  Player HP: %d\n  Player Armor: %d\n  Player Mana: %d\nBoss  HP: %d\n  Boss Damage: %d\n  Used Mana:  %d\n"
        v.player_hp v.player_armor v.player_mana v.boss_hp v.boss_damage v.used_mana;
      fprintf fmt "  Effects: %a\n--\n%!" 
        (pp_print_list 
           ~pp_sep:(fun fmt () -> fprintf fmt ", ") 
           (fun fmt (e, i) ->
              let s = match e with Shield -> "Shield" | Poison -> "Poison" | Recharge -> "Recharge" in 
              fprintf fmt "%s:%d" s i )) v.effects

    let apply_effect v =
      let el = v.effects in
      let v = 
        List.fold_left (fun acc_v (e,n) ->
            let v = acc_v in
            match e with
              Shield -> if n = 1 
              then { acc_v 
                     with player_armor = 0; 
                          effects = acc_v.effects } 
              else { acc_v with effects = (Shield, n-1) :: acc_v.effects }
            | Recharge -> { acc_v 
                            with player_mana = acc_v.player_mana + 101;
                                 effects = (if n = 1 
                                            then acc_v.effects
                                            else (Recharge,n-1)::acc_v.effects) }
            | Poison -> { acc_v 
                          with boss_hp = acc_v.boss_hp - 3;
                               effects = (if n = 1
                                          then acc_v.effects
                                          else (Poison, n-1)::acc_v.effects)
                        }

          ) { v with effects = [] } el
      in {v with effects = List.sort compare v.effects }

    let iter_succ hard_mode v f =   
      let player_turn = v.player_turn in
      let v = if player_turn && hard_mode then { v with player_hp = v.player_hp - 1 } else v in
      if v.player_hp <= 0 then ()
      else
        let v = apply_effect v in
        let v = { v with player_turn = not player_turn }  in
        if not player_turn then 
          let v'' = { v with 
                      player_hp = v.player_hp - (max 1 (v.boss_damage - v.player_armor))}
          in
          if v''.player_hp > 0 then 
            f (v'', 0);
        else begin
          if v.player_mana >= 53 then f ({v with 
                                          player_mana = v.player_mana - 53; 
                                          used_mana = v.used_mana + 53;
                                          boss_hp = v.boss_hp - 4}, 53);
          if v.player_mana >= 73 then f ({v with 
                                          player_mana = v.player_mana - 73; 
                                          boss_hp = v.boss_hp - 2;
                                          used_mana = v.used_mana + 73;
                                          player_hp = v.player_hp + 2}, 73);
          if v.player_mana >= 113 &&
             not (List.mem_assoc Shield v.effects) then 
            f ({v with 
                player_mana = v.player_mana  - 113;
                used_mana = v.used_mana + 113;
                player_armor = 7;
                effects = (Shield,6)::v.effects}, 113);
          if v.player_mana >= 173 &&
             not (List.mem_assoc Poison v.effects) then 
            f ({v with 
                player_mana = v.player_mana  - 173;
                used_mana = v.used_mana + 173;
                effects = (Poison,6)::v.effects}, 173);
          if v.player_mana >= 229 &&
             not (List.mem_assoc Recharge v.effects) then 
            f ({v with 
                player_mana = v.player_mana  - 229;
                used_mana = v.used_mana + 229;
                effects = (Recharge,5)::v.effects}, 229) 
        end
  end

  module GA = GraphAlgo(Config)
  module HV = Hashtbl.Make(struct type t = Config.v let equal = (=) let hash = Hashtbl.hash end)

  let find_best_strategy part2 boss_hp boss_damage =
    let start = Config.{ boss_hp; boss_damage;
                         player_turn = true;
                         player_hp = 50;
                         player_mana = 500;
                         player_armor = 0;
                         used_mana = 0;
                         effects = []}
    in
    let res = GA.astar (module HV) ~h:(Fun.const 0) part2 start (fun v -> v.boss_hp <= 0) in
    match List.rev res with
      [] -> assert false
    | v :: _ -> v.used_mana

  let read_input () =
    match
      Input.list_scan "%[^:]: %d" (fun _ i -> i)
    with 
      [hp; d] -> (hp, d)
    | _ -> assert false
  let solve part2 () =
    let boss_hp, boss_damage = read_input () in
    let n = find_best_strategy part2 boss_hp boss_damage in
    Solution.printf "%d" n
  let solve_part1 = solve false

  let solve_part2 = solve true
end

let () = Solution.register_mod (module S)