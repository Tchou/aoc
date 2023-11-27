open Utils
open Syntax
let debug = false

module S =
struct
  let name = Name.mk "s22"

  type brick = {x : Interval.t; y : Interval.t; z : Interval.t }

  let cap cube1 cube2 =
    let open Interval in
    let*& x = cap cube1.x cube2.x in
    let*& y = cap cube1.y cube2.y in
    let*& z = cap cube1.z cube2.z in
    Some {x;y;z } (* None ⇒ empty intersection *)

  let intersects b1 b2 =
    match cap b1 b2 with None -> false | _ -> true

  let pp ppf b = Ansi.fprintf ppf
      "{x=%a; y=%a; z=%a}"
      Interval.pp b.x
      Interval.pp b.y
      Interval.pp b.z

  let mk_bricks xl yl z acc =
    List.fold_left (fun acc x ->
        List.fold_left (fun acc y ->
            {x; y; z}::acc) acc yl) acc xl

  let sort_increasing =
    List.sort (fun a b ->
        compare a.z.Interval.inf b.z.Interval.inf)
  let sort_decreasing =
    List.sort (fun a b ->
        compare b.z.Interval.sup a.z.Interval.sup)

  module BrickSet = Set.Make(struct type t = brick let compare = compare end)
  let load_input () =
    Input.fold_scan "%d,%d,%d~%d,%d,%d%[^ ]"
      (fun acc x1 y1 z1 x2 y2 z2 _ ->
         let x = Interval.{inf=x1; sup=x2+1} in
         let y = Interval.{inf=y1; sup=y2+1} in
         let z = Interval.{inf=z1; sup=z2+1} in
         {x; y; z}::acc) []


  (*
    tests whether a is above b. Assumes a is higher.
  *)
  let is_above a b =
    match Interval.cap a.x b.x, Interval.cap a.y b.y with
      None, _ | _, None -> false
    | Some _, Some _ -> true

  let resting_height = function
      [] -> 1
    | b :: _ -> b.z.Interval.sup

  let rec remove_high_blocks limit blocks acc =
    match blocks with
      [] -> [], acc
    | b :: bblocks ->
      if b.z.Interval.sup > limit then
        remove_high_blocks limit bblocks (b::acc)
      else
        blocks, acc

  let fall_until blocks a =
    let rec loop blocks a  acc moved =
      match blocks with
        [] -> a::acc, moved
      | b :: bblocks ->
        if not (is_above a b) then
          let z_max = resting_height bblocks in
          let old_inf = a.z.Interval.inf in
          let a = {a with z = Interval.make_length z_max (Interval.length a.z)} in
          loop bblocks a (b::acc) (moved || (old_inf > z_max))
        else
          let z_max = resting_height blocks in
          let old_inf = a.z.Interval.inf in
          let a = {a with z = Interval.make_length z_max (Interval.length a.z)} in
          a::b::List.rev_append bblocks acc, (moved || old_inf > z_max)
    in
    let blocks = sort_decreasing blocks in
    let old_inf = a.z.Interval.inf in
    let blocks, acc = remove_high_blocks old_inf blocks [] in
    let z_max = resting_height blocks in
    assert (old_inf >= z_max);
    let a = {a with z = Interval.make_length z_max (Interval.length a.z)} in
    loop blocks a acc (old_inf > z_max)


  let grow_up b =
    { b with z = Interval.{ b.z with sup = b.z.sup + 1}}
  let grow_down b =
    { b with z = Interval.{ b.z with inf= b.z.inf - 1}}
  let collect_info blocks =
    let blocks = sort_increasing blocks in
    let fallen = List.fold_left (fun acc b -> fst (fall_until acc b)) [] blocks in
    assert (List.length fallen = List.length blocks);
    let blocks_below = ~%[] in
    let blocks_above = ~%[] in
    let fill_table table f =
      let intersects orig b other =
        orig <> other && intersects b other
      in
      List.iter (fun b ->
          let b_sup = f b in
          let matched = List.filter (intersects b b_sup) fallen in
          table.%{b} <- BrickSet.(union (of_list matched) (table.%?{b} or BrickSet.empty))
        ) fallen
    in
    fill_table blocks_above grow_up;
    fill_table blocks_below grow_down;
    let multiple_table = ~%[] in
    (* Add all blocks that suport a block but are not alone. *)
    Hashtbl.iter (fun a bset ->
        if BrickSet.cardinal bset >= 2 then
          BrickSet.iter (fun b -> 
              multiple_table.%{b} <- ()) bset
      ) blocks_below;
    (* Remove blocks that are the sole upporter of another block.
       We need to do both phases in order since e.g.
       AAAA  DDD
       B  CCCC
       A is supported by B and C in the same set, but C is
       the only one supporting D.
    *)
    Hashtbl.iter (fun _ bset ->
        if BrickSet.cardinal bset = 1 then
          let b = BrickSet.choose bset in
          multiple_table %- b
      ) blocks_below;
    List.filter (fun b ->
        let res =
          match blocks_above.%?{b} with
            None -> assert false
          | Some s when BrickSet.is_empty s -> true
          | _ ->  multiple_table %? b in
        res
      ) fallen

  let solve_part1 () =
    let blocks = load_input () in
    let can_be_removed = collect_info blocks in
    Ansi.printf "%d\n" (List.length can_be_removed) 


  let count_movable blocks =
    let blocks = sort_increasing blocks in
    let fallen = List.fold_left (fun acc b -> fst (fall_until acc b)) [] blocks in
    let rec loop before after num_moved =
      match after with
        [] -> num_moved
      | b :: aafter ->
        let _, num_moved =
          List.fold_left (fun (acc,n) b ->
              let acc, moved = fall_until acc b in
              if moved then acc, n+1
              else acc, n
            ) ([], num_moved) (List.rev_append before aafter)
        in
        loop (b::before) aafter num_moved
    in
    loop [] (sort_increasing fallen) 0

  let solve_part2 () =
    let blocks = load_input () in
    count_movable blocks
    |> Ansi.printf "%d\n"
end

let () = Solution.register_mod (module S)