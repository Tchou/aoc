open Utils
open Syntax
module S =
struct
  let name = Name.mk "s06"
  (*
        A point will block another one if the line between the two is above the
        diagonal:

                   .........................
                   ........Aaaaaa...........
                   .........#.Bbbbbbbbbb....
                   ..........#b.............
                   ......C....#oo...........
                   ...........oOo...........
                   ...........ooo...........
                   .........................
                   .........................
                   .........................

       Points A and B block O above

       (x', y') - (x, y) = (dx, dy) abs dx >= abs dy   (x',y') blocks x
       horizontally abs dy >= abs dx   (x', y') block y vertically dx < 0,  on
       the left, dy < 0 above

       Once a point has four blocks, it is a valid candidate. We then do a DFS
       and brute-force with all the points, which is not too slow.
*)
  type conf = { left: Grid.position; right: Grid.position;
                top:Grid.position; bottom: Grid.position }
  let dummy_pos = (min_int, min_int)
  let default = { left = dummy_pos; right = dummy_pos; top = dummy_pos; bottom = dummy_pos }

  let ppp fmt (x, y) = Format.fprintf fmt "(%d, %d)" x y
  let ppc fmt c = Format.fprintf fmt "{left=%a, right=%a, top=%a, bottom=%a}" ppp c.left ppp c.right ppp c.top ppp c.bottom

  let (-!) (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
  let mdist p1 p2 = let dx, dy = p1 -! p2 in abs dx + abs dy

  let block p pc conf =
    (* updates the configuration conf in all direction where
       pc blocks p, if it is better than an already existing point *)
    if p = pc then conf else
      let dx, dy = pc -! p in
      let conf =  if abs dx >= abs dy then
          if dx < 0 then { conf with right = pc }
          else {conf with left = pc}
        else conf
      in
      let conf = if abs dy >= abs dx then
          if dy < 0 then { conf with bottom = pc }
          else {conf with top = pc}
        else conf
      in
      conf
  let is_blocked conf =
    conf.left <> dummy_pos &&
    conf.right <> dummy_pos &&
    conf.top <> dummy_pos &&
    conf.bottom <> dummy_pos

  let read_input () =
    Input.fold_scan "%d, %d" (fun acc x y -> (x, y) :: acc) []


  let dfs continue origin all =
    let count = ref 0 in
    let visited = ~%[] in
    let rec loop stack =
      match stack with
        [] -> ()
      | v :: sstack ->
        if visited %? v then loop sstack else begin
          visited.%{v} <- ();
          incr count;
          loop @@ List.fold_left (fun acc d ->
              let w = Grid.(v +! d) in
              if continue w then w::acc else acc
            ) sstack Grid.dir4
        end
    in
    loop [origin];
    !count

  let blocked l =
    List.fold_left (fun acc p ->
        let conf = l |> List.fold_left (fun c pc -> block p pc c) default in
        if is_blocked conf then p::acc else acc) [] l

  let largest_area all =
    let area = ref 0 in
    let lb = blocked all in
    lb |> List.iter (fun p ->
        let n =  dfs
            (fun w -> List.for_all (fun b -> b = w || b = p || mdist p w < mdist w b) all)
            p all
        in
        area := Int.max !area n);
    !area

  let safe_area all =
    let lb = blocked all in
    let origin = List.hd lb in
    dfs
      (fun v -> List.fold_left (fun acc b -> acc + mdist v b) 0 all < 10000)
      origin all

  let solve f =
    let l = read_input () in
    let n = f l in
    Solution.printf "%d" n
  let solve_part1 () = solve largest_area
  let solve_part2 () = solve safe_area

end

let () = Solution.register_mod (module S)