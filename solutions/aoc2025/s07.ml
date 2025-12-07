open Utils
open Grid
module S =
struct
  let name = Name.mk "s07"
  let read_input () =
    BytesGrid.read ()


  module CoordSet = Set.Make(struct type t = Grid.position let compare = compare end)
  let step grid set i splitters =
    let splitters = ref splitters in
    let new_set =
      CoordSet.fold (fun pos acc ->
          assert (snd pos = i);
          let below = pos +! south in 
          if not (BytesGrid.inside grid below) then acc
          else 
          if BytesGrid.(grid.!(below) = '^') then begin
            splitters := CoordSet.add below !splitters;
            [ below +! west; below +! east]
            |> List.filter (BytesGrid.inside grid)
            |> CoordSet.of_list
            |> CoordSet.union acc
          end else CoordSet.add below acc
        ) set CoordSet.empty
    in
    new_set, !splitters

  let count_splits grid = 
    let start = BytesGrid.find ((=) 'S') grid in
    assert (snd start = 0);
    let height = BytesGrid.height grid in
    let rec loop set i splitters =
      if i >= height || CoordSet.is_empty set then splitters
      else
        let new_set, splitters = step grid set i splitters in
        loop new_set (i+1) splitters
    in
    loop (CoordSet.singleton start) 0 CoordSet.empty
    |> CoordSet.cardinal


  let solve_part1 () =
    let grid = read_input () in 
    let n = count_splits grid in
    Solution.printf "%d" n

  (* rewrite as a recursive function + memo *)
  let count_timelines grid =     
    let start = BytesGrid.find ((=) 'S') grid in
    assert (snd start = 0);
    let last = BytesGrid.height grid - 1 in
    let memo = Hashtbl.create 16 in
    let rec loop ((_, h) as pos) = 
      if h = last then 1
      else
        match Hashtbl.find_opt memo pos with
          Some n -> n
        | None -> 

          let below = pos +! south in
          if BytesGrid.(grid.!(pos)) <> '^' then
            loop below 
          else
            let left = below +! west in
            let n1 = if BytesGrid.inside grid left then loop left else 0 in 
            let right = below +! east in
            let n2 = if BytesGrid.inside grid right then loop right else 0 in 
            let r = n1 + n2 in
            Hashtbl.add memo pos r; r
    in
    loop start

  let solve_part2 () =
    let grid = read_input () in 
    let n = count_timelines grid in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)