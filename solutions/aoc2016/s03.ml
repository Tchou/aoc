open Utils
module S =
struct
  let name = Name.mk "s03"

  let sort3 x y z =
    let x, z = if x > z then z, x else x, z in
    let x, y = if x > y then y, x else x, y in
    let y, z = if y > z then z, y else y, z in
    x, y, z
  
  let read_input () =
    Input.list_scan " %d %d %d" (fun x y z -> (x, y, z))


  let is_triangle (x,y,z) = 
    let x, y, z = sort3 x y z in
    x + y > z

  let count_triangles l = Iter.(l |> list |> count_if is_triangle)


  let rec transform l acc =
    match l with
      (x1,y1,z1)::(x2,y2,z2)::(x3,y3,z3)::ll ->
      transform ll ((z1, z2, z3)::(y1, y2, y3)::(x1, x2, x3)::acc)
    |[] -> acc
    | _ -> assert false

  let solve part1 =
    let l = read_input () in
    let l = if part1 then l else transform l [] in
    let n = count_triangles l in
    Solution.printf "%d" n

  let solve_part1 () = solve true
  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S)