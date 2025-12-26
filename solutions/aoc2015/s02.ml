open Utils
module S =
struct
  let name = Name.mk "s02"

  let read_input () =
    Input.list_scan "%dx%dx%d" (fun l w h -> (l, w, h))

  let paper (l, w, h) =
    let a1 = l * w in 
    let a2 = l * h in 
    let a3 = w * h in
    let s = min a1 (min a2 a3) in 
    s + 2 * (a1 + a2 + a3)


  let ribbon (l, w, h) = 
    let perim = 2 * (l + w + h - (max l (max w h))) in
    let bow = l * w * h in
    bow + perim

  let solve f () =
    let l = read_input () in
    let n = Iter2. (list l |> fold (fun acc d -> acc + f d) 0) in
    Solution.printf "%d" n
  let solve_part1 = solve paper
  let solve_part2 = solve ribbon
end

let () = Solution.register_mod (module S)