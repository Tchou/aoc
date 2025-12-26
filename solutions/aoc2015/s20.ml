open Utils
module S =
struct
  let name = Name.mk "s20"

  let sum_divisors part1 n =
    let rec loop d acc =
      let d50 = d * 50 in
      let d2 = d * d in
      if d2 >= n then 
        if d2 = n && (part1 || d50 >= n)
        then d + acc else acc
      else if n mod d = 0 then
        let d1 = if part1 || d50 >= n then d else 0 in
        let nd = n/d in
        let d2 = if part1 || nd * 50 >= n then nd else 0 in
        loop (d + 1) (d1 + d2 + acc)
      else
        loop (d + 1) acc
    in
    loop 1 0 

  let read_input () = Input.read_line () |> int_of_string
  let solve factor part2 () = 
    let target = read_input () / factor in
    Iter2.(range int ~start:1 target
           |> find (fun i ->
               let n = sum_divisors part2 i in
               n >= target
             )) |> Solution.printf "%d"

  let solve_part1 = solve 10 true
  let solve_part2 = solve 11 false
end

let () = Solution.register_mod (module S)