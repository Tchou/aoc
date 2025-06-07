open Utils
module S =
struct
  let name = Name.mk "s15"
  (*
  Disc sizes are co-primes, smells like CRT.

  Disc #1 has 17 positions; at time=0, it is at position 5.
  Disc #2 has 19 positions; at time=0, it is at position 8.
  Disc #3 has 7 positions; at time=0, it is at position 1.
  Disc #4 has 13 positions; at time=0, it is at position 7.
  Disc #5 has 5 positions; at time=0, it is at position 1.
  Disc #6 has 3 positions; at time=0, it is at position 0.  

  *)

  let compute_mod i x y = ((x-(y+i) mod x), x)
  let read_input () =
    Input.list_scan
      "Disc #%d has %d positions; at time=0, it is at position %d."
      compute_mod


  let solve o =
    let l = read_input ()  in
    let extra = match o with 
        None -> []
      | Some (x, y) -> let i = 1 + List.length l in [compute_mod i x y]
    in
    let a, _ = Math.solve_crt (l @ extra) in
    Solution.printf "%d" a 
  let solve_part1 () = solve None
  let solve_part2 () = solve (Some (11, 0))
end

let () = Solution.register_mod (module S)