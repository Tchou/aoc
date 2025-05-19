open Utils
module S =
struct
  let name = Name.mk "s13"

  let read_input () =
    Input.list_scan "%d: %d" (fun a b -> (a,b))

  (*
    S is at the top a times (len -1)*2k  
  *)

  let calc_severity l =
    List.fold_left (fun acc (a,b) ->
        if a mod ((b-1)*2) = 0 then acc + a * b else acc
      ) 0 l

  let solve_part1 () =
    let l = read_input () in
    let n = calc_severity l in
    Solution.printf "%d" n


  let sieve l =
    let rec loop i =
      if List.for_all (fun (a, b) ->
          (i + a) mod ((b-1)*2) <> 0) l
      then i 
      else loop (i + 1)
    in
    loop 0

  let solve_part2 () =
    let l = read_input () in
    let n = sieve l in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)