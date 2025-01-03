open Utils
module S =
struct
  let name = Name.mk "s09"

  let read_input () =
    Scanf.sscanf (Input.read_line ())
      "%d players; last marble is worth %d points"
      (fun d1 d2 -> (d1, d2))

  let play num_players last_marble =
    let players = Array.make num_players 0 in
    let rec loop p n current =
      if n <= last_marble then
        let current =
          if n mod 23 = 0 then
            let current = Dll.backward 7 current in
            let v, current = Dll.pop current in
            let () = players.(p) <- players.(p) + n + v in
            current
          else
            let current = Dll.next current in
            Dll.insert_after current n
        in
        loop ((p+1) mod num_players) (n+1) current
    in
    loop 0 1 Dll.(singleton 0);
    Array.fold_left (max) players.(0) players

  let solve c =
    let num_players, last_marble = read_input () in
    let n = play num_players (c * last_marble) in
    Solution.printf "%d" n

  let solve_part1 () = solve 1

  let solve_part2 () = solve 100
end

let () = Solution.register_mod (module S)