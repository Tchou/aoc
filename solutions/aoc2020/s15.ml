open Utils
module S =
struct
  let name = Name.mk "s15"

  let read_input () =
    Input.read_line ()
    |> String.split_on_char ','
    |> List.map int_of_string

  let play limit l =
    let last_times = Array.make limit 0 in
    let say n num =
      let res =
        let l = last_times.(num) in
        if l == 0 then 0 else n - l
      in
      last_times.(num) <- n;
      res
    in
    let num_to_say, turn =
      List.fold_left (fun (_, turn) n ->
          (say turn n, turn+1))
        (0, 1) l
    in
    let rec loop turn num_to_say =
      if turn = limit
      then num_to_say
      else loop (turn+1) (say turn num_to_say)
    in
    loop turn num_to_say

  let solve n =
    let l = read_input () in
    let n = play n l in
    Solution.printf "%d" n

  let solve_part1 () = solve 2020
  let solve_part2 () = solve 30000000
end

let () = Solution.register_mod (module S)