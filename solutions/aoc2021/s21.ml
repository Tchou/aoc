open Utils
module S =
struct
  let name = Name.mk "s21"

  type player = { mutable score : int;
                  mutable pos : int;
                }

  let next_pos pos die =
    (((pos - 1) + (die mod 10)) mod 10)+1

  let rec play i dice players =
    if players.(0).score >= 1000 then
      players.(1).score * (dice - 1)
    else if players.(1).score >= 1000 then
      players.(0).score * (dice - 1)
    else
      let pos = players.(i).pos in
      let vdice = 3*dice + 3 in
      let npos = next_pos pos vdice in
      players.(i).score <- players.(i).score + npos;
      players.(i).pos <- npos;
      play (1-i) ((dice+3) mod 100) players


  let read_player () =
    Scanf.sscanf (Input.read_line()) "Player %d starting position: %d" (fun _ b -> b)
  let read_input () =
    let p1 = read_player () in
    let p2 = read_player () in
    [|
      {score = 0; pos = p1 };
      {score = 0; pos = p2 };
    |]

  let solve_part1 () =
    let players = read_input () in
    play 0 1 players
    |> Solution.printf "%d"

  (* We observe that backtracking individual
     dices score is not needed. Combining the result
     of 3 dices gives   *)
  let dice_results = [|3; 4; 5; 6; 7; 8; 9|]
  (* With the following frequencies. (e.g. there are
     7 ways to get a 6, 222,321,123,312,...)
     but only one to get 3: 111
  *)
  let dice_freq =    [|1; 3; 6; 7; 6; 3; 1|]

  (* we use a simple backtracking algorithm that goes through
     these possibilities.
  *)
  let quantum_play players limit =
    let rec play round0 pos0 score0 pos1 score1 =
      if score0 >= limit then
        (1, 0)
      else if score1 >= limit then
        (0, 1)
      else
        let total0 = ref 0 in
        let total1 = ref 0 in
        for j = 0 to 6 do
          let vdice = dice_results.(j) in
          let p, score = if round0 then pos0, score0 else pos1, score1 in
          let npos = next_pos p vdice in
          let nscore = score + npos in
          let c0, c1 = if round0 then
              play false npos nscore pos1 score1
            else
              play true pos0 score0 npos nscore
          in
          let freq = dice_freq.(j) in
          total0 := !total0 + freq * c0;
          total1 := !total1 + freq * c1
        done;
        (!total0, !total1)
    in
    play true players.(0).pos 0 players.(1).pos 0

  let solve_part2 () =
    let players = read_input () in
    let (c0, c1) = quantum_play players 21 in
    Solution.printf "%d" (max c0 c1)
end

let () = Solution.register_mod (module S)