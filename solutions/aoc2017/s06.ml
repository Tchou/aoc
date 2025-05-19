open Utils
open Syntax
module S =
struct
  let name = Name.mk "s06"
  let read_input () =
    Input.read_line ()
    |> String.split_on_char '\t'
    |> List.map int_of_string
    |> Array.of_list

  let distribute part2 bank =
    let memo = ~%[] in
    let len = Array.length bank in
    let cmp (i, v) (j, w) =
      let c = Int.compare v w in
      if c <> 0 then c else -Int.compare i j
    in
    let first_repeat = ref None in
    let first_repeat_round = ref (-1) in
    let rec loop bank c =
      match !first_repeat, memo %? bank with
        Some b, _  when bank = b -> assert part2; 
        c - !first_repeat_round
      | _, true when not part2 -> c
      | o, present ->
        if Option.is_none o && part2 && present then begin
          first_repeat_round := c; first_repeat := Some (Array.copy bank)
        end;
        if not present then memo.%{Array.copy bank} <- ();
        begin
          let i, n = Iter.(max iarray ~compare:cmp bank) in
          let d = n / len in
          let r = n mod len in
          bank.(i) <- 0;
          for j = 0 to len-1 do
            bank.(j) <- bank.(j) + d;
          done;
          for j = 1 to r do
            bank.((i+j) mod len) <- bank.((i+j) mod len) + 1;
          done;
          loop bank (c+1)
        end;
    in
    loop (Array.copy bank) 0


  let solve part2 =
    let bank = read_input () in
    let n = distribute part2 bank in
    Solution.printf "%d" n

  let solve_part1 () = solve false

  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)