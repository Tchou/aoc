open Utils
open Syntax


module S =
struct
  let name = Name.mk "s06"

  let most_frequent compare pos l =
    let h = ~%[] in
    List.iter (fun s ->
        let c = s.[pos] in
        h.%{c} <- 1 + (h.%?{c} or 0)
      ) l;
    Iter.(max Hashtbl.to_seq ~compare) h
    |> fst

  let compare_max a b = compare (snd a) (snd b)
  let compare_min a b = - compare_max a b
  let word comp l =
    let b = Buffer.create 16 in
    for i = 0 to String.length (List.hd l) - 1 do
      Buffer.add_char b (most_frequent comp i l);
    done;
    Buffer.contents b

  let read_input () = Input.list_lines Fun.id

  let solve comp =
    let l = read_input () in
    let w = word comp l in
    Solution.printf "%s" w

  let solve_part1 () = solve compare_max
  let solve_part2 () = solve compare_min
end

let () = Solution.register_mod (module S)