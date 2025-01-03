open Utils
open Syntax
module S =
struct
  let name = Name.mk "s01"

  let read_input () =
    Input.fold_scan "%d" (fun acc d -> d :: acc) []
    |> List.rev

  let sum = Iter.sum (module Int) List.to_seq
  let solve_part1 () =
    let l = read_input () in
    let n = sum l in
    Solution.printf "%d" n

    let repeat numbers =
    let cache = ~%[] in
    let rec loop l acc =
      match l with
        [] -> loop numbers acc
      | n :: ll ->
        let nacc = n + acc in
        if cache %? nacc then nacc else begin
          cache.%{nacc} <- ();
          loop ll nacc
        end
    in
    loop numbers 0


  let solve_part2 () =
    let l = read_input () in
    let n = repeat l in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)
