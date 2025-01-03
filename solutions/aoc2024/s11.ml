open Utils
open Syntax
module S =
struct

  let name = Name.mk "s11"
  let read_input () =
    Input.read_line () |> String.split_on_char ' ' |> List.map int_of_string

  let rec num_digits n =
    if n < 10 then 1
    else 1+num_digits (n/10)

  let split_number n k =
    let p = Math.pow 10 (k/2) in
    n / p, n mod p

  let blink cache r n =
    let rec split i n =
      if i >= r then 1 else
        match cache.%?{i, n} with
          Some len -> len
        | None ->
          let res =
            if n = 0 then split (i+1) 1
            else let k = num_digits n in
              if k land 1 = 0 then
                let n1, n2 = split_number n k in
                split (i+1) n1 + split (i+1) n2
              else
                split (i+1) (n*2024)
          in
          cache.%{i, n} <- res;
          res
    in
    split 0 n

  let run r l =
    let cache = ~%[] in
    l
    |> List.map (blink cache r)
    |> Iter.sum (module Int) List.to_seq

  let solve n =
    let l = read_input () in
    let n = run n l in
    Solution.printf "%d" n
  let solve_part1 () = solve 25
  let solve_part2 () = solve 75
end

let () = Solution.register_mod (module S)