open Utils
open Syntax

module S =
struct
  let name = Name.mk "s12"

  let numbers s =
    List.map int_of_string (String.split_on_char ',' s)

  let count_arrangements puzzle dys_list =
    let cache = ~%[] in
    let rec eat_nsharps n puzzle dys_list ip id =
      match cache.%?{ip,id,n} with
        Some n -> n
      | None ->
        let count =
          match puzzle with
          | ('#'|'?') :: lp ->
            if n = 1 then
              eat_one_dot lp dys_list (ip+1) id
            else
              eat_nsharps (n-1) lp dys_list (ip+1) id
          | [] -> if n = 0 then 1 else 0
          | _ -> 0
        in cache.%{ip,id,n} <- count; count
    and eat_one_dot puzzle dys_list ip id =
      match puzzle, dys_list with
        ('.'|'?')::lp, ld -> eat_dots lp ld (ip+1) id
      | [], [] -> 1
      | _ -> 0
    and eat_dots puzzle dys_list ip id =
      match puzzle, dys_list with
      | '.'::lp, ld -> eat_dots lp ld (ip+1) id
      | '#'::_, n::ld -> eat_nsharps n puzzle ld ip (id+1)
      | '?'::_, n::ld ->
        let c = eat_nsharps n puzzle ld ip (id+1) in
        c + eat_one_dot puzzle dys_list ip id
      | '?'::lp, [] -> eat_dots lp [] (ip+1) id
      | [], [] -> 1
      | _ -> 0
    in
    eat_dots puzzle dys_list 0 0

  let load_input fp fd =
    Input.fold_scan "%[.#?] %[0-9,]"
      (fun acc s1 s2 ->
         let puzzle = fp (String.explode s1) in
         let dys_list = fd (numbers s2) in
         acc + count_arrangements puzzle dys_list
      ) 0
    |> Solution.printf "%d"

  let five s l =
    List.concat [l;s;l;s;l;s;l;s;l]

  let solve_part1 () =
    load_input Fun.id Fun.id
  let solve_part2 () =
    load_input (five ['?']) (five [])

end

let () = Solution.register_mod (module S)