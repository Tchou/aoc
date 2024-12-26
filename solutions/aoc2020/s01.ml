open Utils
open Syntax

module S =
struct
  let name = Name.mk "s01"

  let load_input () =
    Input.fold_lines (fun acc s ->
        let i = int_of_string s in
        i ::acc
      ) []

  let find2020 l =
    let cache = ~%[] in
    let rec loop l =
      match l with
        [] -> assert false
      | i :: ll ->
        let j = 2020 - i in
        if cache %? i then i * j
        else if cache %? i then i * j
        else (cache.%{i}<- (); cache.%{j}<-(); loop ll)
    in
    loop l

  let solve_part1 () =
    load_input ()
    |> find2020
    |> Solution.printf "%d"
  let solve_part2 () =
    let numbers = load_input () in
    let cache = ~%[] in
    let () =
      List.iteri (fun i n ->
          List.iteri (fun j m ->
              if j > i then cache.%{(n+m)} <- (n,m);
            ) numbers) numbers
    in
    List.find (fun k ->
        let c = 2020 - k in
        match cache.%?{c} with
          Some (n,m) -> Solution.printf "%d" (k*n*m); true
        | None -> false
      ) numbers
    |> ignore
end

let () = Solution.register_mod (module S)