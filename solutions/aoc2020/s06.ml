open Utils
open Syntax

module S =
struct
  let name = Name.mk "s06"

  let read_input () =
    Input.fold_lines (fun acc s ->
        match acc, s with
          _, "" -> ([]::acc)
        | l::acc', _ ->(s::l)::acc'
        | _ -> assert false
      ) [[]]

  let tab = Bytes.make (Char.code 'z' - Char.code 'a' + 1) '\x00'
  let count_answers l =
    Bytes.fill tab 0 (Bytes.length tab) '\x00';
    let open Iter in
    l |> list |> iter (fun s ->
        String.iter (fun c ->
            tab.$[Char.code c - Char.code 'a']<- '\x01') s);
    tab 
    |> bytes
    |> map Char.code
    |> sum int

  let solve_part1 () =
    let l = read_input () in
    let open Iter in
    l
    |> list
    |> map count_answers
    |> sum int
    |> Solution.printf "%d"

  let count_distinct_answers l =
    Bytes.fill tab 0 (Bytes.length tab) '\x00';
    let n = List.length l in
    let open Iter in
    l 
    |> list
    |> iter (fun s ->
        String.iter (fun c ->
            let idx = Char.code c - Char.code 'a' in
            tab.$[idx] <- Char.unsafe_chr ((Char.code tab.$[idx]) + 1)) s);
    tab
    |> bytes
    |> map Char.code
    |> count_if  ((=) n)

  let solve fcount =
    let l = read_input () in
    let open Iter in
    l
    |> list
    |> map fcount
    |> sum int
    |> Solution.printf "%d"

  let solve_part1 () = solve count_answers
  let solve_part2 () = solve count_distinct_answers

end

let () = Solution.register_mod (module S)