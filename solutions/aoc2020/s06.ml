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
    List.iter (fun s ->
        String.iter (fun c ->
            tab.$[Char.code c - Char.code 'a']<- '\x01') s) l;
    Bytes.to_seq tab
    |> Seq.map Char.code
    |> Iter.sum (module Int) Fun.id

  let solve_part1 () =
    let l = read_input () in
    l
    |> List.map count_answers
    |> Iter.sum (module Int) List.to_seq
    |> Solution.printf "%d"

  let count_distinct_answers l =
    Bytes.fill tab 0 (Bytes.length tab) '\x00';
    let n = List.length l in
    List.iter (fun s ->
        String.iter (fun c ->
            let idx = Char.code c - Char.code 'a' in
            tab.$[idx] <- Char.unsafe_chr ((Char.code tab.$[idx]) + 1)) s) l;
    Bytes.to_seq tab
    |> Seq.map Char.code
    |> Iter.count_if ((=) n) Fun.id

  let solve count =
    let l = read_input () in
    l
    |> List.map count
    |> Iter.sum (module Int) List.to_seq
    |> Solution.printf "%d"

  let solve_part1 () = solve count_answers
  let solve_part2 () = solve count_distinct_answers

end

let () = Solution.register_mod (module S)