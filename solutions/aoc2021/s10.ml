open Utils

module S =
struct
  let name = Name.mk "s10"

  let validate s =
    let len = String.length s in
    let rec loop i stack =
      if i = len then None, stack
      else
        match s.[i], stack with
          '<' | '{' | '(' | '[' as c, _ -> loop (i+1) (c::stack)
        | '>', '<' :: sstack
        |'}', '{' :: sstack
        |')', '(' :: sstack
        |']', '[' :: sstack -> loop (i+1) sstack
        | c, _ -> Some c, stack
    in
    loop 0 []

  let solve_part1 () =
    Input.fold_lines (fun acc s ->
        match fst (validate s) with
          None -> acc
        | Some ')' -> 3 + acc
        | Some ']' -> 57 + acc
        | Some '}' -> 1197 + acc
        | Some '>' -> 25137 + acc
        | _ -> acc) 0
    |>Printf.printf "%d\n"
  let solve_part2 () =
    let scores =
      Input.fold_lines (fun acc s ->
          match validate s with
          | Some _, _ -> acc
          | None, l ->
            let score =
              List.fold_left (fun acc ->
                  function '(' -> 5*acc + 1
                         | '[' -> 5*acc+2
                         | '{' -> 5*acc+3
                         | _ -> 5*acc+4) 0 l
            in (score::acc)
        ) []
    in
    let scores = Array.of_list scores in
    let () = Array.sort compare scores in
    let len = Array.length scores / 2 in
    Printf.printf "%d\n" scores.(len)
end

let () = Solution.register_mod (module S)