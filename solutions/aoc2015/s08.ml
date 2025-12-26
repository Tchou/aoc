open Utils
module S =
struct
  let name = Name.mk "s08"


  let is_hex c = 
    match c with
    '0'..'9' | 'a'..'f' -> true
             | _ -> false

  let count_esc_chars s =
    let rec loop i len acc =
      if i >= len then acc
      else
        match s.[i] with
          '\\' -> begin match s.[i+1] with 
              '\\'| '"' -> loop (i+2) len (acc+1)
            | 'x' when is_hex s.[i+2] &&  is_hex s.[i+3] ->
              loop (i+4) len (acc+1)
            | _ -> assert false
          end
        | c -> loop (i+1) len (acc+1)
    in
    loop 1 (String.length s - 1) 0

  let count_unesc_chars s =
    let rec loop i len acc =
      if i >= len then acc
      else
        match s.[i] with
          '\\' | '"' -> loop (i+1) len (acc+2)
        | _ -> loop (i+1) len (acc+1)
    in
    loop 0 (String.length s) 2


  let read_input () =
    Input.list_lines String.trim

  let sum_repr f l =
    Iter2.(
      list l
      |> fold (fun acc s ->
          let n = String.length s in
          let m = f s in
          acc + (max m n) - (min m n)) 
        0)
  let solve f () =
    let l = read_input () in
    let n = sum_repr f l in
    Solution.printf "%d" n

  let solve_part1 = solve count_esc_chars
  let solve_part2 = solve count_unesc_chars
end

let () = Solution.register_mod (module S)