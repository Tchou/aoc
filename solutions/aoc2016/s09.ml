open Utils
module S =
struct
  let name = Name.mk "s09"

  let read_input () = 
    let b = Buffer.create 16 in
    Input.fold_lines (fun () s -> Buffer.add_string b s) ();
    Buffer.contents b


  let parse_int_from s i =
    let rec loop i acc =
      match s.[i] with
      '0'..'9' as c -> loop (i+1) (acc * 10 + (Char.code c - Char.code '0'))
      | _ -> i, acc
    in
    loop i 0

  let rec count_size recursive input i ilen =
    let rec loop i size =
      if i >= ilen then size else
        match input.[i] with
        | '(' -> 
          let i, len = parse_int_from input (i+1) in
          let i, rep = parse_int_from input (i+1) in
          let len' = if recursive then 
              count_size true input (i+1) (i+1+len)
            else len
          in  
          loop (i+1+len) (size + len' * rep)
        | c -> loop (i+1) (size + 1)
    in
    loop i 0

  let solve recursive = 
    let b = read_input () in
    let n = count_size recursive b 0 (String.length b) in
    Solution.printf "%d" n

  let solve_part1 () = solve false
  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)