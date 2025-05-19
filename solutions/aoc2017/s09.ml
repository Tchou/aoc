open Utils
module S =
struct
  let name = Name.mk "s09"
  let read_input () =
    Input.read_line ()


  let parse s =
    let open Format in
    let group_count = ref 0 in
    let garbage_count = ref 0 in
    let push = function n::_ as l -> (n+1)::l  | [] -> assert false in
    let pop = function n::l -> group_count := !group_count + n; l | _ -> assert false in
    let len = String.length s in
    let rec group i stack =
      if i >= len then stack else
        match s.[i] with
          '{' -> group (i+1) (push stack)
        | '}' -> group (i+1) (pop stack)
        | ',' -> group (i+1) stack
        | '<' -> garbage (i+1) stack
        | c -> failwith (sprintf "Malformed sequence, position %d, '%c'" i c)
    and garbage i stack =
      if i >= len then failwith "Unterminated garbage" else
        match s.[i] with
          '>' -> group (i+1) stack
        | '!' -> if i+2 >= len then failwith "Unterminated garbage" else garbage (i+2) stack
        | _ -> incr garbage_count; garbage (i+1) stack
    in
    match group 0 [0] with
      [0] -> !group_count, !garbage_count
    | _ -> failwith "Invalid input"


  let solve f =
    let s = read_input () in
    let n = f (parse s) in
    Solution.printf "%d" n

  let solve_part1 () = solve fst
  let solve_part2 () = solve snd
end

let () = Solution.register_mod (module S)