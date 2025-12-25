open Utils
module S =
struct
  let name = Name.mk "s23"

  type register = int
  type instr = 
      Hlf of register
    | Tpl of register
    | Inc of register
    | Jmp of int
    | Jie of register * int
    | Jio of register * int
  let read_register s =
    match s.[0] (* avoids ',' *) with
    'a' -> 0
    |'b' -> 1
    | _ -> assert false
  let read_input () =
    Input.list_fields ' ' (
      fun l ->
        match List.filter ((<>) "") l with
          ["hlf"; r] -> Hlf (read_register r)
        | ["tpl"; r] -> Tpl (read_register r)
        | ["inc"; r] -> Inc (read_register r)
        | ["jmp"; i] -> Jmp (int_of_string i)
        | ["jie"; r; i] -> Jie (read_register r, int_of_string i)
        | ["jio"; r; i] -> Jio (read_register r, int_of_string i)
        | _ -> assert false
    )
    |> Array.of_list

  let eval regs prog =
    let len = Array.length prog in
    let rec loop pc =
      if pc >= 0 && pc < len then
        match prog.(pc) with
        | Hlf r -> regs.(r) <- regs.(r) lsr 1; loop (pc+1)
        | Tpl r ->
          let n = regs.(r) in 
          regs.(r) <- (n lsl 1) + n;
          loop (pc+1)
        | Inc r -> regs.(r) <- regs.(r) + 1; loop (pc+1)
        | Jmp i -> loop (pc+i)
        | Jie (r, i) -> 
          let i = 
            if regs.(r) land 1 = 0 then i else 1
          in 
          loop (pc+i)
        | Jio (r, i) -> 
          let i = 
            if regs.(r) = 1 then i else 1
          in 
          loop (pc+i)
    in
    loop 0
  let solve regs () =
    let prog = read_input () in
    eval regs prog;
    Solution.printf "%d" regs.(1)

  let solve_part1 = solve [|0;0|]
  let solve_part2 = solve [|1;0|]

end

let () = Solution.register_mod (module S)