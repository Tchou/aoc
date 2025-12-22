open Utils
module S =
struct
  let name = Name.mk "s12"

  type op = Int of int | Reg of int
  type op_code = ..
  type op_code +=
      Cpy of op * int
    | Inc of int
    | Dec of int
    | Jnz of op * op


  let eval_op regs = function Int i -> i | Reg i -> regs.(i)
  let eval reg_c prog =
    let regs = Array.make 4 0 in
    let () = regs.(2) <- reg_c in
    let len = Array.length prog in
    let rec loop pc =
      if pc < len then
        match prog.(pc) with
          Cpy (op, n) -> regs.(n) <- eval_op regs op; loop (pc+1)
        | Inc n -> regs.(n) <- regs.(n) + 1; loop (pc+1)
        | Dec n -> regs.(n) <- regs.(n) - 1; loop (pc+1)
        | Jnz (op1, op2) -> 
          if eval_op regs op1 <> 0 then loop (pc + eval_op regs op2) else loop (pc + 1)
        | _ -> fail "Unsupported operand in %s.%s" (fst name) (snd name)
    in
    loop 0;
    regs.(0)

  let read_op s =
    match s with
      "a" -> Reg 0
    | "b" -> Reg 1
    | "c" -> Reg 2
    | "d" -> Reg 3
    | _ -> Int (int_of_string s)

  let read_reg s = 
    match read_op s with
      Reg n -> n
    | _ -> assert false

  let read_instr = function
      [ "cpy"; o; n] -> Cpy (read_op o, read_reg n)
    | [ "inc"; n] -> Inc (read_reg n)
    | [ "dec"; n] -> Dec (read_reg n)
    | [ "jnz"; o1; o2] -> Jnz(read_op o1, read_op o2)
    | l -> fail "Parse_error: %s" (String.concat " " l)
  
  let read_input () =
    Input.list_fields ' ' read_instr
    |> Array.of_list

  let solve reg_c =
    let prog = read_input () in
    let n = eval reg_c prog in
    Solution.printf "%d" n

  let solve_part1 () = solve 0
  let solve_part2 () = solve 1
end

let () = Solution.register_mod (module S)