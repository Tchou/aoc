open Utils
module S =
struct
  let name = Name.mk "s23"

  type S12.S.op_code += Tgl of int | InvCpy of S12.S.op * S12.S.op


  let read_instr =
    function
    | [ "tgl"; n] -> S12.S.(Tgl (read_reg n))
    | l -> S12.S.read_instr l

  let read_input () =
    Input.list_fields ' ' read_instr
    |> Array.of_list

  let tgl_instr = 
    let open S12.S in
    function
      Inc n -> Dec n
    | Tgl n | Dec n -> Inc n
    | Jnz (op1, op2) -> begin match op2 with 
          Reg n -> Cpy (op1, n)
        | Int n -> InvCpy (op1, op2) end
    | Cpy(op, n) -> Jnz(op, Reg n)
    | InvCpy (op1, op2) -> Jnz(op1, op2)
    | _ -> fail "Invalid opcode for %s.%s" (fst name) (snd name)

  let reg_name = function 
      0 -> "a"
    | 1 -> "b"
    | 2 -> "c"
    | 3 -> "d"
    | n -> fail "Invalid register number %d" n

  let pp_op fmt o =
    let open Format in
    match o with 
      S12.S.Int n -> fprintf fmt "%d" n
    | Reg n -> fprintf fmt "%s" (reg_name n)

  let pp_instr fmt i =
    let open Format in
    let open S12.S in 
    match i with 
    | Inc n -> fprintf fmt "inc %s" (reg_name n)
    | Dec n -> fprintf fmt "dec %s" (reg_name n)
    | Cpy (op, n) -> fprintf fmt "cpy %a %s" pp_op op (reg_name n)
    | Jnz (op1, op2) -> fprintf fmt "jnz %a %a" pp_op op1 pp_op op2
    | Tgl n -> fprintf fmt "tgl %s" (reg_name n)
    | InvCpy (op1, op2) -> fprintf fmt "(invalid)cpy %a %a" pp_op op1 pp_op op2
    | _ -> fail "Invalid instruction"

  let pp_prog pc regs fmt prog =
    let open Format in 
    Array.iteri (fun i ins -> 
        fprintf fmt  "%s %3d: %a\n" (if i = pc then ">" else " ") i pp_instr ins)
      prog;
    fprintf fmt "Registers: ";
    Array.iteri (fun i n -> fprintf fmt "%s: %d  " (reg_name i) n) regs;
    fprintf fmt "\n%!"

  let eval reg_a prog =
    let open S12.S in
    let regs = Array.make 4 0 in
    let () = regs.(0) <- reg_a in
    let len = Array.length prog in
    let rec loop pc =
      if false then Format.printf "Program is:\n%a\n" (pp_prog pc regs) prog;
      if pc < len then
        match prog.(pc) with
          Cpy (op, n) -> regs.(n) <- eval_op regs op; loop (pc+1)
        | Inc n -> regs.(n) <- regs.(n) + 1; loop (pc+1)
        | Dec n -> regs.(n) <- regs.(n) - 1; loop (pc+1)
        | Jnz (op1, op2) -> 
          if eval_op regs op1 <> 0 then loop (pc + eval_op regs op2) else loop (pc + 1)
        | Tgl n -> let pos = pc + regs.(n) in 
          if pos < 0 || pos >= len then loop (pc + 1)
          else begin
            prog.(pos) <- tgl_instr prog.(pos);
            loop (pc + 1)
          end
        | InvCpy _ -> loop (pc + 1)
        | _ -> fail "Unsupported operand in %s.%s" (fst name) (snd name)
    in
    loop 0;
    regs.(0)
  let solve n () =
    let code = read_input () in
    let a = eval n (Array.copy code) in 
    Solution.printf "%d" a

  let solve_part1 = solve 7
  let solve_part2 = solve 12
end

let () = Solution.register_mod (module S)