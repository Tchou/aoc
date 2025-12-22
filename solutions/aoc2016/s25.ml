open Utils
open Syntax
module S =
struct
  let name = Name.mk "s25"

  type S12.S.op_code += Out of S12.S.op 


  let read_instr =
    function
    | [ "out"; n] -> S12.S.(Out (read_op n))
    | l -> S23.S.read_instr l

  let read_input () =
    Input.list_fields ' ' read_instr
    |> Array.of_list

  let pp_instr fmt i =
    match i with
      Out op -> Format.fprintf fmt "out %a" S23.S.pp_op op
    | i -> S23.S.pp_instr fmt i

  let eval prog regs pc =
    let open S12.S in
    let len = Array.length prog in
    let rec loop pc =
      if pc < len then
        match prog.(pc) with
          Cpy (op, n) -> regs.(n) <- eval_op regs op; loop (pc+1)
        | Inc n -> regs.(n) <- regs.(n) + 1; loop (pc+1)
        | Dec n -> regs.(n) <- regs.(n) - 1; loop (pc+1)
        | Jnz (op1, op2) -> 
          if eval_op regs op1 <> 0 then loop (pc + eval_op regs op2) else loop (pc + 1)
        | Out op -> `Out (pc, eval_op regs op)
        | _ -> fail "Unsupported operand in %s.%s" (fst name) (snd name)
      else `Exit
    in
    loop pc


  (* By printing things we remark that all programs starting from small integers enter a cycle
      (no program diverges with new values).
     We use a Hashtable to cache previous results and detec cycles. We also no that the ith 
     out instruction must output number (i mod 2) (counting from 0). Whenever this condition does not hold,
     we can abort. Otherwise, cache the result and loop. This is crude but works well.
  *)
  let repeat prog =
    let regs = Array.make 4 0 in
    let rec loop table count pc =
      match eval prog regs pc with
        `Out (npc, nval) ->
        begin match table.%?{npc, nval, regs} with
            Some _ -> true
          | None ->
            if count mod 2 <> nval then false else begin
              table.%{npc, nval, Array.copy regs} <- ();
              loop table (count+1) (npc+1)
            end
        end
      | `Exit -> false
    in
    let table = Hashtbl.create 16 in
    let rec iter_n i =
      Hashtbl.clear table;
      regs.(0) <- i;
      regs.(1) <- 0;
      regs.(2) <- 0;
      regs.(3) <- 0;
      if loop table 0 0 then i
      else iter_n (i+1)
    in
    iter_n 0

  let solve_part1 () =
    let prog = read_input () in 
    let n = repeat prog in 
    Solution.printf "%d" n
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)