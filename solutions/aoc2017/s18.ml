open Utils
module S =
struct
  let name = Name.mk "s18"

  type arg = Reg of char | Int of int
  type instr = ..
  type instr +=
      Snd of arg
    | Set of arg * arg
    | Add of arg * arg
    | Mul of arg * arg
    | Mod of arg * arg
    | Rcv of arg
    | Jgz of arg * arg

  let get reg = function
    | Int i -> i
    | Reg c -> Array.unsafe_get reg (Char.code c)

  let set reg arg v = match arg with
      Reg c -> Array.unsafe_set reg (Char.code c) v
    | _ -> assert false

  type status = Output | Need_input | Halt
  type state = { mutable pc : int;
                 reg : int array;
                 stdin: int Queue.t;
                 stdout : int Queue.t;
                 mutable count : int }
  exception Pause of status

  let eval part1 state code =
    let reg = state.reg in
    let len = Array.length code in
    let rec loop () =
      let i = state.pc in
      if i >= 0 && i < len then
        let i' =
          match Array.unsafe_get code i with
            Snd a -> 
            state.count <- state.count + 1;
            state.pc <- i + 1;
            Queue.push (get reg a) state.stdout;
            raise (Pause Output)
          | Set (a1, a2) -> set reg a1 (get reg a2); i + 1
          | Add (a1, a2) -> set reg a1 ((get reg a1) + (get reg a2)); i + 1
          | Mul (a1, a2) -> set reg a1 ((get reg a1) * (get reg a2)); i + 1
          | Mod (a1, a2) -> set reg a1 ((get reg a1) mod (get reg a2)); i + 1
          | Rcv a when part1 -> if (get reg a) = 0 then i+1 else begin
              raise (Pause Need_input)
            end
          | Rcv a (* part2 *) -> if Queue.is_empty state.stdin then begin
              raise (Pause Need_input)
            end else (set reg a (Queue.pop state.stdin); i + 1)
          | Jgz (a1, a2) -> if (get reg a1) > 0 then i + (get reg a2) else i + 1
          | _ -> failwith "Unimplemented operation"
        in
        state.pc <- i';
        loop ()
      else Halt
    in
    (try loop () with Pause s -> s)

  let arg s =
    match int_of_string_opt s with
      Some i -> Int i
    | None -> Reg s.[0]

  let parse_others l : instr =
    failwith (Format.sprintf "Syntax error: %s" (String.concat " " l))

  let read_input ?(others=parse_others) () =
    Input.list_fields ' ' (function 
        | [ "snd"; a] -> Snd (arg a)
        | [ "set"; a1; a2 ] -> Set (arg a1, arg a2)
        | [ "add"; a1; a2 ] -> Add (arg a1, arg a2)
        | [ "mul"; a1; a2 ] -> Mul (arg a1, arg a2)
        | [ "mod"; a1; a2 ] -> Mod (arg a1, arg a2)
        | [ "rcv"; a ] -> Rcv (arg a)
        | [ "jgz"; a1; a2 ] -> Jgz (arg a1, arg a2)
        | l -> others l
      )
    |> Array.of_list

  let eval_single code = 
    let state = { pc = 0; reg = Array.make 256 0; stdin = Queue.create ();
                  stdout = Queue.create ();
                  count = 0 } in
    let rec loop () =
      match eval true state code with
        Halt -> assert false
      | Output -> loop ()
      | Need_input -> 
        (Queue.to_seq state.stdout) |> List.of_seq |> List.rev |> List.hd
    in
    loop ()
  let solve_part1 () =
    let instrs = read_input () in
    let n = eval_single instrs in
    Solution.printf "%d" n

  let eval_both instrs =
    let reg = Array.make 256 0 in
    let stdin0 = Queue.create () in
    let stdout0 = Queue.create () in
    let state0 = { pc = 0; reg; stdin = stdin0; stdout = stdout0; count = 0 } in
    let reg = Array.make 256 0 in
    let () = set reg (Reg 'p') 1 in
    let state1 = { pc = 0; reg; stdout = stdin0; stdin = stdout0; count = 0 } in
    let rec loop () =
      match eval false state0 instrs with
        Halt -> consume state1; state1.count
      | Output ->
        begin match eval false state1 instrs with
            Halt -> state1.count
          | _ -> loop ()
        end
      | Need_input ->
        begin match eval false state1 instrs with
            Halt -> state1.count
          | Output -> loop ()
          | Need_input -> if Queue.is_empty state0.stdin then (* deadlock *) state1.count
            else loop ()
        end
    and consume state =
      match eval false state instrs with
        Halt | Need_input -> ()
      | Output -> consume state
    in
    loop ()


  let solve_part2 () =
    let instrs = read_input () in
    let n = eval_both instrs in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)