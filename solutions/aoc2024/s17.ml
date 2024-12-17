open Utils
module S =
struct
  let name = Name.mk "s17"

  type state = { stdout : int Queue.t;
                 mutable pc : int;
                 mutable a : int;
                 mutable b : int;
                 mutable c : int; }
  let mk_state () = { stdout = Queue.create ();
                      pc = 0;
                      a = 0;
                      b = 0;
                      c = 0
                    }
  let next state = state.pc <- state.pc + 2

  let combo state n =
    match n with
      4 -> state.a
    | 5 -> state.b
    | 6 -> state.c
    | 7 -> assert false
    | n -> n

  let display stdout =
    stdout
    |> Queue.to_seq
    |> Seq.map string_of_int
    |> List.of_seq
    |> String.concat ","

  let eval code state =
    let last_op = String.length code - 2 in
    let rec loop () =
      let {pc;a;b;c;stdout} = state in
      if pc <= last_op then
        let arg = Char.code code.[pc+1] - Char.code '0' in
        let d =
          match Char.code code.[pc] - Char.code '0' with
            0 -> state.a <- a / (1 lsl combo state arg); 2
          | 1 -> state.b <- (b lxor arg); 2 (* land 0b111 ? *)
          | 2 -> state.b <- (combo state arg) land 0b111; 2
          | 3 -> if a = 0 then 2 else arg - pc
          | 4 -> state.b <- b lxor c; 2
          | 5 -> Queue.push ((combo state arg) land 0b111) stdout; 2
          | 6 -> state.b <- a / (1 lsl combo state arg); 2
          | 7 -> state.c <- a / (1 lsl combo state arg); 2
          | n -> failwith (Format.sprintf "Invalid opcode %d" n)
        in
        state.pc <- pc + d;
        loop ()
    in
    loop ()

  let read_input () =
    let state = mk_state () in
    let open Scanf in
    sscanf (read_line ()) "Register A: %d" (fun n -> state.a <- n);
    sscanf (read_line ()) "Register B: %d" (fun n -> state.b <- n);
    sscanf (read_line ()) "Register C: %d" (fun n -> state.c <- n);
    read_line () |> ignore;
    sscanf (read_line ()) "Program: %s" (fun s ->
        s
        |> String.split_on_char ','
        |> String.concat "", state)
  let solve_part1 () =
    let code, state = read_input () in
    let () = eval code state in
    let s = display state.stdout in
    Ansi.(printf "%a%s%a\n%!" fg green s clear color)


  (* if we decode the program from the input:
     Program: 2,4,1,1,7,5,4,0,0,3,1,6,5,5,3,0
     b <- a land 0b111       //2,4 (3 lsb from a)
     b <- b lxor 1           //1,1
     c <- a / 2^b            //7,5 (shift a from at most 7 bits)
     b <- b lxor c           //4,0 mixb and c
     a <- a / 8              //0,3 (shift a from 3 bits)
     b <- b lxor 6           //1,6 mix b
     out b                   //output b
     if a <> 0 goto 0
     The program decodes the number in its 'a' register 3 bits at a time, and then
     mixes these with upper bits. We can build the quine number starting
     from the last character of the string, which is the most significant bits
     of the quine number, and therefore has the leading "mixed" bits to 0.
     Then we proceed downward accumulating the bits.
  *)
  let find_quine code state =
    let len = String.length code in
    let res = ref [] in
    let exception Found of int in
    let rec loop i acc =
      if i < 0 then raise (Found acc) else
        for n = 0 to 7 do
          let p = (acc lsl 3) lor n in
          state.a <- p;
          state.b <- 0;
          state.c <- 0;
          state.pc <- 0;
          Queue.clear state.stdout;
          eval code state;
          let m = Queue.pop state.stdout in
          let cr = Char.code code.[i] - Char.code '0' in
          if m = cr then
            loop (i-1) ((acc lsl 3) lor n)
        done
    in
    try
      loop (String.length code - 1) 0 ;
      raise Not_found
    with Found res -> res

  let solve_part2 () =
    let code, state = read_input () in
    let state_copy = { state with stdout = Queue.create () } in
    let a = find_quine code state in
    let () = eval code { state_copy with a } in
    assert (code = (display state_copy.stdout |> String.split_on_char ',' |> String.concat ""));
    Ansi.(printf "%a%d%a\n%!" fg green a clear color)

end

let () = Solution.register_mod (module S)