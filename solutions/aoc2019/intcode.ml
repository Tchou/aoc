open Utils
open Syntax

let read_from_string s =
  s
  |> String.split_on_char ','
  |> List.map int_of_string
  |> Array.of_list

let read () = read_line () |> read_from_string

type state = { code : int array;
               mutable pc : int;
               stdin : int Queue.t;
               stdout : int Queue.t;
               mem : (int, int) Hashtbl.t;
               mutable rbase : int;
             }

let make_state code =
  {code = Array.copy code; pc = 0;
   stdin = Queue.create ();
   stdout = Queue.create ();
   mem = ~%[];
   rbase = 0;
  }

let copy_state s =
  let stdin = Queue.copy s.stdin in
  let stdout = Queue.copy s.stdout in
  { s with code = Array.copy s.code; stdin; stdout;
           mem = Hashtbl.copy s.mem
  }

let debug = ref false

let (.@()) state n =
  if !debug then Printf.printf "READING address: %d\n" n;
  if n < Array.length state.code then state.code.(n)
  else state.mem.%?{n} or 0
let (.@()<-) state n v =
  if !debug then Printf.printf "WRITING %d to address: %d\n" v n;
  if n < Array.length state.code then state.code.(n) <- v
  else state.mem.%{n} <- v

let decode state offset =
  let opcode = state.code.(state.pc) in
  let pmode = (opcode / (10*Math.pow 10 offset)) mod 10 in
  let r =
    match pmode with
      0 -> state.@(state.@(state.pc + offset))
    | 1 -> state.@(state.pc + offset)
    | 2 -> state.@(state.rbase + state.@(state.pc + offset))
    | n -> failwith (Printf.sprintf "invalid parameter mode: %d" n)
  in
  if !debug then Printf.printf "DECODING OPCODE %d PARAM %d, mode:%d, value: %d\n"
      (opcode mod 100) (offset) pmode r;
  r

let decode_addr state offset =
  let opcode = state.code.(state.pc) in
  let pmode = (opcode / (10*Math.pow 10 offset)) mod 10 in
  let r =
    match pmode with
      0 -> state.@(state.pc + offset)
    | 2 -> state.rbase + state.@(state.pc + offset)
    | n -> failwith (Printf.sprintf "invalid address parameter mode: %d" n)
  in
  if !debug then Printf.printf "DECODING DEST %d PARAM %d, mode:%d, value: %d\n"
      (opcode mod 100) (offset) pmode r;
  r

let rec eval ?(out_count=max_int) state =
  let { code; pc; mem; stdin; stdout; rbase } = state in
  let opcode = code.(pc) in
  if !debug then Printf.printf "CURRENT PC: %d, FULL OPCODE: %d\n%!" pc opcode;
  match opcode mod 100 with
    1|2 as op ->
    let v1 = decode state 1 in
    let v2 = decode state 2 in
    let r = if op = 1 then v1 + v2 else v1 * v2 in
    let a3 = decode_addr state 3 in
    state.@(a3) <- r;
    state.pc <- pc + 4;
    eval ~out_count state;
  | 3 ->
    if Queue.is_empty stdin then `need_input
    else begin
      let a1 = decode_addr state 1 in
      state.@(a1) <- Queue.pop stdin;
      state.pc <- pc + 2;
      eval ~out_count state
    end
  | 4 ->
    let v1 = decode state 1 in
    Queue.push v1 stdout;
    state.pc <- pc + 2;
    if Queue.length stdout = out_count then `full_output
    else eval ~out_count state
  | 5|6 as op ->
    let v1 = decode state 1 in
    let v2 = decode state 2 in
    let npc =
      if (op = 5 && v1 <> 0) || (op = 6 && v1 = 0) then v2 else pc + 3
    in
    state.pc <- npc;
    eval ~out_count state
  | 7|8 as op ->
    let v1 = decode state 1 in
    let v2 = decode state 2 in
    let v = int_of_bool ((op = 7 && v1 < v2)|| (op = 8 && v1 = v2)) in
    let a3 = decode_addr state 3 in
    state.@(a3) <- v;
    state.pc <- pc+4;
    eval ~out_count state
  | 9 -> let v1 = decode state 1 in
    state.rbase <- state.rbase + v1;
    state.pc <- pc + 2;
    eval ~out_count state
  | 99 -> `halt
  | n -> failwith (Printf.sprintf "failure: %d" n)

