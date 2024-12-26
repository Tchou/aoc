open Utils
open Syntax
module S =
struct

  module Bits :
  sig
    type t = private bytes
    type index = int
    val make : string -> t

    val length : t -> int
    (** in bits *)

    val get : t -> index -> int -> index * int
  end = struct
    type t = bytes
    type index = int

    let digit = function
      | '0'..'9' as c -> Char.code c - Char.code '0'
      | 'A'..'F' as c -> 10 + Char.code c - Char.code 'A'
      | 'a'..'f' as c -> 10 + Char.code c - Char.code 'a'
      | _ -> assert false

    let make s =
      let l = String.length s / 2 in
      let b = Bytes.create l in
      for i = 0 to l - 1 do
        b.$[i] <- Char.chr (digit (s.[2*i]) * 16 + digit(s.[2*i+1]));
      done;
      b

    let length s = Bytes.length s lsl 3

    (*
      ...|XXXXXXXX|XXXXXXXX|...|XXXXXXXX|
             ^--------- len ----------^
             | idx
        24  27
    *)
    let bit s idx =
      let i = idx asr 3 in
      let r = idx land 7 in
      let b = Char.code s.$[i] in
      (b lsr (7-r)) land 1

    let get s idx len =
      assert (len <= 62);
      let res = ref 0 in
      for i = 0 to len - 1 do
        let k = bit s (idx + i) in
        res := (!res lsl 1) lor k
      done;
      idx + len, !res


  end

  type code = Sum | Prod | Min | Max | Gt | Lt | Eq

  type op_ = Literal of int
           | Op of { code : code; args : op array }
  and op = { version : int; op : op_ }
  let op_code = function
    | 0 -> Sum
    | 1 -> Prod
    | 2 -> Min
    | 3 -> Max
    | 5 -> Gt
    | 6 -> Lt
    | 7 -> Eq
    | _ -> assert false

  let header b i =
    let open Bits in
    let i, vers = get b i 3 in
    let i, id = get b i 3 in
    i, (vers, id)

  let parse_literal b i =
    let rec loop i acc =
      let i, n = Bits.get b i 5 in
      let acc = ((acc lsl 4) lor (n land 0xf)) in
      if n land 0b10000 = 0 then i, Literal acc
      else
        loop i acc
    in
    loop i 0

  let rec parse_packet b i =
    let i, (version, id) = header b i in
    let i, op =
      match id with
        4 -> parse_literal b i
      | code -> let i, len_type = Bits.get b i 1 in
        let i, r_args =
          if len_type = 0 then
            let i, len = Bits.get b i 15 in
            parse_packets_0 b i (i+len) []
          else
            let i, num = Bits.get b i 11 in
            parse_packets_1 b i num []
        in
        let args = Array.of_list (List.rev r_args) in
        let code = op_code code in
        i, Op {code; args}
    in
    i, {version; op}
  and parse_packets_0 b i stop acc =
    if i = stop then i, acc
    else
      let i, p = parse_packet b i in
      parse_packets_0 b i stop (p :: acc)
  and parse_packets_1 b i num acc =
    if num = 0 then i, acc
    else
      let i, p = parse_packet b i in
      parse_packets_1 b i (num-1) (p::acc)

  let rec score1 op =
    match op.op with
      Literal _ -> op.version
    | Op {args; _} ->
      Array.fold_left (fun acc op -> acc + score1 op) op.version args

  let ibool = function false -> 0 | true -> 1
  let rec big_op op args =
    let f,init = match op with
        Sum -> (+), 0
      | Prod -> ( * ), 1
      | Max -> max, min_int
      | _ (* Min*) -> min, max_int
    in
    Array.fold_left (fun acc o -> f acc (score2 o)) init args
  and score2 op =
    match op.op with
      Literal v -> v
    | Op {code; args } ->
      match code with
        Sum | Prod | Max | Min -> big_op code args
      | Lt -> ibool (score2 args.(0) < score2 args.(1))
      | Gt -> ibool (score2 args.(0) > score2 args.(1))
      | Eq -> ibool (score2 args.(0) = score2 args.(1))



  let name = Name.mk "s16"
  let solve score =
    let s = Input.read_line () in
    let b = Bits.make s in
    let _, op = parse_packet b 0 in
    let n = score op in
    Solution.printf "%d" n
  let solve_part1 () = solve score1

  let solve_part2 () = solve score2
end

let () = Solution.register_mod (module S)