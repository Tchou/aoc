open Utils
module S =
struct
  let name = Name.mk "s25"

  let dir = function "left" -> true | _ -> false

  let istate c = Char.code c - Char.code 'A'
  let read_state () =
    let open Input in
    let open Scanf in
    let state = sscanf (read_line ()) "In state %c:" istate in
    let _ = read_line () in
    let v0 = sscanf (read_line()) " - Write the value %d." Fun.id in
    let d0 = sscanf (read_line()) " - Move one slot to the %[^.]." dir in
    let s0 = sscanf (read_line()) " - Continue with state %c." istate in
    let _ = read_line () in
    let v1 = sscanf (read_line()) " - Write the value %d." Fun.id in
    let d1 = sscanf (read_line()) " - Move one slot to the %[^.]." dir in
    let s1 = sscanf (read_line()) " - Continue with state %c." istate in
    (state,[|v0,d0,s0;v1,d1,s1|])

  let read_input () =
    let open Input in
    let open Scanf in
    let init = sscanf (read_line ()) "Begin in state %c." istate in
    let steps = sscanf (read_line ()) "Perform a diagnostic checksum after %d steps." Fun.id in
    let rec loop acc =
      match read_line () with
        _ -> loop (read_state()::acc)
      | exception End_of_file -> List.rev acc
    in
    let l = loop [] in
    let a = Array.make (List.length l) [||] in
    List.iter (fun (s, t) -> a.(s) <- t) l;
    init, steps, a

  module Tape =
  struct

    type t = { mutable left : t; mutable right : t; mutable value : int}

    let rec dummy = { value = -1; left = dummy; right = dummy }
    let left c = 
      if c.left != dummy then c.left 
      else
        let n = { left = dummy; right = c; value = 0} in
        let () = c.left <- n in
        n
    let right c =
      if c.right != dummy then c.right 
      else
        let n = { left = c; right = dummy; value = 0} in
        let () = c.right <- n in
        n

    let rec iter_left f c =
      if c != dummy then begin
        f c.value;
        iter_left f c.left;
      end

    let rec iter_right f c =
      if c != dummy then begin
        f c.value;
        iter_right f c.right;
      end

    let iter f c =
      if c != dummy then begin
        f c.value;
        iter_left f c.left;
        iter_right f c.right;
      end

    let count c =
      let total = ref 0 in
      iter (fun v -> total:= v + !total) c;
      !total
    let create () = { left = dummy; value = 0; right = dummy }

  end

  let eval init steps a =
    let rec loop cell q n =
      if n = 0 then Tape.count cell
      else
        let v = cell.Tape.value in
        let v', d, q' = a.(q).(v) in
        let () = cell.Tape.value <- v' in
        loop Tape.(if d then left cell else right cell) q' (n-1)
    in
    loop (Tape.create ()) init steps

  let solve_part1 () =
    let init, steps, a = read_input () in
    let n = eval init steps a in
    Solution.printf "%d" n
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)