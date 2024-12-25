open Utils
open Syntax
module S =
struct
  let name = Name.mk "s24"

  type wire = string

  type gate = OR | AND | XOR
  let parse_gate g =
    match g with
      "OR" -> (OR)
    | "AND" -> (AND)
    | "XOR" -> (XOR)
    | _ -> assert false
  let pp_op fmt op =
    Format.fprintf fmt "%s" (match op with OR -> "OR" | AND -> "AND" | XOR -> "XOR")

  module StrMap = Map.Make(String)

  let read_input () =
    let wires = ref StrMap.empty in
    let gates = ref StrMap.empty in
    InputUntil.fold_lines (fun acc l ->
        if l = "" then false, ()
        else
          Scanf.sscanf l "%[^:]: %d" (fun s i -> true, (wires := StrMap.add s  i !wires))
      ) ();
    Input.fold_scan "%[a-z0-9] %[A-Z] %[a-z0-9] -> %[a-z0-9]"
      (fun acc r1 g r2 r ->
         gates := StrMap.add r (r1, parse_gate g, r2) !gates) ();
    !wires, !gates

  let eval_op o v1 v2 _ =
    match o with
      OR -> (v1 lor v2) land 1
    | AND -> (v1 land v2) land 1
    | XOR -> (v1 lxor v2) land 1

  let eval eval_op wires gates =
    let rec loop todo acc =
      match todo with
        [] -> acc
      | r :: ttodo ->
        let r1, op, r2 = StrMap.find r gates in
        match StrMap.(find_opt r1 acc, find_opt r2 acc) with
          None, None -> loop (r1::r2::todo) acc
        | Some _, None -> loop (r2::todo)  acc
        | None, Some _ -> loop (r1::todo) acc
        | Some v1, Some v2 ->
          let v = eval_op op v1 v2 r in
          loop ttodo StrMap.(add r v acc)
    in
    let todo = StrMap.fold (fun w _ acc -> if w.[0] = 'z' then w::acc else acc) gates [] in
    loop todo wires

  let compute f init todo wires =
    List.sort (fun x y -> compare y x) todo
    |> List.fold_left (f wires) init

  let solve_part1 () =
    let wires, gates = read_input () in
    let wires = eval eval_op wires gates in
    let n =
      StrMap.fold (fun w n acc ->
          if w.[0] <> 'z' then acc else
            let i = String.sub w 1 2 |> int_of_string in
            acc + (n lsl i)) wires 0
    in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  module Expr =
  struct
    type expr =
        Wire of wire
      | Binop of t * gate * t
    and t = { expr : expr; wire : string}
    let cons expr wire = {expr; wire}
    let wire s = cons (Wire s) s
    let rec letter e =
      match e.expr with
        Wire s -> s
      | Binop(e1, _, e2) ->
        let s1 = letter e1 in
        let s2 = letter e2 in
        let s = if s1 >= s2 then s1 else s2 in
        s
    let binop e1 op e2 name =
      let s1 = letter e1 in
      let s2 = letter e2 in
      if s1 >= s2 then cons (Binop(e1, op, e2)) name
      else cons (Binop (e2, op, e1)) name
  end
  let rec pp fmt e =
    let open Format in
    match e.Expr.expr with
      Wire s -> fprintf fmt "%s" s
    | Binop  (e1, op, e2) -> fprintf fmt "(%a %a %a)" pp e1 pp_op op pp e2

  let sym_eval wires gates =
    let sym_eval_op op r1 r2  n =
      Expr.binop r1 op r2 n
    in
    eval sym_eval_op wires gates

  let sym_wires wires =
    let sym_wires = ~%[] in
    StrMap.fold (fun r _ acc -> StrMap.add r Expr.(wire r) acc) wires StrMap.empty

  (* bit zN is well-formed if:
     z05 = ((y05 XOR x05) XOR (((y04 XOR x04) AND (((y03 XOR x03) AND (((y02 XOR x02) AND (((y01 XOR x01) AND (y00 AND x00)) OR (y01 AND x01))) OR (y02 AND x02))) OR (y03 AND x03))) OR (y04 AND x04)))
     z04 = ((y04 XOR x04) XOR (((y03 XOR x03) AND (((y02 XOR x02) AND (((y01 XOR x01) AND (y00 AND x00)) OR (y01 AND x01))) OR (y02 AND x02))) OR (y03 AND x03)))
     z03 = ((y03 XOR x03) XOR (((y02 XOR x02) AND (((y01 XOR x01) AND (y00 AND x00)) OR (y01 AND x01))) OR (y02 AND x02)))
     z02 = ((y02 XOR x02) XOR (((y01 XOR x01) AND (y00 AND x00)) OR (y01 AND x01)))
     z01 = ((y01 XOR x01) XOR (y00 AND x00))
     z00 = (y00 XOR x00)

     zN = ((yN XOR xN)  XOR  ((right subtree z(N-1) where toplevel XOR are replaced by AND) OR (yN-1 AND xN-1)

                                zN = XOR
                            /               \
                          XOR               OR
                         /   \           /            \
                       yN    xN    zN-1{XOR<-AND}      AND
                                                      /   \
                                                   yN-1   xN-1
     (rule 1) So a zN should be a XOR
     (rule 2) a left of zN should be a XOR
     (rule 3) a right of zN should be a OR
     (rule 4) a right of a right of zN should be an AND
     except for the final z45, the final carry,which should only be an OR, without the left part
     We also know that (for our input) rules from z00 to z05 are ok so we don't test for those
     (just to avoid writing custom rules for z00 and z01).
     We evaluate symbolicaly the rules and fully expand them (while remembering which rule correspond
     to which subtree) and test with simple pattern matching on rules put in "normal form", as above.

     To just find only the right rules, we must record them and not check them when they appear again.

     This covers the 4x2 rules that are swapped. No need to do anything else, just sort and print.
     Instead of returning a boolean, we return an Error t with the message, for debugging purpose.
  *)

  let update w r =
    Format.kasprintf (fun msg -> r := w :: !r; Error msg)

  let check_z1 Expr.{expr;wire} t bad_rules =
    Hashtbl.add t wire ();
    match expr with
      Binop (e1, (AND|OR as op), e2) -> (* rule 2 *)
      update wire bad_rules "Rule %s Left of z should be XOR but is %a" wire pp_op op
    | _ -> Ok ()

  let check_and Expr.{expr; wire} t bad_rules =
    Hashtbl.add t wire ();
    match expr with
      Binop (_, (OR|XOR as op), _) -> (* rule 4 *)
      update wire bad_rules "Rule %s should be AND but is %a" wire pp_op op
    | _ -> Ok()

  let check_z2 Expr.{expr;wire} t bad_rules =
    Hashtbl.add t wire ();
    match expr with
      Binop (e1, (AND|XOR as op), e2) -> (* rule 3 *)
      update wire bad_rules "Rule %s right of z should be OR but is %a" wire pp_op op
    | Binop (e1, OR, e2) ->
      Result.bind (check_and e1 t bad_rules) (fun () -> check_and e2 t bad_rules)
    | _ -> Ok ()

  let check_z (Expr.{expr;wire}as e) t bad_rules =
    Hashtbl.add t wire ();
    if wire <= "z01" then Ok ()
    else if wire = "z45" then check_z2 e t bad_rules
    else match expr with
        Binop (e1, XOR, e2) ->
        Result.bind (check_z1 e1 t bad_rules) (fun () -> check_z2 e2 t bad_rules)
      | Binop (_, op, _) -> update wire bad_rules "Rule %s should be XOR but is %a" wire pp_op op
      | _ -> Ok ()

  let rule_checker ?(debug=false) wires gates =
    let sw = sym_wires wires in
    let full_formulae = sym_eval sw gates in
    let memo = ~%[] in
    let bad_rules = ref [] in
    (* Because sw is a map, it will iterate the rules by increasing name,
       so z00, before z01, … the other rules can be skipped, they will be
       checked recursively  *)
    StrMap.iter (fun w e ->
        if not (memo %? w) && w.[0] = 'z' then
          match check_z e memo bad_rules with
            Error msg when debug -> Format.printf "Rule %s is invalid: %s \n%!" w msg
          | _ -> ()
      ) full_formulae;
    assert (List.length !bad_rules = 8);
    List.sort compare !bad_rules |> String.concat ","

  let solve_part2 () =
    let wires, gates = read_input () in
    let s = rule_checker wires gates in
    Ansi.(printf "%a%s%a\n%!" fg green s clear color)
end

let () = Solution.register_mod (module S)