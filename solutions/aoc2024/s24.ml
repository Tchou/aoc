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
    let todo = ref [] in
    InputUntil.fold_lines (fun acc l ->
        if l = "" then false, ()
        else
          Scanf.sscanf l "%[^:]: %d" (fun s i -> true, (wires := StrMap.add s  i !wires))
      ) ();
    Input.fold_scan "%[a-z0-9] %[A-Z] %[a-z0-9] -> %[a-z0-9]"
      (fun acc r1 g r2 r ->
         gates := StrMap.add r (r1, parse_gate g, r2) !gates;
         if r.[0] = 'z' then todo:=r::!todo
      ) ();
    !wires, !gates, !todo

  let eval_op o v1 v2 _ =
    match o with
      OR -> (v1 lor v2) land 1
    | AND -> (v1 land v2) land 1
    | XOR -> (v1 lxor v2) land 1


  let eval eval_op wires gates todo =
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
    loop todo wires

  let compute f init todo wires =
    List.sort (fun x y -> compare y x) todo
    |> List.fold_left (f wires) init

  let solve_part1 () =
    let wires, gates, todo = read_input () in
    let wires = eval eval_op wires gates todo in
    let n = compute (fun wires acc z ->
        assert (z.[0] = 'z'); 2*acc + StrMap.find z wires)
        0 todo wires
    in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  module Expr =
  struct
    type expr =
        Wire of wire
      | Binop of t * gate * t
    and t = { id : int; node : expr; name : string}
    module Key =
    struct
      type nonrec t = t
      let equal e1 e2 =
        match e1.node, e2.node with
          Wire s1, Wire s2 -> s1 = s2
        | Binop (k11, op1, k12), Binop(k21, op2, k22) ->
          op1 == op2 && k11 == k21 && k12 == k22
        | _ -> false
      let hash e =
        match e.node with
          Wire s -> Hashtbl.hash s
        | Binop (k1, op, k2) ->
          k1.id + 17 * Hashtbl.hash op + 257 * k2.id
    end
    module H = Hashtbl.Make(Key)

    let memo = H.create 16
    let id = ref 0
    let cons e name =
      let n = { id = !id; node = e; name} in
      try H.find memo n with
        Not_found ->
        incr id;
        H.replace memo n n; n
    let wire s = cons (Wire s) s

    let memo_letter = H.create 16

    let rec letter e =
      try H.find memo_letter e with
        Not_found ->
        match e.node with
          Wire s -> s
        | Binop(e1, _, e2) ->
          let s1 = letter e1 in
          let s2 = letter e2 in
          let s = if s1 >= s2 then s1 else s2 in
          H.replace memo_letter e s;
          s

    let binop e1 op e2 name =
      let s1 = letter e1 in
      let s2 = letter e2 in
      if s1 >= s2 then cons (Binop(e1, op, e2)) name
      else cons (Binop (e2, op, e1)) name
    let equal e1 e2 = e1 == e2
    let hash e = e.id
  end

  let rec pp fmt e =
    let open Format in
    match e.Expr.node with
      Wire s -> fprintf fmt "%s" s
    | Binop  (e1, op, e2) -> fprintf fmt "(%a %a %a)" pp e1 pp_op op pp e2

  let sym_eval wires gates todo =
    let sym_eval_op op r1 r2  n =
      Expr.binop r1 op r2 n
    in
    eval sym_eval_op wires gates todo

  let sym_wires wires =
    let sym_wires = ~%[] in
    StrMap.fold (fun r _ acc -> StrMap.add r Expr.(wire r) acc) wires StrMap.empty

  module StrSet = Set.Make(String)

  let display todo wires =
    compute (fun wires () z ->
        Format.printf "%s = %a\n%!" z pp wires.%{z}) () todo wires

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
     So a zN should be a XOR
     a left of zN should be a XOR
     a right of zN should be a OR
     a right of a right of zN should be an AND
     except for the final z45, the final carry,which should only be an OR, without the left part
     We also know that (for our input) rules from z00 to z05 are ok so we don't test for those
     (just to avoid writing custom rules for z00 and z01).
     We evaluate symbolicaly the rules and fully expand them (while remembering which rule correspond
     to which subtree) and test with simple pattern matching on rules put in "normal form", as above.

     To just find only the right rules, we must record them and not check them when they appear again.

     This covers the 4x2 rules that are swapped. No need to do anything else, just sort and print.
  *)

  let update w r = r := w :: !r

  let check_z1 wires z w t bad_rules =
    Hashtbl.add t w ();
    let num = String.sub z 1 2 in
    let e = StrMap.find w wires in
    match e.Expr.node with
      Binop (e1, op, e2) ->
      if op <> XOR then Format.(update w bad_rules; asprintf "Rule %s Left of z should be XOR but is %a" w pp_op op) else
      if e2.name <> "x" ^ num || e1.name <> "y" ^ num then
        (update w bad_rules; "Left of z is not between x and y")
      else ""
    | Wire _ -> "Should not be a wire"

  let check_and wires w t bad_rules =
    Hashtbl.add t w ();
    let e = StrMap.find w wires in
    match e.Expr.node with
      Binop (_, op, _) ->
      if op <> AND then Format.(update w bad_rules; asprintf "Rule %s should be AND but is %a" w pp_op op)
      else ""
    | Wire _ -> "Should not be a wire"

  let check_z2 wires w t bad_rules =
    Hashtbl.add t w ();
    let e = StrMap.find w wires in
    match e.Expr.node with
      Binop (e1, op, e2) ->
      if op <> OR then Format.(update w bad_rules; asprintf "Rule %s right of z should be OR but is %a" w pp_op op)
      else let s = check_and wires e1.Expr.name t bad_rules in
        if s = "" then check_and wires e2.Expr.name t bad_rules
        else s
    | Wire _ -> "Right of z should not be a wire"

  let check_z wires w t bad_rules =
    Hashtbl.add t w ();
    if w <= "z05" then ""
    else
      let e = StrMap.find w wires in
      match e.Expr.node with
        Binop (e1, op, e2) ->
        if w = "z45" then check_z2 wires w t bad_rules
        else if op = XOR then
          let s = check_z1 wires w e1.Expr.name t bad_rules in
          if s = "" then check_z2 wires e2.Expr.name t bad_rules
          else s
        else Format.(update w bad_rules; asprintf "Rule %s should be XOR but is %a" w pp_op op)
      | Wire _ -> "Z rule should not be a single wire"


  let rule_checker ?(debug=false) wires gates =
    let z_rules =
      StrMap.fold (fun w _ acc -> if w.[0] = 'z' then w::acc else acc) gates []
      |> List.sort compare
    in
    let sw = sym_wires wires in
    let sw = sym_eval sw gates z_rules in
    let t = ~%[] in
    let bad_rules = ref [] in
    List.iter (fun w ->
        if not (t %? w) then
          let s = check_z sw w t bad_rules in
          if s <> "" && debug then Format.printf "Rule %s is invalid: %s \n%!" w s
      ) z_rules;
    assert (List.length !bad_rules = 8);
    List.sort compare !bad_rules |> String.concat ","

  let solve_part2 () =
    let wires, gates, todo = read_input () in
    let s = rule_checker wires gates in
    Ansi.(printf "%a%s%a\n%!" fg green s clear color)
end

let () = Solution.register_mod (module S)