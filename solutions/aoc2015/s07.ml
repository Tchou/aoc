open Utils
open Syntax

module S =
struct
  let name = Name.mk "s07"

  type op = Wire of string | Signal of int
  type binop = And | Or | Lshift | Rshift
  type unop = Not
  type expr = 
      Cst of op
    | Binop of op * binop * op
    | Unop of unop * op


  let read_op s =
    match int_of_string_opt s with
      Some n -> Signal n
    | None -> Wire s

  let read_input () =
    let graph = ~%[] in
    Input.fold_fields ' ' (fun () l ->
        let dst, expr =
          match l with
          | [op; "->"; dst] -> dst, Cst (read_op op)
          | [op1; s; op2; "->"; dst ] -> 
            let binop = match s with 
                "AND" -> And
              | "OR" -> Or
              | "LSHIFT" -> Lshift
              | "RSHIFT" -> Rshift
              | _ -> assert false in
            dst, Binop(read_op op1, binop, read_op op2)
          | [ "NOT"; op; "->"; dst] -> dst, Unop(Not, read_op op)
          | _ -> assert false
        in
        graph.%{dst} <- expr) ();
    graph

  let eval_op env op = 
    match op with
    | Signal i -> i
    | Wire s -> env.%{s}

  let mask = 0xffff
  let rec eval_expr env e =
    match e with
      Cst o -> eval_op env o
    | Unop (Not, o) -> 
      let n = eval_op env o in
      (lnot n) land mask
    | Binop (o1, op, o2) ->
      let n1 = eval_op env o1 in
      let n2 = eval_op env o2 in
      let r = match op with
        | And -> n1 land n2
        | Or -> n1 lor n2
        | Lshift -> n1 lsl n2
        | Rshift -> n1 lsr n2
      in
      r land mask

  let get_dep env o l =
    match o with 
      Wire n when not (env %? n) -> n::l
    | _ -> l
  let unsat_deps env expr =
    match expr with
      Cst (o) | Unop(_, o) -> get_dep env o []
    | Binop(o1,_, o2) -> get_dep env o1 (get_dep env o2 [])

  let eval graph final =
    let env = ~%[] in
    let rec loop dst =
      let expr = graph.%{dst} in
      let deps = unsat_deps env expr in
      List.iter loop deps;
      env.%{dst} <- eval_expr env expr
    in
    loop final;
    env.%{final}

  let solve_part1 () =
    let graph = read_input () in
    let n = eval graph "a" in
    Solution.printf "%d" n

  let solve_part2 () = 
    let graph = read_input () in
    let n = eval graph "a" in
    let () = graph.%{"b"} <- Cst (Signal n) in
    let n = eval graph "a" in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)