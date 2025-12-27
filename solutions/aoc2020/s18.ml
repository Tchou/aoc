open Utils
module S =
struct
  let name = Name.mk "s18"

  type op =
      Int of int
    | Add
    | Mul
    | LP
    | RP

  let read_input () =
    Input.list_lines (fun line ->
        let expr =
          line
          |> String.fold_left (fun acc c ->
              match c with
              '0'..'9' -> (Int (Char.code c - Char.code '0'))::acc
              | '(' -> LP :: acc
              | ')' -> RP :: acc
              | '+' -> Add :: acc
              | '*' -> Mul :: acc
              | _ -> acc
            ) []
        in
        (List.rev expr))

  let prio1 _ = 10
  let prio2 = function
      Add -> 10
    | Mul -> 5
    | _ -> 100

  let eval prio expr =
    let push_result op stack =
      match op, stack with
        Int i, _ -> i :: stack
      | Add, e1 :: e2 :: ss -> (e2 + e1)::ss
      | Mul, e1 :: e2 :: ss -> (e2 * e1)::ss
      | _ -> failwith "empty stack"
    in
    let rec loop rstack ostack expr =
      match expr with
        (Int _ as i) :: ee -> loop (push_result i rstack) ostack ee
      | (Add | Mul as op)::ee ->
        let rstack, ostack = add_to_stack rstack ostack op in
        loop rstack ostack ee
      | LP :: ee -> loop rstack (LP::ostack) ee
      | RP :: ee ->
        let rstack, ostack = pop_until_lp rstack ostack in
        loop rstack ostack ee
      | [] -> empty_ostack rstack ostack
    and add_to_stack rstack ostack op =
      match ostack with
        [] -> rstack, [op]
      | LP::_ -> rstack, op::ostack
      | RP::_ | Int _ :: _ -> assert false
      | op2 :: ostack ->
        if prio op2 >= prio op then
          add_to_stack (push_result op2 rstack) ostack op
        else rstack, op::op2::ostack
    and pop_until_lp rstack ostack =
      match ostack with
        LP:: oo -> rstack, oo
      | op :: oo -> pop_until_lp (push_result op rstack) oo
      | [] -> assert false
    and empty_ostack rstack ostack =
      match ostack with
        [] -> rstack
      | op :: oo -> empty_ostack (push_result op rstack) oo
    in
    match loop [] [] expr with
      [ i ] -> i
    | _ -> assert false


  let solve prio =
    let exprs = read_input () in
    let n = 
      let open Iter in
      exprs
      |> list
      |> map (eval prio)
      |> sum int
    in
    Solution.printf "%d" n

  let solve_part1 () = solve prio1
  let solve_part2 () = solve prio2
end

let () = Solution.register_mod (module S)