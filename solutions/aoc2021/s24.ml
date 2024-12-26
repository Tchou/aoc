open Utils

module S =
struct
  let name = Name.mk "s24"

  type binop = Add | Mul | Div | Mod | Eq
  module Expr =
  struct
    type expr =
      | Num of int
      | In of int (* 0 based. *)
      | Var of int (* w = 0, x = 1; y = 2; z = 3 *)
      | Op of t * binop * t
    and t = { id : int; expr : expr; inputs : int array }
    module K = struct
      type nonrec t = t
      let equal e1 e2 =
        e1 == e2 ||
        e1.expr == e2.expr ||
        match e1.expr, e2.expr with
          (Num i1, Num i2)
        | (In i1, In i2)
        | (Var i1, Var i2) -> i1 == i2
        | Op(e11, op1, e12),
          Op(e21, op2, e22) ->
          op1 == op2 && e11 == e21 && e12 == e22
        | _ -> false
      let hash e =
        match e.expr with
          Num i -> i * 17
        | In i -> i
        | Var i -> i*2+1
        | Op (e1, op, e2) ->
          253 * e1.id + Hashtbl.hash op + 997 * e2.id
    end
    module H = Hashtbl.Make(K)
    let memo = H.create 16
    let id = ref 0
    let cons expr inputs =
      let n = { id = !id; expr; inputs } in
      try H.find memo n with
        Not_found ->
        H.replace memo n n;
        incr id;
        n

    let in_ i = cons (In i) [| i |]
    let var i = cons (Var i) [| |]
    let num i = cons (Num i) [| |]
    let[@tail_mod_cons] rec merge l1 l2 =
      match l1, l2 with
        [], _ -> l2
      | _, [] -> l1
      | (i1:int) :: ll1, i2::ll2 ->
        if i1 < i2 then i1 :: merge ll1 l2
        else if i1 == i2 then i1 :: merge ll1 ll2
        else i2 :: merge l1 ll2
    let merge a1 a2 =
      merge  (Array.to_list a1) (Array.to_list a2)
      |> Array.of_list
    let op e1 op e2 = cons (Op(e1, op, e2)) (merge e1.inputs e2.inputs)

    let hash e = e.id
    let equal e1 e2 = e1 == e2

  end
  type instr = Input of int
             | Binop of int * binop * Expr.t

  let pp_op = function
    | Add -> '+'
    | Mul -> '*'
    | Div -> '/'
    | Mod -> '%'
    | Eq -> '='
  let rec pp ppf e =
    let open Ansi in
    match e.Expr.expr with
      Num i ->  fprintf ppf "%d" i
    | In i -> fprintf ppf "N%02d" (i+1)
    | Var i -> fprintf ppf "%c" (Char.chr (i + Char.code 'w'))
    | Op (e1, op, e2) ->
      fprintf ppf "@[(@[%a@] %c @[%a@])@]"
        pp e1 (pp_op op) pp e2

  let var_code s =
    Char.code s.[0] - Char.code 'w'

  let op_code s =
    match s with
    | "add" -> Add
    | "mul" -> Mul
    | "div" -> Div
    | "mod" -> Mod
    | "eql" -> Eq
    | _ -> assert false

  let cst_code c =
    match int_of_string_opt c with
      Some i -> Expr.num i
    | None -> Expr.var (var_code c)
  let load_input () =
    Input.fold_fields ' ' (fun acc l ->
        let i =
          match l with
            [ "inp"; s ] -> Input (var_code s)
          | [ op; dst; src] ->
            Binop (var_code dst, op_code op, cst_code src)
          | _ -> assert false
        in i::acc
      ) []
    |> List.rev

  let num n = Expr.num n, (n,n)
  let num0 = num 0
  let num1 = num 1

  let[@tail_mod_cons] rec insert (n:int) l =
    match l with
      [] -> [n]
    | v :: ll -> if n < v then n::l
      else if n > v then v::insert n ll
      else l

  let[@tail_mod_cons] rec insert_rev (n:int) l =
    match l with
      [] -> [n]
    | v :: ll -> if n > v then n::l
      else if n < v then v::insert_rev n ll
      else l

  module HExpr = Hashtbl.Make(struct
      type t = Expr.t * int  * Expr.t array
      let equal (e1, i1, t1) (e2, i2, t2) =
        e1 == e2 && (i1==i2) &&
        Array.for_all2 (==) t1 t2
      let hash (e, i, t) = Expr.hash e * 253 + (Hashtbl.hash i)
                           + Array.fold_left (fun acc e -> acc + 997 * e.Expr.id) 0 t
    end)

  let memo = HExpr.create 16

  let rec eval_expr_int insert cache vars inputs e =
    let key = ref 0 in
    for i = 0 to Array.length e.Expr.inputs - 1 do
      let k = Array.unsafe_get e.Expr.inputs i in
      let v = (Array.unsafe_get inputs k) lsl (k lsl 2) in
      key := !key + v
    done;
    try HExpr.find memo (e, !key, vars)
    with Not_found ->
      let res =
        match e.Expr.expr with
          Num n  -> num n
        | In i -> let n = inputs.(i) in
          if n = 0 then e, (1, 9) else num n
        | Var i -> vars.(i),(min_int, max_int)
        | Op (e1, op, e2) ->
          let (v1, (min1, max1)) as res1 = eval_expr_int insert cache vars inputs e1 in
          let (v2, (min2, max2)) as res2 = eval_expr_int insert cache vars inputs e2 in
          match op, v1.Expr.expr, v2.Expr.expr with
          | Add, Num n1, Num n2 -> num (n1+n2)
          | Add, Num 0, _ -> v2, (min2, max2)
          | Add, _, Num 0 -> v1, (min1, max1)
          | Add, Op(v11, Add, {expr = Num nv12;_}), Num n2
          | Add, Num n2, Op(v11, Add, {expr = Num nv12;_}) ->
            Expr.op v11 Add Expr.(num (nv12 + n2)), (min1+min2, max1+max2)

          | Add, Op({expr = Num nv11;_}, Add, v12), Num n2
          | Add, Num n2, Op({expr = Num nv11;_}, Add, v12) ->
            Expr.op v12 Add Expr.(num (nv11 + n2)), (min1+min2, max1+max2)

          | Add, _, _ -> Expr.op v1 Add v2, (min1+min2, max1+max2)

          | Mul, Num n1, Num n2 -> num (n1*n2)
          | Mul, Num 1, _ -> res2
          | Mul, _, Num 1 -> res1
          | Mul, Num 0, _
          | Mul, _, Num 0 -> num0
          | Mul, Op(v11, Add, {expr=Num nv12;_}), Num n2
          | Mul, Num n2, Op(v11, Add, {expr=Num nv12;_}) ->
            Expr.(op (op v11 Mul (num n2)) Add (num (nv12 * n2))), (min1*min2, max1*max2)


          | Mul, Op({expr = Num nv11;_}, Add, v12), Num n2
          | Mul, Num n2, Op({expr = Num nv11;_}, Add, v12) ->
            Expr.(op (op v12 Mul (num n2)) Add (num (nv11 * n2))), (min1*min2, max1*max2)

          | Mul, _, _ -> Expr.(op v1 Mul v2), (min1*min2, max1*max2)

          | Div, Num n1, Num n2 -> num (n1/n2)
          | Div, _, Num 1 -> res1
          | Div, _, Num n2 -> Expr.(op v1 Div v2), (min1/n2, max1/n2)
          | Div, _, _ -> assert false

          | Mod, Num n1, Num n2 -> num (n1 mod n2)
          | Mod, _ , Num n2 ->
            if min1 >= 0 && max1 < n2 then res1
            else Expr.(op v1 Mod v2), (0, n2-1)
          |  Mod, _, _ -> Expr.(op v1 Mod v2), (0, max2 - 1)

          | Eq, Num n1, Num n2 ->
            if n1 = n2 then num1 else num0
          | Eq, Op({expr =Num n;_}, Eq, {expr=In i;_}), Num 0 ->
            if n < 1 || n > 9 then num1
            else begin
              cache.(i) <- insert n cache.(i);
              Expr.(op v1 Eq v2), (0, 1)
            end
          | Eq, _, _ ->
            if max1 < min2 || max2 < min1 then num0
            else Expr.(op v1 Eq v2), (0, 1)
      in
      HExpr.replace memo (e, !key, Array.copy vars) res;
      res

  let eval_prog prog inputs =
    let vars = Array.make 4 (Expr.num 0) in
    let input = ref ~-1 in
    let cache = Array.make 14 [] in
    let rec loop = function
        [] -> vars
      | (Input (var)) :: ll ->
        incr input;
        let n = inputs.(!input) in
        let v = if n = 0 then Expr.in_ !input else Expr.num n in
        vars.(var) <- v;
        loop ll
      | (Binop (i, op0, e)) :: ll ->
        let v, _ = eval_expr_int insert cache vars inputs Expr.(op (var i) op0 e) in
        vars.(i) <- v;
        loop ll
    in loop prog


  let enum_opt inputs forward e =
    let default = if forward then [1;2;3;4;5;6;7;8;9] else
        [9;8;7;6;5;4;3;2;1]
    in
    let insert = if forward then insert else insert_rev in
    let cache = Array.make 14 [] in
    let rec loop i e =
      if i <= 13 then begin
        let digits = if cache.(i) == [] then default else cache.(i) in
        loop_digits i e digits;
      end else
        match e.Expr.expr with
          Num 0 -> raise Exit
        | _ -> ()
    and loop_digits i e l =
      match l with
        [] -> inputs.(i) <- 0
      | j::ll ->
        inputs.(i) <- j;
        let ei, (a, b) = eval_expr_int insert cache [| |] inputs e in
        if a <= 0 && 0 <= b then loop (i+1) ei;
        for k = i+1 to 13 do
          cache.(k) <- [];
        done;
        loop_digits i e ll
    in
    try loop 0 e with Exit -> ()

  let solve forward =
    let prog = load_input () in
    let inputs = Array.make 14 0 in
    let vars = eval_prog prog inputs in
    let z = vars.(var_code "z") in
    let () = HExpr.clear memo in
    let () = enum_opt inputs forward z in
    String.join "" (Array.map string_of_int inputs)
    |> Solution.printf "%s"

  let solve_part1 () = solve false
  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)