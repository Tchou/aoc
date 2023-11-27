open Utils

module S =
struct
  let name = Name.mk "s24"

  type binop = Add | Mul | Div | Mod | Eq
  type expr =
    | Num of int
    | In of int (* 0 based. *)
    | Var of int (* w = 0, x = 1; y = 2; z = 3 *)
    | Op of expr * binop * expr

  type instr = Input of int
             | Binop of int * binop * expr

  let pp_op = function
    | Add -> '+'
    | Mul -> '*'
    | Div -> '/'
    | Mod -> '%'
    | Eq -> '='
  let rec  pp ppf e =
    let open Ansi in
    match e with
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
      Some i -> Num i
    | None -> Var (var_code c)
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

  let num n = Num n, (n,n)
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

  let rec eval_expr_int forward cache vars inputs e =
    match e with
      Num n  -> num n
    | In i -> let n = inputs.(i) in
      if n = 0 then e, (1, 9) else num n
    | Var i -> vars.(i),(min_int, max_int)
    | Op (e1, op, e2) ->
      let (v1, (min1, max1)) as res1 = eval_expr_int forward cache vars inputs e1 in
      let (v2, (min2, max2)) as res2 = eval_expr_int forward cache vars inputs e2 in
      match op, v1, v2 with
      | Add, Num n1, Num n2 -> num (n1+n2)
      | Add, Num 0, _ -> v2, (min2, max2)
      | Add, _, Num 0 -> v1, (min1, max1)
      | Add, Op(v11, Add, Num nv12), Num n2
      | Add, Num n2, Op(v11, Add, Num nv12) ->
        Op (v11, Add, Num (nv12 + n2)), (min1+min2, max1+max2)

      | Add, Op(Num nv11, Add, v12), Num n2
      | Add, Num n2, Op(Num nv11, Add, v12) ->
        Op (v12, Add, Num (nv11 + n2)), (min1+min2, max1+max2)

      | Add, _, _ -> Op(v1, Add, v2), (min1+min2, max1+max2)

      | Mul, Num n1, Num n2 -> num (n1*n2)
      | Mul, Num 1, _ -> res2
      | Mul, _, Num 1 -> res1
      | Mul, Num 0, _
      | Mul, _, Num 0 -> num0
      | Mul, Op(v11, Add, Num nv12), Num n2
      | Mul, Num n2, Op(v11, Add, Num nv12) ->
        Op (Op(v11,Mul,Num n2), Add, Num (nv12 * n2)), (min1*min2, max1*max2)


      | Mul, Op(Num nv11, Add, v12), Num n2
      | Mul, Num n2, Op(Num nv11, Add, v12) ->
        Op (Op(v12,Mul,Num n2), Add, Num (nv11 * n2)), (min1*min2, max1*max2)


      | Mul, _, _ -> Op(v1, Mul, v2), (min1*min2, max1*max2)

      | Div, Num n1, Num n2 -> num (n1/n2)
      | Div, _, Num 1 -> res1
      | Div, _, Num n2 -> Op(v1, Div, v2), (min1/n2, max1/n2)
      | Div, _, _ -> assert false

      | Mod, Num n1, Num n2 -> num (n1 mod n2)
      | Mod, _ , Num n2 ->
        if min1 >= 0 && max1 < n2 then res1
        else Op(v1, Mod, v2), (0, n2-1)
      |  Mod, _, _ -> Op(v1, Mod, v2), (0, max2 - 1)

      | Eq, Num n1, Num n2 ->
        if n1 = n2 then num1 else num0
      | Eq, Op(Num n, Eq, In i), Num 0 ->
        if n < 1 || n > 9 then num1
        else begin
          if forward then
            cache.(i) <- insert n cache.(i)
          else
            cache.(i) <- insert_rev n cache.(i);
          Op(v1, Eq, v2), (0, 1)
        end
      | Eq, _, _ ->
        if max1 < min2 || max2 < min1 then num0
        else Op(v1, Eq, v2), (0, 1)


  let eval_prog prog inputs =
    let vars = Array.make 4 (Num 0) in
    let input = ref ~-1 in
    let cache = Array.make 14 [] in
    let rec loop = function
        [] -> vars
      | (Input (var)) :: ll ->
        incr input;
        let n = inputs.(!input) in
        let v = if n = 0 then In !input else Num n in
        vars.(var) <- v;
        loop ll
      | (Binop (i, op, e)) :: ll ->
        let v, _ = eval_expr_int true cache vars inputs (Op(Var i, op, e)) in
        vars.(i) <- v;
        loop ll
    in loop prog


  let enum_opt inputs forward e =
    let default = if forward then [1;2;3;4;5;6;7;8;9] else
        [9;8;7;6;5;4;3;2;1]
    in
    let cache = Array.make 14 [] in
    let rec loop i e =
      if i <= 13 then begin
        let digits = if cache.(i) == [] then default else cache.(i) in
        loop_digits i e digits;
      end else
        match e with
          Num 0 -> raise Exit
        | _ -> ()
    and loop_digits i e l =
      match l with
        [] -> inputs.(i) <- 0
      | j::ll ->
        inputs.(i) <- j;
        let ei, (a, b) = eval_expr_int forward cache [| |] inputs e in
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
    let () = enum_opt inputs forward z in
    String.join "" (Array.map string_of_int inputs)
    |> Ansi.printf "%s\n"

  let solve_part1 () = solve false
  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)