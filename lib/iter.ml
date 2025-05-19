
let conv_seq to_seq col f init =
  let s = to_seq col in
  Seq.fold_left f init s


let count (to_seq:'a -> 'b Seq.t) col =
  conv_seq to_seq col (fun acc _ -> 1+acc) 0

let count_if to_seq f col =
  conv_seq to_seq col
    (fun acc v -> if f v then 1+acc else acc) 0

module type NUM = 
sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
end

let sum (type a) to_seq 
    (module M : NUM with type t =a) col =
  conv_seq to_seq col
    (fun acc v -> M.add v acc) M.zero

let prod (type a)
    to_seq 
    (module M : NUM with type t =a) col =
  conv_seq to_seq col
    (fun acc v -> M.mul v acc) M.one

let agg_cmp_opt to_seq cmp (compare:'b -> 'b -> int) col =
  let f a b =
    match a, b with
      None, _ -> Some b
    | Some a, b -> Some (if cmp (compare a b) 0 then a else b)
  in
  conv_seq  to_seq col
    f None

let agg_cmp to_seq s cmp compare col =
  match agg_cmp_opt to_seq cmp compare col with
    None -> invalid_arg ("Invalid argument: " ^ s)
  | Some v -> v

let max_opt to_seq ?(compare=compare) =
  agg_cmp_opt to_seq (>) compare

let min_opt to_seq ?(compare=compare) =
  agg_cmp_opt  to_seq (<) compare

let max to_seq ?(compare=compare) col =
  agg_cmp to_seq "max" (>) compare col

let min to_seq ?(compare=compare) col =
  agg_cmp to_seq "min"  (<) compare col

let range ?(step=1) start stop =
  let rec loop i () =
    if i >= stop then Seq.Nil
    else
      Seq.Cons(i, (loop (i+step)))
  in
  loop start

let rec insert e l =
  match l () with
    Seq.Nil -> Seq.cons (Seq.cons e Seq.empty) Seq.empty
  | Seq.Cons(x, ll) ->
    Seq.cons
      (Seq.cons e (Seq.cons x ll))
      (Seq.map (Seq.cons x) (fun () -> insert e ll ()))

let rec perm_aux l =
  match l () with
    Seq.Nil -> Seq.cons Seq.empty Seq.empty
  | Seq.Cons(e , ll) ->
    Seq.flat_map (insert e) (fun () -> perm_aux ll ())

let perm to_seq l = perm_aux (to_seq l)

let rec powerset_aux l =
  match l () with
    Seq.Nil -> Seq.cons Seq.empty Seq.empty
  | Seq.Cons(x, ll) ->
    let s = fun () -> powerset_aux ll () in
    Seq.append s (Seq.map (Seq.cons x) s)

let powerset to_seq l = powerset_aux (to_seq l)


let pairs_aux sym refl l =
  let rec loop1 l1 acc =
    match l1 () with
      Seq.Nil -> Seq.empty
    | Seq.Cons(x1, ll1) ->
      let rec loop2 l2 =
        match l2 () with
          Seq.Nil -> Seq.empty
        | Seq.Cons(x2 , ll2) ->
          Seq.cons (x1, x2) (loop2 ll2)
      in
      let s1 = fun () -> match sym, refl with
          true, true -> loop2 l ()
        | true, false -> loop2 (Seq.append acc ll1) ()
        | false, true -> loop2 l1 ()
        | false, false -> loop2 ll1 ()
      in
      Seq.append s1 (fun () -> loop1 ll1 (Seq.cons x1 acc) ())
  in
  loop1 l Seq.empty

let pairs to_seq ?(sym=true) ?(refl=true) l =
  pairs_aux sym refl (to_seq l)

let product to_seq1 to_seq2 l1 l2 =
  Seq.product (to_seq1 l1) (to_seq2 l2) 

let fst to_seq l = Seq.map fst (to_seq l)
let snd to_seq l = Seq.map snd (to_seq l)

exception Break
exception Continue
let break () = raise Break
let continue () = raise Continue

let for_ ?(step=1) start stop f =
  let rec loop i =
    if i <= stop then
      match f i with
        () -> loop (i+step)
      | exception Continue -> loop (i+step)
  in
  try
    loop start
  with Break -> ()

let while_ cond body =
  let rec loop () =
    if cond () then
      match body () with
        () -> loop ()
      | exception Continue -> loop ()
  in
  try
    loop ()
  with Break -> ()

let seq x = x
let iseq l = Seq.mapi (fun i x -> (i, x)) l
let list = List.to_seq
let ilist l = iseq (list l)
let array = Array.to_seq
let iarray = Array.to_seqi
let keys = Hashtbl.to_seq_keys
let values = Hashtbl.to_seq_values
let items = Hashtbl.to_seq

let string = String.to_seq
let istring = String.to_seqi

let bytes = Bytes.to_seq

let ibytes = Bytes.to_seqi

type range = Range of int * int | Step of int * int * int
let (--) a b = Range (a, b)

let (-%) r n = 
  match r with
    Range (a, b) 
  | Step (a, b, _) -> Step (a, b, n) 

let (let& ) r e =
  match r with
    Range (a, b) -> for_ a b e
  | Step (a, b, step) -> for_ ~step a b e

let int = (module Int : NUM with type t = int)
let int64 = (module Int64 : NUM with type t = int64)
let int32 = (module Int32 : NUM with type t = int32)
let float = (module Float : NUM with type t = float)
