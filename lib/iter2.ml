type 'a t = 
    Persistent of { f : ('a -> unit) -> unit }
  | Ephemeral of { mutable f : ('a -> unit) -> unit }

let is_persistent = function Persistent _ -> true | _ -> false
exception Break
exception Continue
exception EphemeralReused

let break () = raise Break
let continue () = raise Continue
let with_cont f =
  fun x -> try 
      f x 
    with Continue -> ()

let with_bc f x =
  let x = with_cont x in
  try
    f x 
  with
    Break -> ()

let get_f = function Persistent { f; _ } | Ephemeral { f; _} -> f

let set_f r f =
  match r with
    Ephemeral _ -> Ephemeral { f }
  | Persistent _ -> Persistent { f }

let (@.) it f : unit = 
  with_bc (get_f it) f;
  match it with
    Ephemeral r -> r.f <-  (fun _ -> raise EphemeralReused)
  | Persistent _ -> ()
let p_iter iter l =
  Persistent{ f = (fun f -> iter f l) }
let e_iter iter l =
  Ephemeral{ f = (fun f -> iter f l)}



let of_iter = e_iter

let map f (it : 'a t)  : 'b t =
  set_f it (fun g -> it @. (fun x -> g (f x)))

let filter f (it : 'a t) : 'a t =
  set_f it (fun g -> it @. (fun x -> if f x then g x))

let filter_map f (it : 'a t) : 'b t =
  set_f it (fun g -> it @. (fun x -> 
      match f x with 
        Some v -> g v
      | None -> ()))

let enum it =
  let i = ref ~-1 in
  map (fun x -> (incr i; !i, x)) it

let list l = p_iter List.iter l
let ilist l = list l |> enum
let array a = e_iter Array.iter a
let iarray a = array a |> enum
let string s = p_iter String.iter s
let istring s = string s |> enum
let bytes b = e_iter Bytes.iter b
let ibytes b = bytes b |> enum
let seq s = e_iter Seq.iter s
let iseq s = seq s |> enum
let items h =
  e_iter (fun f -> Hashtbl.iter (fun k v -> f (k,v))) h

let keys h =
  e_iter (fun f -> Hashtbl.iter (fun k _ -> f k)) h

let values h =
  e_iter (fun f -> Hashtbl.iter (fun _ v -> f v)) h
let option o = p_iter Option.iter o
let count it =
  let c = ref 0 in
  it @. (fun _ -> incr c);
  !c

let count_if p it =
  let c = ref 0 in
  it @. (fun x -> if p x then incr c);
  !c

module type NUM = 
sig
  type t
  val zero : t 
  val one : t
  val minus_one : t
  val add : t -> t -> t 
  val mul : t -> t -> t
  val compare : t -> t -> int
end

let int : (module NUM with type t = int) = (module Int)
let int64 : (module NUM with type t = int64) = (module Int64)
let int32 : (module NUM with type t = int32) = (module Int32)
let float : (module NUM with type t = float) = (module Float)
let zarith : (module NUM with type t = Z.t) = (module Z)

let sum (type a) (module M : NUM with type t = a) it =
  let total = ref M.zero in
  it @. (fun v -> total := M.add v !total);
  !total

let prod (type a) (module M : NUM with type t = a) it =
  let total = ref M.one in
  it @. (fun v -> total := M.mul v !total);
  !total

let comp_opt comp ?(compare=compare) it =
  let m = ref None in
  it @. (fun v -> match !m with
        None -> m := Some v
      | Some w -> if comp (compare v w) 0 then m := Some v);
  !m

let max_opt ?compare = comp_opt (>) ?compare
let min_opt ?compare = comp_opt (<) ?compare
let last_max_opt ?compare = comp_opt (>=) ?compare
let last_min_opt ?compare = comp_opt (<=) ?compare

let fail msg = 
  Format.kasprintf 
    (fun s -> failwith ( "Iter2." ^s)) msg

let unopt name (f : ?compare:('a -> 'a -> int) -> 'a t -> 'a option) ?compare it =
  match f ?compare it with
    Some v -> v
  |  None -> fail "%s" name

let max_ ?compare = unopt "max" max_opt ?compare
let min_ ?compare = unopt "min" min_opt ?compare 
let last_max ?compare = unopt "last_max" max_opt ?compare
let last_min ?compare = unopt "last_min" min_opt ?compare 

let range (type a) (module M : NUM with type t = a)
    ?(start=M.zero) ?(step=M.one) stop =
  if M.compare step M.zero = 0 then invalid_arg "Null step";
  let step_sign = M.compare step M.zero in

  Persistent{ f = with_bc ((fun f -> 
      let rec loop i =
        if M.compare i stop < 0 then
          let () = f i in
          let ni = M.add i step in
          let dir = M.compare ni i in
          if dir > 0 && step_sign > 0 || dir < 0 && step_sign < 0 then
            loop (M.add i step)
            (* else overflow *)
      in
      loop start )); }

let iter f (it : 'a t) = it @. f
let iteri f it = iter f (enum it)
let first (type a) (it : a t) =
  let exception Found of a in
  try
    it @. (fun x -> raise_notrace (Found x));
    fail "first"
  with Found v -> v

let take n it : 'a t =
  let i = ref ~-1 in
  set_f  it (fun f -> it @. (fun x -> 
      if (incr i;!i) < n then f x else break()))

let fold f (init : 'b) (it : 'a t) =
  let acc = ref init in
  it @. (fun x -> acc := f !acc x);
  !acc

let to_list_rev_len it =
  let acc = ref [] in
  let len = ref 0 in
  it @. (fun x -> acc := x :: !acc; incr len);
  !acc, !len

let to_list_rev it = 
  let a, _ = to_list_rev_len it in a

let to_list it =
  List.rev (to_list_rev it)

let to_seq it =
  List.to_seq (to_list it)

let to_array it =
  let lr, len = to_list_rev_len it in
  match lr with
    [] -> [||]
  | x :: _ ->
    let a = Array.make len x in
    let l = ref lr in
    for i = len - 1 downto 0 do
      a.(i) <- List.hd !l;
      l := List.tl !l;
    done;
    a
let to_string it =
  let b = Buffer.create 16 in
  iter (fun c -> Buffer.add_char b c) it;
  Buffer.contents b

let to_bytes it = Bytes.of_string (to_string it)



(* Heap's algorithm *)
let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let rec perm_aux f k a =
  if k = 1 then
    f (Array.to_list a)
  else begin
    perm_aux f (k-1) a;
    for i = 0 to k - 2 do
      if k land 1 = 0 then
        swap a i (k-1)
      else
        swap a 0 (k-1);
      perm_aux f (k-1) a;
    done
  end
let perm it =
  let a = to_array it in
  Persistent{f = (with_bc (fun f -> perm_aux f (Array.length a) a))}

let powerset it =
  let a = to_array it in
  let len = Array.length a in
  let rec loop_bits i z acc =
    if i < 0 then acc
    else
      let acc =
        if Z.testbit z i then 
          a.(i) ::acc
        else acc 
      in
      loop_bits (i-1) z acc 
  in
  let rec loop f z =
    if Z.numbits z <= len then begin
      f (loop_bits (len-1) z []);
      loop f (Z.succ z)
    end
  in
  Persistent{f = with_bc (fun f -> loop (with_cont f) Z.zero) }

let persist it =
  if is_persistent it then it
  else
    list (to_list it)
let product it1 it2 =
  let it1 = persist it1 in
  let it2 = persist it2 in
  Persistent{
    f = with_bc (fun f -> 
        let f = with_cont f in
        it1 |> iter (fun x ->
            it2 |> iter (fun y -> f (x, y))
          )) }

let pairs ?(sym=true) ?(refl=true) it =
  let a = to_array it in
  let last = Array.length a - 1 in
  Persistent{ f = with_bc (fun f ->
      for i = 0 to last do
        let ai = a.(i) in
        for j = if sym then 0 else i to last do 
          if i <> j || refl then f (ai, a.(j))
        done
      done)
    }

let exists p it =
  try
    it @. (fun x -> if p x then raise_notrace Exit);
    false
  with Exit -> true

let forall p it = not (exists (Fun.negate p) it)

let sort ?(compare=compare) it =
  let a = to_array it in
  Array.fast_sort compare a;
  array a

let find_map (type a) f it =
  let exception Found of a option in
  try
    it @. (fun x -> match f x with 
          None -> ()
        | o -> raise_notrace (Found o));
    None
  with Found o -> o

let find_opt f it = find_map (fun x -> if f x then Some x else None) it
let find f it =
  match find_opt f it with
    None -> raise Not_found
  | Some  v -> v

let (let->) x f =
  iter f x

let (++) it1 it2 =
  let f g = it1 @. g;it2 @. g in
  if is_persistent it1 && is_persistent it2
  then Persistent{f}
  else Ephemeral{f}

let cons x it =
  set_f  it (fun g ->
      g x;
      it @. g)
let snoc it x =
  set_f it (fun g ->
      it @. g;
      g x)

let cycle it =
  let it = persist it in
  Persistent{f = fun g ->
      while true do it @. g done
    }

let empty = Persistent{f = fun _ -> ()}