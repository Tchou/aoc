let int_of_bool = function false -> 0 | true -> 1

module String =
struct
  include Stdlib.String
  let mem c s =
    match String.index s c with
      _ -> true
    | exception Not_found -> false

  let join s a = String.concat s (Array.to_list a)

  let explode_array s =
    Array.init (String.length s) (fun i -> s.[i])

  let explode s =
    let[@tail_mod_cons] rec loop i =
      if i >= String.length s then []
      else s.[i] :: loop (i+1)
    in
    loop 0

  let implode l = String.concat "" (List.map (String.make 1) l)
  let implode_array a = implode (Array.to_list a)

  let compare_from s i p =
    let ls = String.length s in
    let lp = String.length p in
    let rec loop j =
      if i + j < ls && j < lp then
        let c = Char.compare s.[i+j] p.[j] in
        if c =  0 then loop (j+1) else c
      else
      if j < lp then -1 else 0
    in
    loop 0


  let remove_prefix ~prefix s =
    if String.starts_with ~prefix s then
      let len = String.length prefix in
      String.sub s len (String.length s - len)
    else s

  let remove_suffix ~suffix s =
    if String.ends_with ~suffix s then
      let len = String.length suffix in
      String.sub s 0 (String.length s - len)
    else s

end
module Hashtbl =
struct
  include Hashtbl
  let update tbl key f =
    match f (find_opt tbl key) with
      Some v -> replace tbl key v
    | None -> remove tbl key


  module Make (H : HashedType) = struct
    include Make(H)
    let update tbl key f =
      match f (find_opt tbl key) with
        Some v -> replace tbl key v
      | None -> remove tbl key
  end

end
module Syntax =
struct


  let (<<) x (a, b) = x > a && x < b
  let (<=<) x (a, b) = x >= a && x < b
  let (<<=) x (a, b) = x > a && x <= b
  let (<=<=) x (a, b) = x >= a && x <= b


  let ( .$[] ) b i = Bytes.get b i
  let ( .$[]<- ) b i c = Bytes.set b i c


  let (.%{}) h k = Hashtbl.find h k
  let (.%?{}) h k = Hashtbl.find_opt h k
  let (%?) h k = Hashtbl.mem h k
  let (.%{}<-) h k v = Hashtbl.replace h k v
  let ( %- ) h k = Hashtbl.remove h k
  let ( %+ ) h k = Hashtbl.replace h k ()
  let (~%) l =
    let h =  Hashtbl.create 16 in
    List.iter (fun (k,v) -> h.%{k} <- v) l;
    h

  let (or) x y = match x with Some v -> v | None -> y
  let (let*|) x y =
    match x with
      Some _ -> x
    | None -> y ()

  let (let*&) = Option.bind

  let (let*%) x y =
    match x with
      Some v -> v
    | None -> y ()
end
module type Num = sig
  type t
  val to_string : t -> string
  val zero : t
  val succ : t -> t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val rem : t -> t -> t
  val div : t -> t -> t
  val compare : t -> t -> int

end
module type MATH = sig
  type t
  val gcd : t -> t  -> t
  val egcd : t  -> t  -> t  * t  * t
  val lcm : t  -> t  -> t
  val solve_crt : (t  * t ) list -> t  * t
  val pow : t -> int -> t

end
module MathGen (N : Num) : MATH with type t = N.t = struct
  type t = N.t
  let one = N.succ N.zero

  let egcd a b =
    let rec loop old_r r old_s s old_t t =
      if N.(compare r zero = 0) then old_s, old_t, old_r
      else
        let q = N.div old_r  r in
        loop r N.(sub old_r  (mul q  r))
          s N.(sub old_s  (mul q  s))
          t N.(sub old_t  (mul q  t))
    in
    N.(loop a b one zero zero one)

  let gcd a b =
    let _, _, r = egcd a b in r
  let lcm a b = N.(mul a  (div b  (gcd a b)))

  let mod_inv a m =
    let x, _y, g = egcd a m in
    if N.(compare g one <> 0) then
      let s1 = N.to_string a in
      let s2 = N.to_string m in
      raise (Invalid_argument Printf.(sprintf
                                        "coefficient %s and %s are not coprime (gcd(%s, %s)=%s)"
                                        s1 s2 s1 s2 (N.to_string g)))
    else N. (rem (add (rem x m) m) m)

  let solve_crt pairs =
    let n_product = List.fold_left (fun acc (_, n) -> N.mul acc n) one pairs in
    let solve_term (a_i, n_i) =
      let m_i = N.div n_product  n_i in
      let inv = mod_inv m_i n_i in
      N.(mul a_i (mul m_i  inv))
    in
    let a = List.fold_left (fun acc pair -> N.add acc (solve_term pair)) N.zero pairs in
    (N.rem (N.add (N.rem a  n_product)  n_product)  n_product), n_product

  let pow a b =
    let rec loop r x e =
      if e = 0 then r
      else if e land 1 = 1 then
        loop (N.mul r x) (N.mul x  x) (e lsr 1)
      else loop r (N.mul x  x) (e lsr 1)
    in
    loop one a b
end

module Math = MathGen(Int)
module Time = struct

  let time f arg =
    let t0 = Unix.gettimeofday () in
    let res = f arg in
    let t1 = Unix.gettimeofday () in
    res, 1000. *. (t1 -. t0)
end
let get_input = function None -> Solution.get_input () | Some i -> i
module InputUntil = struct

  let list_lines ?input f =
    let input = get_input input in
    let [@tail_mod_cons] rec loop () =
      match input_line input with
      | s -> begin match f s with 
            None -> []
          | Some e -> e :: loop ()
        end
      | exception End_of_file -> []
    in
    loop ()

  let list_scan ?input fmt f =
    list_lines ?input (fun s -> Scanf.sscanf s fmt f)

  let list_fields ?input c f =
    list_lines ?input (fun s -> f (String.split_on_char c s))


  let rec fold_lines ?input f acc =
    let input = get_input input in
    match input_line input with
    | s ->
      let continue, acc = f acc s in
      if continue then fold_lines ~input f acc else acc
    | exception End_of_file -> acc

  let split_string s n =
    let len = String.length s in
    let fields = ref [] in
    let i = ref(
        let rem = len mod n in
        if rem = 0 then len - n
        else begin
          fields := [ String.sub s (len - rem) rem];
          len - n - rem;
        end)
    in
    while !i >= 0 do
      fields := (String.sub s !i n) :: !fields;
      i := !i - n;
    done;
    !fields

  let fold_substrings ?input n f acc =
    if n <= 0 then invalid_arg "fold_substrings";
    fold_lines ?input (fun acc s ->
        f acc (split_string s n)
      ) acc

  let fold_scan ?input fmt f acc =
    fold_lines ?input (fun acc s ->
        Scanf.sscanf s fmt (f acc)) acc

  let fold_fields ?input sep f acc =
    fold_lines ?input (fun acc l -> f acc (String.split_on_char sep l)) acc

  let rec fold_chars ?input f acc =
    let input = get_input input in
    match input_char input with
    | c ->
      let continue, acc = f acc c in
      if continue then fold_chars ~input f acc else acc
    | exception End_of_file -> acc


  let invalid_utf_seq () =
    invalid_arg "Invalid UTF-8 sequence"
  let utf8_length b =
    let b = Char.code b in
    if b < 128 then 1
    else if b >= 128 && b < 192 then invalid_utf_seq ()
    else if b < 224 then 2
    else if b < 240 then 3
    else 4

  let fold_uchars ?input f acc =
    let input = get_input input in
    let buffer = Bytes.create 4 in
    let get_uchar input =
      let c = input_char input in
      let l = utf8_length c in
      Bytes.set buffer 0 c;
      for i = 1 to l - 1 do
        Bytes.set buffer i (input_char input);
      done;
      let dec = Bytes.get_utf_8_uchar buffer 0 in
      if Uchar.utf_decode_is_valid dec then
        Uchar.utf_decode_uchar dec
      else invalid_utf_seq ()
    in
    let rec loop acc =
      match get_uchar input with
      | c -> let continue, acc = f acc c in
        if continue then loop acc else acc
      | exception End_of_file -> acc
    in loop acc
end
module Input =
struct
  let set_input = Solution.set_input
  let fold_lines ?input f = InputUntil.fold_lines ?input (fun a e -> true, f a e)
  let fold_scan ?input fmt f acc =
    fold_lines ?input (fun acc s -> Scanf.sscanf s fmt (f acc)) acc

  let fold_substrings ?input n f acc =
    InputUntil.fold_substrings ?input n (fun acc l -> true, f acc l) acc
  let fold_fields ?input c f = InputUntil.fold_fields ?input c (fun a e -> true, f a e)
  let fold_chars ?input f = InputUntil.fold_chars ?input (fun a e -> true, f a e)
  let fold_uchars ?input f = InputUntil.fold_uchars ?input (fun a e -> true, f a e)

  let read_line ?input () =
    let input = get_input input in
    input_line input

  let read_char ?input () =
    let input = get_input input in
    input_char input

  let list_lines ?input f = InputUntil.list_lines ?input (fun e -> Some (f e))
  let list_scan ?input fmt f = 
    list_lines ?input (fun s -> Scanf.sscanf s fmt f)

  let list_fields ?input c f = InputUntil.list_fields ?input c (fun l -> Some (f l))

end
module Ansi = Ansi

module Compare =
struct
  let fst x y = Stdlib.compare (fst x) (fst y)
  let snd x y = Stdlib.compare (snd x) (snd y)
  let agg_list agg f l =
    match l with
      [] -> failwith "min/max on empty list"
    | e :: l ->
      List.fold_left (fun acc e -> agg acc (f e)) (f e) l

  let min_list f l = agg_list min f l
  let max_list f l = agg_list max f l

  let rec lex l x y =
    match l with
      [] -> 0
    | [ o ] -> o x y
    | o1::o2::[] ->
      let c = o1 x y in
      if c <> 0 then c
      else o2 x y
    | o1::o2::o3::[] ->
      let c = o1 x y in
      if c <> 0 then c
      else let c = o2 x y in
        if c <> 0 then c
        else o3 x y
    | o :: ll ->
      let c = o x y in
      if c <> 0 then c
      else lex ll x y
end
module Comb =
struct
  let rec insert e l =
    match l with
      [] -> Seq.cons [e] Seq.empty
    | x :: ll ->
      Seq.cons (e::l) (Seq.map (fun ll -> x :: ll) (fun () -> insert e ll ()))


  let rec perm l =
    match l with
      [] -> Seq.cons [] Seq.empty
    | e :: ll ->
      Seq.flat_map (insert e) (perm ll)

  let rec powerset l =
    match l with
      [] -> Seq.cons [] Seq.empty
    | x :: ll ->
      let s = powerset ll in
      Seq.append s (Seq.map (fun l -> x :: l) s)


  let pairs ?(sym=true) ?(refl=true) l =
    let rec loop1 l1 acc =
      match l1 with
        [] -> Seq.empty
      | x1 :: ll1 ->
        let rec loop2 l2 =
          match l2 with
            [] -> Seq.empty
          | x2 :: ll2 ->
            Seq.cons (x1, x2) (loop2 ll2)
        in
        let s1 = match sym, refl with
            true, true -> loop2 l
          | true, false -> loop2 (List.rev_append acc ll1)
          | false, true -> loop2 l1
          | false, false -> loop2 ll1
        in
        Seq.append s1 (loop1 ll1 (x1::acc))
    in
    loop1 l []

  let product col1 col2 =
    Seq.product (List.to_seq col1) (List.to_seq col2)


  let choose k l =
    let rec loop k len l =
      if k = 0
      then Seq.cons [] Seq.empty
      else
      if len < k
      then Seq.empty
      else if k = len
      then Seq.cons l (Seq.empty)
      else
        match l with
          h :: t ->
          let starting_with_h =
            (Seq.map (fun sublist -> h :: sublist) (loop (pred k) (len - 1 ) t))
          in
          let not_starting_with_h = loop k (len - 1) t in
          Seq.append starting_with_h not_starting_with_h
        | [] -> assert false
    in
    loop k (List.length l) l

end

module type GRAPH = sig
  type v
  type t

  val iter_vertices : t -> (v -> unit) -> unit
  val iter_succ : t -> v -> (v * int -> unit) -> unit
end
module Pqueue (X : sig type t val compare : t -> t -> int end) =
struct
  type t = { mutable size : int; mutable data : X.t array }
  (* Taken (with minor modifications) from:
     https://usr.lmf.cnrs.fr/~jcf/ftp/ocaml/ds/binary_heap.ml.html
  *)
  let create n =
    if n <= 0 then invalid_arg "create";
    { size = -n; data = [||] }

  let is_empty h = h.size <= 0
  let length h = if h.size < 0 then 0 else h.size
  let resize h =
    let n = h.size in
    let n' = n lsl 1 in
    let d = h.data in
    let d' = Array.make n' d.(0) in
    Array.blit d 1 d' 1 (n-1);
    h.data <- d'
  let add h x =
    (* first addition: we allocate the array *)
    if h.size < 0 then begin
      h.data <- Array.make (- h.size) x; h.size <- 0
    end;
    let n = h.size in
    (* resizing if needed *)
    if n == Array.length h.data then resize h;
    let d = h.data in
    (* moving [x] up in the heap *)
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare d.(fi) x > 0 then begin
        d.(i) <- d.(fi);
        moveup fi
      end else
        d.(i) <- x
    in
    moveup n;
    h.size <- n + 1
  let minimum h =
    if h.size <= 0 then failwith "Pqueue.minimum";
    h.data.(0)
  let remove h =
    if h.size <= 0 then failwith "Pqueue.remove";
    let n = h.size - 1 in
    h.size <- n;
    let d = h.data in
    let x = d.(n) in
    (* moving [x] down in the heap *)
    let rec movedown i =
      let j = 2 * i + 1 in
      if j < n then
        let j =
          let j' = j + 1 in
          if j' < n &&  X.compare d.(j') d.(j) < 0 then j' else j
        in
        if X.compare d.(j) x  < 0 then begin
          d.(i) <- d.(j);
          movedown j
        end else
          d.(i) <- x
      else
        d.(i) <- x
    in
    movedown 0

  let remove_min h = let m = minimum h in remove h; m

end
module GraphAlgo (Graph : GRAPH) = struct
  open Syntax
  module X = struct
    type t = (int * Graph.v)
    let compare (i, _) (j, _) = Int.compare i j
  end


  module Pq = Pqueue(X)

  let add_dist d1 d2 =
    match d1, d2 with
      None, None -> None
    | Some _, None | None, Some _ -> None
    | Some d1, Some d2 -> Some (d1 + d2)

  let floyd_warshall g =
    let dist = ~%[] in
    let () =
      Graph.iter_vertices g (fun v ->
          dist.%{v, v} <- 0;
          Graph.iter_succ g v (fun (w, n) ->
              dist.%{v, w} <- n))
    in
    Graph.iter_vertices g (fun k ->
        Graph.iter_vertices g (fun i ->
            Graph.iter_vertices g (fun j ->
                let sum_sub_path = add_dist dist.%?{i, k} dist.%?{k,j} in
                match dist.%?{i, j}, sum_sub_path with
                  None, None -> ()
                | Some _, None -> ()
                | None, Some d -> dist.%{i, j} <- d
                | Some d1, Some d2 -> if d2 < d1 then dist.%{i, j} <- d2
              )
          )
      );
    dist
  let reverse_floyd_warshall g dist =
    let rdist = ~%[] in
    dist |>
    Hashtbl.iter
      (fun (i, j) d ->
         let dist_array = if d <= 1 then [||] else Array.make d None in
         rdist.%{i, j} <- dist_array;
         for d' = 1 to d - 1 do
           try
             Graph.iter_vertices g (fun k ->
                 if dist.%{i, k} == d' &&
                    dist.%{k, j} == d - d' &&
                    d >= 0 then begin
                   dist_array.(d') <- Some k; raise_notrace Exit
                 end)
           with Exit -> ()
         done);
    rdist

  let rebuild_path2 post t last =
    let rec loop acc_p v =
      match t.%{v} with
      | lv ->
        post v;
        let n_acc = List.fold_left
            (fun acc v -> List.fold_left (fun acc l -> (v::l)::acc) acc acc_p) [] lv
        in
        List.fold_left (fun acc v -> List.rev_append (loop n_acc v) acc) [] lv
      | exception Not_found -> post v; acc_p
    in
    loop [ [last] ] last

  let dijkstra ?(h=(fun (_:Graph.v) -> 0)) ?(post=(fun (_:Graph.v) -> ())) ?(first=false) ?(all_path=false) g start targets =
    let finish_map = ~%(List.map (fun v -> (v, (max_int, []))) targets) in
    (*  let todo = ref (Hashtbl.length finish_map) in *)
    let prev = ~%[] in
    let dist = ~%[] in
    let get_dist v = try dist.%{v} with Not_found -> max_int in
    let add_dist d1 d2 =
      let d = d1+d2 in
      if d < 0 then max_int else d
    in
    let queue = Pq.create 16 in
    let () =
      dist.%{start} <- 0;
      Pq.add queue (0, start);
    in

    let rec loop todo =
      if Pq.is_empty queue then finish_map
      else
        let _, u = Pq.remove_min queue in
        if finish_map %? u && prev %? u then begin
          let l = rebuild_path2 post prev u in
          finish_map.%{u} <- dist.%{u}, l;
          let todo = todo - 1 in
          if first || todo = 0 then finish_map
          else loop_aux todo u
        end
        else loop_aux todo u
    and loop_aux todo u =
      Graph.iter_succ g u (fun (v, d) ->
          let v_dist = get_dist v in
          let alt = add_dist (get_dist u) d in
          if all_path && alt = v_dist then begin
            let l = (prev.%?{v} or []) in
            prev.%{v} <- if List.mem u l then l else u::l ;
            dist.%{v} <- alt;
            Pq.add queue (alt + h v, v);
          end else if alt < v_dist then begin
            prev.%{v} <- [u];
            dist.%{v} <- alt;
            Pq.add queue (alt + h v, v);
          end);
      loop todo
    in
    loop (Hashtbl.length finish_map)


end

(* from 2021 day 22... *)
module Interval =
struct
  type t = { inf : int; sup : int } (* [inf, sup)*)
  let of_int i = { inf = i; sup = i + 1 }

  let length i = i.sup - i.inf
  let make_length a len =
    { inf = a; sup = a + len }
  let add t n = { inf = t.inf + n; sup = t.sup + n }
  let compare = compare
  let pp fmt t =
    let open Format in
    if t.inf = t.sup - 1 then
      fprintf fmt "%d" t.inf
    else
      fprintf fmt "[%d..%d)" t.inf t.sup
  let check t = if t.inf < t.sup then Some t else None
  let cap t1 t2 =
    check { inf = (max t1.inf t2.inf);
            sup = (min t1.sup t2.sup) }
  let cup t1 t2 =
    let t1, t2 = if t2.inf > t1.inf then t1, t2 else t2, t1 in
    if t1.sup >= t2.inf then
      [{t1 with sup = max t2.sup t1.sup}]
    else
      [t1; t2]

  let diff t1 t2 =
    match
      (check { t1 with sup = min t1.sup t2.inf }),
      (check { t1 with inf = max t1.inf t2.sup })
    with
      Some t, None | None, Some t -> [ t ]
    | None, None -> []
    | Some t1, Some t2 -> cup t1 t2

end

module Solution = Solution
module Grid = Grid

module Dll = struct
  type 'a t = { value : 'a; mutable next : 'a t; mutable prev : 'a t}

  let singleton value =
    let rec next = { value; next; prev = next } in next

  let next t = t.next
  let prev t = t.prev

  let rec loop_f n d =
    if n = 0 then d
    else loop_f (n-1) d.next
  let rec loop_b n d =
    if n = 0 then d
    else loop_b (n-1) d.prev

  let forward n t =
    if n < 0 then loop_b (-n) t else loop_f n t
  let backward n t =
    if n < 0 then loop_f (-n) t else loop_b n t

  let insert_before t value =
    let t1 = t.prev in
    let nd =  { value; next = t; prev = t1 } in
    t.prev <- nd;
    t1.next <- nd;
    nd

  let insert_after t v =
    let t2 = t.next in
    let nd = {value = v; next = t2; prev = t} in
    t.next <- nd;
    t2.prev <- nd;
    nd
  let pop t =
    if t.next == t then failwith "Dll.pop"
    else
      let v = t.value in
      let t2 = t.next in
      let t1 = t.prev in
      t.prev <- t; t.next <- t;
      (* make t point to itself in case someone holds a reference to it *)
      t1.next <- t2;
      t2.prev <- t1;
      v, t2

  let peek t = t.value
  let iter f t =
    let rec loop t' =
      if t' == t then ()
      else begin
        f t';
        loop t'.next;
      end
    in
    f t;
    loop t.next
end

module Misc =
struct
  let find_cycle equal f x0 =
    let lam = ref 1 in
    let power = ref 1 in

    let tortoise = ref x0 in
    let hare = ref (f x0) in
    while not (equal !tortoise !hare) do
      if !power == !lam then begin
        tortoise := !hare;
        power := !power * 2;
        lam := 0;
      end;
      hare := f !hare;
      incr lam;
    done;
    tortoise := x0;
    hare := x0;
    for _i = 0 to !lam - 1 do
      hare := f !hare;
    done;
    let mu = ref 0 in
    while not (equal !tortoise !hare) do
      tortoise := f !tortoise;
      hare := f !hare;
      incr mu;
    done;
    !lam, !mu, !tortoise
end

module Dynarray =
struct
  type 'a t = {
    mutable array : 'a array;
    mutable length : int;
  }
  let create n =
    let n = if n <= 0 then 16 else n in {
      array = [| |];
      length = -n;
    }

  let length t = max t.length 0

  let push a v =
    let () =
      if a.length < 0 then begin
        a.array <- Array.make (-a.length) v;
        a.length <- 0;
      end
    in
    let () =
      if a.length  + 1 > Array.length a.array then begin
        let array = Array.make (Array.length a.array * 2) a.array.(0) in
        Array.blit a.array 0 array 0 a.length;
        a.array <- array;
      end
    in
    a.array.(a.length) <- v;
    a.length <- a.length + 1

  let pop a =
    if length a = 0 then failwith "Dynarray.pop";
    let v = a.array.(a.length - 1) in
    a.length <- a.length - 1;
    if a.length * 4 < Array.length a.array then begin
      let array = Array.make (Array.length a.array / 2) a.array.(0) in
      Array.blit a.array 0 array 0 (Array.length array);
      a.array <- array;
    end;
    v

  let get a i = if i >= a.length || i < 0 then failwith "Dynarray.get";
    Array.unsafe_get a.array i

  let set a i v =
    if i >= length a || i < 0 then failwith "Dynarray.set";
    Array.unsafe_set a.array i v


  let iter f a =
    for i = 0 to a.length - 1 do
      f Array.(unsafe_get a.array i)
    done

  let fold_left f init a =
    let acc = ref init in
    for i = 0 to a.length - 1 do
      acc := f !acc Array.(unsafe_get a.array i);
    done;
    !acc

end

module Iter = Iter