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

  let explode s = Array.to_list (explode_array s)
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

  let rec fix a n =
    if N.(compare n a <= 0) then N.rem a n
    else if N.(compare a (neg n) < 0) then fix N.(rem a n) n
    else if N.(compare a zero < 0) then fix N.(add a  n) n
    else a
  let rec solve_crt l =
    match l with
      [] -> failwith "solve_crt"
    | [ (a, n) ] ->  fix a n, n
    | (a1, n1) :: (a2, n2) :: ll ->
      let m1, m2 , x = egcd n1 n2 in
      if not N.(compare x one = 0) then begin
        let s1 = N.to_string n1 in
        let s2 = N.to_string n2 in
        raise (Invalid_argument Printf.(sprintf
                                          "coefficient %s and %s are not coprime (gcd(%s, %s)=%s)"
                                          s1 s2 s1 s2 (N.to_string x)))
      end;
      let n12 = N.mul n1  n2 in
      let k1 = N.(mul a1 (mul m2 n2)) in
      let k2 = N.(mul a2 (mul m1 n1)) in
      let a12 = N.add k1 k2 in
      solve_crt ((a12, n12) :: ll)
end

module Math = MathGen(Int)
module Time = struct

  let time f arg =
    let t0 = Unix.gettimeofday () in
    let res = f arg in
    let t1 = Unix.gettimeofday () in
    res, 1000. *. (t1 -. t0)
end

module InputUntil = struct

  let rec fold_lines ?(input=stdin) f acc =
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

  let fold_substrings ?(input=stdin) n f acc =
    if n <= 0 then invalid_arg "fold_substrings";
    fold_lines ~input (fun acc s ->
        f acc (split_string s n)
      ) acc

  let fold_scan ?(input=stdin) fmt f acc =
    fold_lines ~input (fun acc s ->
        Scanf.sscanf s fmt (f acc)) acc

  let fold_fields ?(input=stdin) sep f acc =
    fold_lines ~input (fun acc l -> f acc (String.split_on_char sep l)) acc

  let rec fold_chars ?(input=stdin) f acc =
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

  let fold_uchars ?(input=stdin) f acc =
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
  let fold_lines ?(input=stdin) f = InputUntil.fold_lines ~input (fun a e -> true, f a e)
  let fold_scan ?(input=stdin) fmt f acc =
    fold_lines ~input (fun acc s -> Scanf.sscanf s fmt (f acc)) acc

  let fold_substrings ?(input=stdin) n f acc =
    InputUntil.fold_substrings ~input n (fun acc l -> true, f acc l) acc
  let fold_fields ?(input=stdin) c f = InputUntil.fold_fields ~input c (fun a e -> true, f a e)
  let fold_chars ?(input=stdin) f = InputUntil.fold_chars ~input (fun a e -> true, f a e)
  let fold_uchars ?(input=stdin) f = InputUntil.fold_uchars ~input (fun a e -> true, f a e)

end
module Ansi = struct
  let printf = Format.printf
  let sprintf = Format.asprintf
  let eprintf = Format.eprintf
  let fprintf = Format.fprintf

  let is_a_tty fmt =
    let open Format in
    let ofmt = if fmt == std_formatter then Some Unix.stdout
      else if fmt == err_formatter then Some Unix.stderr
      else None
    in match ofmt with
      None -> false
    | Some fd -> Unix.isatty fd

  type color = int
  type dev = string
  let black = 30
  let red = 31
  let green = 32
  let yellow = 33
  let blue = 34
  let magenta = 35
  let cyan = 36
  let white = 37

  let screen = "2J"
  let line = "2K"
  let color = "0m"
  let clear fmt s = if is_a_tty fmt then Format.fprintf fmt "\x1b[%s" s
  let pr fmt d = if is_a_tty fmt then Format.fprintf fmt "\x1b[%dm" d
  let fg fmt d = pr fmt d
  let bg fmt d = pr fmt (d + 10)
  let bfg fmt d = pr fmt (d + 60)
  let bbg fmt d = pr fmt (d + 70)


end
module Agg = struct
  module type T = sig
    type ('a, 'b) t
    val min : ('a, 'b) t
    val max : ('a, 'b) t
    val sum : (int, 'b) t
    val prod : (int, 'b) t
  end
  module Left =
  struct
    type ('a, 'b) t = ('b -> 'a) -> 'a -> 'b -> 'a
    let agg op f acc x = op acc (f x)
    let min acc = agg min acc
    let max acc = agg max acc
    let sum acc = agg (+) acc
    let prod acc = agg ( * ) acc
  end
  module Right =
  struct
    type ('a, 'b) t = ('b -> 'a) -> 'b -> 'a -> 'a
    let agg op f x acc = op (f x) acc
    let min f = agg min f
    let max f = agg max f
    let sum f = agg (+) f
    let prod f = agg ( * ) f
  end
end

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
end

module type GRAPH = sig
  type v
  type t

  val iter_vertices : t -> (v -> unit) -> unit
  val iter_succ : t -> v -> (v * int -> unit) -> unit
end

module GraphAlgo (Graph : GRAPH) = struct
  open Syntax

  module Pqueue = struct
    include Set.Make (struct
        type t = int * Graph.v

        let compare (p1, v1) (p2, v2) =
          if p1 == p2 then compare v1 v2
          else if p1 < p2 then -1 else 1
      end)
    let remove_min s =
      let e = min_elt s in
      e, remove e s
  end

  let path_length dist t last =
    let rec loop acc_c acc_p v =
      match t.%{v} with
      | v2 -> loop (dist.%{v2,v} + acc_c) (v2 :: acc_p) v2
      | exception Not_found -> acc_c, acc_p
    in
    loop 0 [ last ] last

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

  let dijkstra ?(h=(fun (_:Graph.v) -> 0)) ?(first=false) g start targets =
    let finish_map = ~%(List.map (fun v -> (v, (max_int, []))) targets) in
    (*  let todo = ref (Hashtbl.length finish_map) in *)
    let prev = ~%[] in
    let dist = ~%[] in
    let global_dist = ~%[] in
    let get_dist v = dist.%?{v} or max_int in
    let add_dist d1 d2 = if d1 == max_int || d2 == max_int then max_int else d1+d2 in
    let queue = ref Pqueue.empty in
    let () =
      dist.%{start} <- 0
    in
    let rec loop todo queue =
      if Pqueue.is_empty queue then finish_map
      else
        let (_, u), nqueue  = Pqueue.remove_min queue in
        if finish_map %? u && prev %? u then begin
          let l = path_length global_dist prev u in
          finish_map.%{u} <- l;
          let todo = todo - 1 in
          if todo = 0 || first then finish_map
          else loop_aux todo nqueue u
        end
        else loop_aux todo nqueue u
    and loop_aux todo queue u =
      let q = ref queue in
      Graph.iter_succ g u (fun (v, d) ->
          global_dist.%{u,v} <- d;
          let v_dist = get_dist v in
          let alt = add_dist (get_dist u) d in
          if alt < v_dist then begin
            prev.%{v} <- u;
            dist.%{v} <- alt;
            q := Pqueue.add (alt + h v, v) !q
          end);
      loop todo !q
    in
    loop (Hashtbl.length finish_map) (Pqueue.singleton (0, start))
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
