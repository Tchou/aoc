val int_of_bool : bool -> int
(** [int_of_bool b] is [1] if [b] is [true] and [0] if [b] is [false]*)

module String :
sig
  include module type of String
  val mem : char -> string -> bool

  val join : string -> string array -> string

  val explode : string -> char list
  val explode_array : string -> char array
  val implode : char list -> string
  val implode_array : char array -> string
  val compare_from : string -> int -> string -> int
  (** [compare_from s i p] is equivalent to
      [String.(compare (sub s i (min (length p) (length s - i)) p)]
      but does not allocate intermediary strings.
  *)
end
module Hashtbl :
sig
  include module type of Hashtbl
  module Make (H : HashedType) : sig
    include Hashtbl.S with type key = H.t
    val update : 'a t -> key -> ('a option -> 'a option) -> unit
  end
  val update : ('a,'b) t -> 'a -> ('b option -> 'b option) -> unit

end
module Syntax :
sig
  val (<<) : 'a -> 'a * 'a -> bool
  val (<=<) : 'a -> 'a * 'a -> bool
  val (<<=) : 'a -> 'a * 'a -> bool
  val (<=<=) : 'a -> 'a * 'a -> bool
  (** Interval comparison:
      - [x << (a, b)] is [x < a && x < b]
      - [x <=< (a, b)] is [x <= a && x < b]
      - [x <<= (a, b)] is [x < a && x <= b]
      - [x <=<= (a, b)] is [x <= a && x <= b]
  *)

  val ( .$[] ) : Bytes.t -> int -> char
  (** [b.$[i]] is [Bytes.get b i]. *)

  val ( .$[]<- ) : Bytes.t -> int -> char -> unit
  (** [b.$[i]<- c] is [Bytes.set b i c]. *)

  val ( .%{} ) : ('a, 'b) Hashtbl.t -> 'a -> 'b
  (** [h.%{k}] is [Hashtbl.find h k]. *)

  val ( .%?{} ) : ('a, 'b) Hashtbl.t -> 'a -> 'b option
  (** [h.%?{k}] is [Hashtbl.find_opt h k]. *)

  val ( .%{}<-) : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
  (** [h.%{k}<- v] is [Hashtbl.replace h k v]. *)

  val ( %? ) : ('a, 'b) Hashtbl.t -> 'a -> bool
  (** [h %? k] is [Hashtbl.mem h k]. *)

  val ( %- ) : ('a, 'b) Hashtbl.t -> 'a -> unit
  (** [h %- k] is [Hashtbl.remove h k]. *)

  val ( ~% ) : ('a* 'b) list -> ('a, 'b) Hashtbl.t
  (** [ ~% l] creates a fresh Hashtbl.t from the bindings listed in [l]. *)

  val ( or ) : 'a option -> 'a -> 'a
  (** [ a or b ] is [v] if [a] is [Some v] and [b] if [a] is [None]. *)

  val ( let*|) : 'a option -> (unit -> 'a option) -> 'a option
  val ( let*&) : 'a option -> ('a -> 'b option) -> 'b option
  val ( let*%) : 'a option -> (unit -> 'a) -> 'a
end

(** Input related functions. *)
module Input :
sig
  val set_input : in_channel -> unit
  (** [set_input ic] sets the default input channel for all the input reading functions.
      Defaults to [stdin]. *)

  val fold_lines : ?input:in_channel -> ('a -> string -> 'a) -> 'a -> 'a
  (** [fold_lines ~input f acc] folds over the lines of [input] if specified
      or to the default input channel (see [set_input])..
  *)

  val fold_substrings : ?input:in_channel -> int ->
    ('a -> string list -> 'a) -> 'a -> 'a
  (** [fold_substrings ~input n f acc] folds over the lines of [input] if specified
      or to the default input channel (see [set_input]). The line is split into a list [[s1; ..; sk ]] where
      the [si] have length [n] except for [sk] which may be shorter.
  *)

  val fold_scan : ?input:in_channel ->
    ('a, Scanf.Scanning.scanbuf, 'b, 'c -> 'd, 'a -> 'e, 'e) format6 ->
    ('d -> 'c) -> 'd -> 'd
  (** [fold_scan ~input fmt f acc] folds over the lines of [input] if specified
      or to the default input channel (see [set_input]). Each line is parsed according to [fmt] and the
      results is passed to [f].
  *)

  val fold_fields : ?input:in_channel -> char -> ('a -> string list -> 'a) -> 'a -> 'a
  (** [fold_fields c f acc] folds over the [c] separated lines of [input]
      if specified or to the default input channel (see [set_input]).
  *)

  val fold_chars : ?input:in_channel -> ('a -> char -> 'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the characters of [input] if
      specified or to the default input channel (see [set_input]).
  *)

  val fold_uchars : ?input:in_channel -> ('a -> Uchar.t -> 'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the unicode scalar values of [input], if
      specified or to the default input channel (see [set_input]). The input is assumed to be UTF-8 encoded.
  *)

  val read_line : ?input:in_channel -> unit -> string
  (** [read_line ()] reads one line of input from [input], if
      specified or to the default input channel (see [set_input]).
  *)

  val read_char : ?input:in_channel -> unit -> char
  (** [read_line ()] reads one char of input from [input], if
      specified or to the default input channel (see [set_input]).
  *)

end
module InputUntil :
sig
  val fold_lines : ?input:in_channel -> ('a -> string -> bool*'a) -> 'a -> 'a
  (** [fold_lines ~input f acc] folds over the lines of [input] if specified
      or to the default input channel (see [set_input]). [f] must returns [true] to continue processing the
      input and [false] to stop.
  *)

  val fold_substrings : ?input:in_channel -> int ->
    ('a -> string list -> bool*'a) -> 'a -> 'a
  (** [fold_substrings ~input n f acc] folds over the lines of [input] if specified
      or to the default input channel (see [set_input]). The line is split into a list [[s1; ..; sk ]] where
      the [si] have length [n] except for [sk] which may be shorter.
  *)

  val fold_scan : ?input:in_channel ->
    ('a, Scanf.Scanning.scanbuf, 'b, 'c -> bool * 'd, 'a -> 'e, 'e) format6 ->
    ('d -> 'c) -> 'd -> 'd
  (** [fold_scan ~input fmt f acc] folds over the lines of [input] if specified
      or to the default input channel (see [set_input]). Each line is parsed according to [fmt] and the
      results is passed to [f]. [f] must returns [true] to continue processing the
      input and [false] to stop.
  *)

  val fold_fields : ?input:in_channel -> char -> ('a -> string list -> bool*'a) -> 'a -> 'a
  (** [fold_fields c f acc] folds over the [c] separated lines of [input] if
        specified or to the default input channel (see [set_input]). [f] must returns [true] to continue
        processing the input and [false] to stop.
  *)

  val fold_chars : ?input:in_channel -> ('a -> char -> bool*'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the characters of [input] if specified or
        [stdin] by default. [f] must returns [true] to continue processing the
        input and [false] to stop.
  *)

  val fold_uchars : ?input:in_channel -> ('a -> Uchar.t -> bool*'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the unicode scalar values of [input], if
      specified or to the default input channel (see [set_input]). The input is assumed to bu UTF-8 encoded.
      [f] must returns [true] to continue processing the input and [false] to stop.
  *)
end

(** Ainsi escape sequences, mainly for pretty-printing. *)
module Ansi : module type of Ansi

(** Some comparison functions, built on top of Stdlib.compare. *)
module Compare :
sig
  val fst : 'a * 'b -> 'a * 'b -> int
  (* [fst x y] compares the first component of [x] and [y] and
     ignores the second. *)
  val snd : 'a * 'b -> 'a * 'b -> int
  (* [snd x y] compares the second component of [x] and [y] and
     ignores the first. *)

  val min_list : ('a -> 'b) -> 'a list -> 'b
  val max_list : ('a -> 'b) -> 'a list -> 'b
end
module Agg : sig
  module type T = sig
    type ('acc, 'elem) t
    val min : ('acc, 'elem) t
    val max : ('acc, 'elem) t
    val sum : (int, 'elem) t
    val prod : (int, 'elem) t
  end
  module Left : T with type ('a, 'b) t = ('b -> 'a) -> 'a -> 'b -> 'a
  module Right : T with type ('a, 'b) t = ('b -> 'a) -> 'b -> 'a -> 'a
end

(** Timing functions. *)
module Time : sig
  val time : ('a -> 'b) -> 'a -> ('b * float)
  (** [time f arg] computes [f args] and returns a pair
      of its result and its execution time, in miliseconds.
  *)
end

(** Combinatorics *)
module Comb : sig
  val perm : 'a list -> 'a list Seq.t
  (** [perm l] returns the sequence of all permutations of [l]. *)

  val powerset : 'a list -> 'a list Seq.t
  (** [powerset l] returns the sequence of all sublists of [l]. *)

  val pairs : ?sym:bool -> ?refl:bool -> 'a list -> ('a * 'a) Seq.t
  (** [pairs ~sym ~refl l] returns the Cartesian product of [l]
      with itself. Let [x] and [y] be elements of [l]:
      - if [sym] is [true] (default) then both [x,y] and [y,x] are in the result
      - if [refl] is [true] (default) then [x, x] is in the result
        Note that [x, x] may still occur if [x] appears several times in [l]
  *)

  val product : 'a list -> 'b list -> ('a * 'b) Seq.t
  (** [product l1 l2] return the sequences of pairs in the
      Cartesian product of [l1] and [l2]. *)

  val choose : int -> 'a list -> ('a list) Seq.t
  (** [choose n l] returns the sequences of all choices of n elements of l.*)
end

(** Math functions. *)
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
  (** [gcd a b] computes the greatest common divisor of [a] and [b]. *)

  val egcd : t  -> t  -> t  * t  * t
  (** [egcd a b] returns [(x, y , gcd(a, b))] where [x] and [y] are
      coefficients of Bézout's identity : ax+by = gcd(a, b)
  *)

  val lcm : t  -> t  -> t
  (** [lcm a b] computes the lowest common multiple of [a] and [b]. *)

  val solve_crt : (t  * t ) list -> t  * t
  (**
     [solve_crt [(a1, n1); ... ; (ak, nk)]] solves the system of equations:
     x ≡ a1  (mod n1)
     ...
     x ≡ ak  (mod nk)
     The resulst is a pair [(a, n)] such that x = [a] + k[n].
     @raise Failure if the input list is empty
     @raise Invalid_argument if some of the [ni] are not co-prime
  *)

  val pow : t -> int -> t
  (** [pow a b] returns the [b]th power of [a]. *)
end
module MathGen (N : Num) : MATH with type t = N.t
module Math : MATH with type t = int

(** Implementation of a graph structure, aka poorman's OCamlGraph. *)
module type GRAPH = sig
  type v
  (** type of vertices *)

  type t
  (** type of the graph *)

  val iter_vertices : t -> (v -> unit) -> unit
  (** [iter_vertices g f] calls [f] on all vertices of [g]. *)

  val iter_succ : t -> v -> (v * int -> unit) -> unit
  (** [iter_succ g v f] calls [f] on all successors of [v] together with their
      distance from [v].*)

end
module Pqueue(X : sig type t val compare : t -> t -> int end) : sig

  type t
  val create : int -> t
  val is_empty : t -> bool
  val length : t -> int
  val add : t -> X.t -> unit
  val minimum : t -> X.t
  val remove_min : t -> X.t
end

module GraphAlgo (Graph : GRAPH) : sig

  val dijkstra : ?h:(Graph.v -> int) -> ?first:bool -> ?all_path:bool -> Graph.t ->
    Graph.v -> Graph.v list -> (Graph.v, int * Graph.v list list) Hashtbl.t

  (** [dijsktra g src targets] computes the shortest path
      from [src] to all other vertices of [targets]. The result is given
      as a table [tbl] from vertices [v] to pairs [d, path_list ] where
      [d] is the distance from [src] to [v] and [path_list] is the
      list of sequence of vertices of cost [d] to follow to
      reach [v] from [src].
      If given, [h] is a heuristic function (as in the A* algorithm)
      If [first] is true (default to false) the algorithm stops at the
      first found target.
      If [all_path] is true, (default to false) the algorithm tracks all the
      equivalently good path to each exit (otherwise, [path_list] is a singleton)
  *)

  val floyd_warshall : Graph.t -> (Graph.v * Graph.v, int) Hashtbl.t
  (** [floyd_warshall g] returns the shortest distance between any
      two vertices of [g].
  *)

  val reverse_floyd_warshall : Graph.t ->
    (Graph.v * Graph.v, int) Hashtbl.t ->
    (Graph.v * Graph.v, Graph.v option array) Hashtbl.t
    (** [reverse_floyd_warshall g dtable] returns a table
        such that each pair [v,w] in [dtable] is associated
        to an array [t] which is:
        - the empty array if [v,w] are neighbours
        - an array [| o0; o1; ...; on |] where
          [oi] is [Some v'] if [v'] is a vertex at distance
          [i] on the path from [v] to [w] and [None] if there
            is no such vertex at this distance.
    *)
end

module Interval : sig
  type t = { inf : int; sup : int }

  val pp : Format.formatter -> t -> unit

  val of_int : int -> t
  val length : t -> int
  val make_length : int -> int -> t
  val add : t -> int -> t
  val compare : t -> t -> int

  val cap : t -> t -> t option
  val cup : t -> t -> t list
  val diff : t -> t -> t list

end
module Solution : module type of Solution

module Grid : module type of Grid