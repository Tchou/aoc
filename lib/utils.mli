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

module Syntax :
sig
  val (<<) : 'a -> 'a * 'a -> bool
  val (<=<) : 'a -> 'a * 'a -> bool
  val (<<=) : 'a -> 'a * 'a -> bool
  val (<=<=) : 'a -> 'a * 'a -> bool

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
  (** [ ~% l] is creates a fresh Hashtbl.t from the bindings listed in [l]. *)

  val ( or ) : 'a option -> 'a -> 'a
  (** [ a or b ] is [v] if [a] is [Some v] and [b] if [a] is [None]. *)

  val ( let*|) : 'a option -> (unit -> 'a option) -> 'a option
  val ( let*&) : 'a option -> ('a -> 'b option) -> 'b option
  val ( let*%) : 'a option -> (unit -> 'a) -> 'a
end

(** Input related functions. *)
module Input :
sig
  val fold_lines : ?input:in_channel -> ('a -> string -> 'a) -> 'a -> 'a
  (** [fold_lines ~input f acc] folds over the lines of [input] if specified
      or [stdin] by default.
  *)

  val fold_substrings : ?input:in_channel -> int ->
    ('a -> string list -> 'a) -> 'a -> 'a
  (** [fold_substrings ~input n f acc] folds over the lines of [input] if specified
      or [stdin] by default. The line is split into a list [[s1; ...; sk ]] where
      the [si] have length [n] except for [sk] which may be shorter.
  *)

  val fold_scan : ?input:in_channel ->
    ('a, Scanf.Scanning.scanbuf, 'b, 'c -> 'd, 'a -> 'e, 'e) format6 ->
    ('d -> 'c) -> 'd -> 'd
  (** [fold_scan ~input fmt f acc] folds over the lines of [input] if specified
      or [stdin] by default. Each line is parsed according to [fmt] and the
      results is passed to [f].
  *)

  val fold_fields : ?input:in_channel -> char -> ('a -> string list -> 'a) -> 'a -> 'a
  (** [fold_fields c f acc] folds over the [c] separated lines of [input]
      if specified or [stdin] by default.
  *)

  val fold_chars : ?input:in_channel -> ('a -> char -> 'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the characters of [input] if
      specified or [stdin] by default.
  *)

  val fold_uchars : ?input:in_channel -> ('a -> Uchar.t -> 'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the unicode scalar values of [input], if
      specified or [stdin] by default. The input is assumed to bu UTF-8 encoded.
  *)
end
module InputUntil :
sig
  val fold_lines : ?input:in_channel -> ('a -> string -> bool*'a) -> 'a -> 'a
  (** [fold_lines ~input f acc] folds over the lines of [input] if specified
      or [stdin] by default. [f] must returns [true] to continue processing the
      input and [false] to stop.
  *)

  val fold_substrings : ?input:in_channel -> int ->
    ('a -> string list -> bool*'a) -> 'a -> 'a
  (** [fold_substrings ~input n f acc] folds over the lines of [input] if specified
      or [stdin] by default. The line is split into a list [[s1; ...; sk ]] where
      the [si] have length [n] except for [sk] which may be shorter.
  *)

  val fold_scan : ?input:in_channel ->
    ('a, Scanf.Scanning.scanbuf, 'b, 'c -> bool * 'd, 'a -> 'e, 'e) format6 ->
    ('d -> 'c) -> 'd -> 'd
  (** [fold_scan ~input fmt f acc] folds over the lines of [input] if specified
      or [stdin] by default. Each line is parsed according to [fmt] and the
      results is passed to [f]. [f] must returns [true] to continue processing the
      input and [false] to stop.
  *)

  val fold_fields : ?input:in_channel -> char -> ('a -> string list -> bool*'a) -> 'a -> 'a
  (** [fold_fields c f acc] folds over the [c] separated lines of [input] if
        specified or [stdin] by default. [f] must returns [true] to continue
        processing the input and [false] to stop.
  *)

  val fold_chars : ?input:in_channel -> ('a -> char -> bool*'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the characters of [input] if specified or
        [stdin] by default. [f] must returns [true] to continue processing the
        input and [false] to stop.
  *)

  val fold_uchars : ?input:in_channel -> ('a -> Uchar.t -> bool*'a) -> 'a -> 'a
  (** [fold_chars f acc] folds over the unicode scalar values of [input], if
      specified or [stdin] by default. The input is assumed to bu UTF-8 encoded.
      [f] must returns [true] to continue processing the input and [false] to stop.
  *)
end

(** Ainsi escape sequences, mainly for pretty-printing. *)
module Ansi : sig
  type color
  type dev

  val black : color
  val red : color
  val green : color
  val yellow : color
  val blue : color
  val magenta : color
  val cyan : color
  val white : color

  val line : dev
  val color : dev
  val screen : dev

  val bg : Format.formatter -> color -> unit
  val fg : Format.formatter -> color -> unit
  val bbg : Format.formatter -> color -> unit
  val bfg : Format.formatter -> color -> unit

  val clear : Format.formatter -> dev -> unit

  val printf : ('a, Format.formatter, unit) format -> 'a
  val eprintf : ('a, Format.formatter, unit) format -> 'a
  val sprintf : ('a, Format.formatter, unit, string) format4 -> 'a
  val fprintf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a

end

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
    type ('a, 'b) t
    val min : ('a, 'b) t
    val max : ('a, 'b) t
    val sum : (int, 'b) t
    val prod : (int, 'b) t
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

(** Math functions. *)
module Math : sig
  val gcd : int -> int -> int
  (** [gcd a b] computes the greatest common divisor of [a] and [b]. *)

  val lcm : int -> int -> int
  (** [lcm a b] computes the lowest common multiple of [a] and [b]. *)
end

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

module GraphAlgo (Graph : GRAPH) : sig

  val dijkstra : ?h:(Graph.v -> int) -> ?first:bool -> Graph.t ->
    Graph.v -> Graph.v list -> (Graph.v, int * Graph.v list) Hashtbl.t

  (** [dijsktra g src targets] computes the shortest path
      from [src] to all other vertices of [targets]. The result is given
      as a table [tbl] from vertices [v] to pairs [d, path] where
      [d] is the distance from [src] to [v] and [path] is the
      set sequence of vertices of length [d] to follow to
      reach [v] from [src].
  *)

  val floyd_warshall : Graph.t -> (Graph.v * Graph.v, int) Hashtbl.t
  (** [floyd_warshall g] returns the shorted distance between any
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