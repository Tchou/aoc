(** [Seq.t] based iterators.
    All functions in this module expect a conversion function
    to [Seq.t]
*)

val list : 'a list -> 'a Seq.t

val ilist : 'a list -> (int * 'a) Seq.t

val array : 'a array -> 'a Seq.t

val iarray : 'a array -> (int * 'a) Seq.t

val string : string -> char Seq.t
val istring : string -> (int * char) Seq.t

val bytes : bytes -> char Seq.t
val ibytes : bytes -> (int * char) Seq.t

val seq : 'a Seq.t -> 'a Seq.t

val iseq : 'a Seq.t -> (int * 'a) Seq.t

val keys : ('a, 'b) Hashtbl.t -> 'a Seq.t

val values : ('a, 'b) Hashtbl.t -> 'b Seq.t

val items : ('a, 'b) Hashtbl.t -> ('a * 'b) Seq.t

val count : ('a -> 'b Seq.t) -> 'a -> int
(** [count to_seq col] returns the number of elements in [col]. *)

val count_if : ('b -> 'a Seq.t) -> ('a -> bool)  -> 'b -> int
(** [count_if pred to_seq col] returns the number of elements in [col] for
    which [pred] returns true. *)

module type NUM = 
sig
  type t
  val zero : t 
  val one : t 
  val add : t -> t -> t 
  val mul : t -> t -> t
end

val int : (module NUM with type t = int)
val int64 : (module NUM with type t = int64)
val int32 : (module NUM with type t = int32)
val float : (module NUM with type t = float)

val sum :
  ('b -> 'a Seq.t) -> 
  (module NUM with type t = 'a) -> 'b -> 'a
(** [sum (module M) to_seq col] returns the sum of elements of [col].
    Their type should the same as [M.t] where [M] is the module featuring
    [zero] and [add].
*)

val prod :
  ('b -> 'a Seq.t) -> 
  (module NUM with type t = 'a) -> 'b -> 'a
(** [prod (module M) to_seq col] returns the product of elements of [col].
    Their type should the same as [M.t] where [M] is the module featuring
    [one] and [mul].
*)


val max_opt :
  ('b -> 'a Seq.t) ->
  ?compare:('a -> 'a -> int) -> 'b -> 'a option
(** [max_opt to_seq col] returns [Some m] where [m] is the maximal element of
    [col] according to the [compare] function. The function returns [None] if
    the sequence is empty. [compare] defaults to the polymorphic comparison.
*)

val min_opt :
  ('b -> 'a Seq.t) -> ?compare:('a -> 'a -> int) -> 'b -> 'a option
(** [min_opt to_seq col] returns [Some m] where [m] is the minimal element of
    [col] according to the [compare] function. The function returns [None] if
    the sequence is empty. [compare] defaults to the polymorphic comparison.
*)


val max :
  ('b -> 'a Seq.t) -> ?compare:('a -> 'a -> int) -> 'b -> 'a
(** [max to_seq col] returns the maximal elements of [col] according to the
      [compare] function. The function throws [Invalid_argument] if the sequence
      is empty. [compare] defaults to the polymorphic comparison.
*)

val min :
  ('b -> 'a Seq.t) -> ?compare:('a -> 'a -> int)  -> 'b -> 'a
(** [min to_seq col] returns [Some m] where [m] is the maximal elements of
    [col] according to the [compare] function. The function returns [None] if
    the sequence is empty. [compare] defaults to the polymorphic comparison.
*)

val range : ?step:int -> int -> int -> int Seq.t
(**
    [range start stop] returns the sequence of integers
    from [start] to [stop] (excluded). The optional
    [step] argument defaults to [1].
*)

val perm : ('a -> 'b Seq.t) -> 'a -> 'b Seq.t Seq.t
(** [perm to_seq col] returns the permutations of elements in [col]. *)

val powerset : ('a -> 'b Seq.t) -> 'a -> 'b Seq.t Seq.t
(** [poweret to_seq col] returns the powerset of the set of
    all elements of [col].
*)

val pairs :
  ('a -> 'b Seq.t) -> ?sym:bool -> ?refl:bool -> 'a -> ('b * 'b) Seq.t
(** [pairs ~sym ~refl to_seq col] returns the Cartesian product of [col]
    with itself. Let [x] and [y] be elements of [col]:
    - if [sym] is [true] (default) then both [x,y] and [y,x] are in the result
    - if [refl] is [true] (default) then [x, x] is in the result
      Note that [x, x] may still occur if [x] appears several times in [l]
*)

val product :
  ('a -> 'b Seq.t) -> ('c -> 'd Seq.t) -> 'a -> 'c -> ('b * 'd) Seq.t
(** [product to_seq1 to_seq2 col1 col2] return the sequences of pairs in the
    Cartesian product of [col1] and [col2]. *)

val fst : ('a -> ('b * 'c) Seq.t) -> 'a -> 'b Seq.t
val snd : ('a -> ('b * 'c) Seq.t) -> 'a -> 'c Seq.t
val for_ : ?step:int -> int -> int -> (int -> unit) -> unit
val while_ : (unit -> bool) -> (unit -> unit) -> unit
val break : unit -> 'a
val continue : unit -> 'a
type range
val (--) : int -> int -> range
val (-%) : range -> int -> range
val (let&) : range -> (int -> unit) -> unit