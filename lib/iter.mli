(** [Seq.t] based iterators.
    All functions in this module expect a conversion function
    to [Seq.t]
*)

val count : ('a -> 'b Seq.t) -> 'a -> int
(** [count to_seq col] returns the number of elements in [col]. *)

val count_if : ('a -> bool) -> ('b -> 'a Seq.t) -> 'b -> int
(** [count_if pred to_seq col] returns the number of elements in [col] for
    which [pred] returns true. *)

module type ADD = sig type t val zero : t val add : t -> t -> t end
(** A minimal module signatures that features [zero] and [add].
    Modules of the Standard library such as [Int], [Float], [Int64] implement
    this signature. *)

module type MUL = sig type t val one : t val mul : t -> t -> t end
(** A minimal module signatures that features [one] and [mul].
    Modules of the Standard library such as [Int], [Float], [Int64] implement
    this signature. *)

val sum :
  (module ADD with type t = 'a) -> ('b -> 'a Seq.t) -> 'b -> 'a
(** [sum (module M) to_seq col] returns the sum of elements of [col].
    Their type should the same as [M.t] where [M] is the module featuring
    [zero] and [add].
*)

val prod : (module MUL with type t = 'a) -> ('b -> 'a Seq.t) -> 'b -> 'a
(** [prod (module M) to_seq col] returns the product of elements of [col].
    Their type should the same as [M.t] where [M] is the module featuring
    [one] and [mul].
*)


val max_opt :
  ?compare:('a -> 'a -> int) -> ('b -> 'a Seq.t) -> 'b -> 'a option
(** [max_opt to_seq col] returns [Some m] where [m] is the maximal element of
    [col] according to the [compare] function. The function returns [None] if
    the sequence is empty. [compare] defaults to the polymorphic comparison.
*)

val min_opt :
  ?compare:('a -> 'a -> int) -> ('b -> 'a Seq.t) -> 'b -> 'a option
(** [min_opt to_seq col] returns [Some m] where [m] is the minimal element of
    [col] according to the [compare] function. The function returns [None] if
    the sequence is empty. [compare] defaults to the polymorphic comparison.
*)


val max : ?compare:('a -> 'a -> int) -> ('b -> 'a Seq.t) -> 'b -> 'a
(** [max to_seq col] returns the maximal elements of [col] according to the
      [compare] function. The function throws [Invalid_argument] if the sequence
      is empty. [compare] defaults to the polymorphic comparison.
*)

val min : ?compare:('a -> 'a -> int) -> ('b -> 'a Seq.t) -> 'b -> 'a
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
  ?sym:bool -> ?refl:bool -> ('a -> 'b Seq.t) -> 'a -> ('b * 'b) Seq.t
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