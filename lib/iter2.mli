type 'a t
(** The type of iterators. An iterator can be persistent, meaning
    it can be traversed several times and will always return the same
    results at the same position, or ephemeral in which case it can
    only be traversed once.
*)

val is_persistent : 'a t -> bool
(** return whether the iterator is persistent. *)

exception EphemeralReused
(** Raised when using an ephemeral iterator more than once. *)

val break : unit -> 'a
(** [break ()] interrupts the iterator.
    For instance
    [(it |> map (fun x -> if x = 0 then break () else x+1 ))]
    return an iterator where every integer occuring before the first
    0 in the original itertor is incremented.
*)

val continue : unit -> 'a
(** [continue ()] interrupts the iterator.
    For instance
    [(it |> map (fun x -> if x mod 2 = 0 then continue () else x))]
    return an iterator where every even integer is skipped
*)

val enum : 'a t -> (int * 'a) t
(** [enum it] return an iterator over the elements of [it] with
    their 0-based position.
*)

val list : 'a list -> 'a t
(** [list l] return a persistent iterator over the elements of [l]. *)

val ilist : 'a list -> (int * 'a) t
(** Same as [list] but with the position. *)

val array : 'a array -> 'a t
(** [array a] return a ephemeral iterator over the elements of [a]. *)

val iarray : 'a array -> (int * 'a) t
(** Same as [array] but with the position. *)

val string : string -> char t
(** [string s] return a persistent iterator over the characters of [s]. *)

val istring : string -> (int * char) t
(** Same as [string] but with the position. *)

val bytes : bytes -> char t
(** [bytes s] return a ephemeral iterator over the characters of [s]. *)

val ibytes : bytes -> (int * char) t
(** Same as [bytes] but with the position. *)

val seq : 'a Seq.t -> 'a t
(** [seq s] return an ephemeral iterator over the elements of [s]. *)

val iseq : 'a Seq.t -> (int * 'a) t
(** Same as [seq] but with the position. *)

val items : ('a, 'b) Hashtbl.t -> ('a * 'b) t
(** [items h] return a ephemeral iterator over the bindings of [h]. *)

val keys : ('a, 'b) Hashtbl.t -> 'a t
(** [keys h] return a ephemeral iterator over the keys of [h]. *)

val values : ('a, 'b) Hashtbl.t -> 'b t
(** [values h] return a ephemeral iterator over the values of [h]. *)

val option : 'a option -> 'a t
(** [option o] return a persistent iterator over the element of [o]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f it] return an iterator over the image of the elements of [it].
*)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f it] return an iterator of the elements of [it] which for which [f] holds.
*)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f it] return an iterator of the elements of [it] which for which
    [f] returns [Some e].
*)

val count : 'a t -> int
(** [count it] return the number of elements in the iterator, traversing
    it.
*)

val count_if : ('a -> bool) -> 'a t -> int
(** [count_if p it] return the number of elements in the iterator for which [p]
    holds, traversing it.
*)

(** A minimal signature for numbers *)
module type NUM =
sig
  type t
  (** The type of numbers *)

  val zero : t
  (** Zero. *)

  val one : t
  (** One. *)

  val minus_one : t
  (** Minus One*)

  val add : t -> t -> t
  (** Addition. *)

  val mul : t -> t -> t
  (** Multiplication. *)

  val compare : t -> t -> int
  (** Comparison. *)
end

val int : (module NUM with type t = int)
(** Numbers with [int] representation. *)

val int64 : (module NUM with type t = int64)
(** Numbers with [int64] representation. *)

val int32 : (module NUM with type t = int32)
(** Numbers with [int32] representation. *)

val float : (module NUM with type t = float)
(** Numbers with [float] representation. Zero is [+0.0]. *)

val zarith : (module NUM with type t = Z.t)
(** Numbers with {!Z.t} representation. *)

val sum : (module NUM with type t = 'a) -> 'a t -> 'a
(** Sum the elements of the iterator, traversing it. *)

val prod : (module NUM with type t = 'a) -> 'a t -> 'a
(** Multiply the elements of the iterator, traversing it.*)

val max_opt : ?compare:('a -> 'a -> int) -> 'a t -> 'a option
(** Largest element of the iterator. return [None] if the iterator is empty.
    The comparison defaults to generic [compare].
    If several elements compare equal to the largest, 
    the first in the sequence is returned.
    Traverses the iterator.
*)

val min_opt : ?compare:('a -> 'a -> int) -> 'a t -> 'a option
(** Smallest element of the iterator. return [None] if the iterator is empty.
    The comparison defaults to generic [compare]. 
    If several elements compare equal to the smallest, 
    the first in the sequence is returned.
    Traverses the iterator.
*)

val last_max_opt : ?compare:('a -> 'a -> int) -> 'a t -> 'a option
(** Like [max_opt], but return the last element in case of equality. *)

val last_min_opt : ?compare:('a -> 'a -> int) -> 'a t -> 'a option
(** Like [min_opt], but return the last element in case of equality. *)

val max_ : ?compare:('a -> 'a -> int) -> 'a t -> 'a
(** Like [max_opt], but raises [Failure] if the iterator is empty. *)

val min_ : ?compare:('a -> 'a -> int) -> 'a t -> 'a
(** Like [min_opt], but raises [Failure] if the iterator is empty. *)

val last_max : ?compare:('a -> 'a -> int) -> 'a t -> 'a
(** Like [last_max_opt], but raises [Failure] if the iterator is empty. *)

val last_min : ?compare:('a -> 'a -> int) -> 'a t -> 'a
(** Like [last_min_opt], but raises [Failure] if the iterator is empty. *)

val range :
  (module NUM with type t = 'a) -> ?start:'a -> ?step:'a -> 'a -> 'a t
(** [range (module M) ~start ~step stop] return the persistent iterator of every
      [step]th number (default to [M.one]) between [start] (default to [M.zero])
      and [stop] (excluded).
*)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f it] apply [f] to every element of [it], traversing it. *)

val iteri : (int * 'a -> unit) -> 'a t -> unit
(** Like [iter] but with the position. *)

val first : 'a t -> 'a
(** Return the first element of the sequence, traversing int. *)

val take : int -> 'a t -> 'a t
(** [take n it] return the iterator of the first [n] elements of [it], or (an
    iterator over all the elements of [it] if it has less than [n] elements).
*)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Folds over the element of the iterator, traversing it. *)

val to_list_rev : 'a t -> 'a list
(** Converts to a list in reverse order, traversing it. *)

val to_list : 'a t -> 'a list
(** [to_list it] is [List.rev (to_list_rev it)]. *)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq it] is [List.to_seq (to_list it)]. *)

val to_array : 'a t -> 'a array
(** [to_array it] is [Array.of_list (to_list it)]. *)

val to_string : char t -> string
(** Converts the iterator of characters into a string. *)

val to_bytes : char t -> bytes
(** Converts the iterator of characters into a bytes. *)

val perm : 'a t -> 'a list t
(** [perm it] return the persistent iterator over the permutations of the elements of
    [it]. Each permutation is given as a list.
*)

val powerset : 'a t -> 'a list t
(** [powerset it] return the persistent iterator over the subsets of the elements of
    [it]. Each subset is given as a list.
*)

val product : 'a t -> 'b t -> ('a * 'b) t
(** [product it1 it2] return a persistent iterator over the Cartesian products of the elments
    of [it1] and [it2].
*)

val pairs : ?sym:bool -> ?refl:bool -> 'a t -> ('a * 'a) t
(** [pairs ~sym ~refl it] return the persistent iterator 
    of pairs of elements of [it].
    If [refl] is true, all pairs [(x,y)] and [(y,x)] are returned,
    otherwise only one such pair is returned.
    If [sym] is true, then the pair [(x,x)] is returned.
*)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists f it] return [true] if there exists an element
    in [it] for which [f] holds, traversing it.
*)

val forall :('a -> bool) -> 'a t -> bool
(** [forall f it] return [true] if [f] holds for all the elements
    of [it], traversing it.
*)

val sort : ?compare:('a -> 'a -> int) -> 'a t -> 'a t
(** [sort ~compare it] return a persistent iterator over the elements of [it].
    [compare] defaults to the the polymorphic [compare].*)

val find : ('a -> bool) -> 'a t -> 'a
(** [find f it] return the first element in [it] for which 
    [f] holds. 
    @raise Not_found if there is no such element.*)

val find_opt : ('a -> bool) -> 'a t -> 'a option
(** [find_opt f it] return the first element in [it] for which 
    [f] holds.
*)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f it] return the first element in [it] for which 
    [f] returns [Some e].
*)

val (let->) : 'a t -> ('a -> unit) -> unit
(** Le binding to write for-loop style expressions.

*)

val persist : 'a t -> 'a t
(** If the argument is persistent, this function is the identity.
    Otherwise, it creates a persistent version of the iterator, backed
    by a list.
*)

val (++) : 'a t -> 'a t -> 'a t
(** Concatenate both iterators. 
    Concatenating the same ephemeral iterator will always fail.
*)

val cons : 'a  -> 'a t -> 'a t
(** [cons x it] prepend [x] at the begining of [it]. *)

val snoc :  'a t -> 'a  -> 'a t
(** [snoc it x] append [x] at the end of [it]. *)

val cycle : 'a t -> 'a t
(** [cycle it] repeats the elements of [it] indefinitely. *)

val empty : 'a t
(** The persistent empty iterator. *)