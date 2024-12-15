(** Simple module to handle Grid like puzzles *)

(** Module type of read-only lines of a grid *)
module type LINE =
sig
  type elt
  (** individual elements of a line *)

  type t
  (** A line *)

  val get : t -> int -> elt
  (** Returns the ith element of a line *)

  val length : t -> int
  (** Returns the length of the line *)

  val of_string : string -> t
  (** Convert an (input) string into a line *)

  val map : (elt -> elt) -> t -> t

end

(** Types of read-write lines of a grid *)
module type RWLINE =
sig
  include LINE
  val set : t -> int -> elt -> unit
  (** Updates the element at the given index *)

  val copy : t -> t
  (** Returns a copy of the line *)
end

type dir = int * int
(** Directions on a grid *)

type position = int * int
(** A position on a grid *)

val north : int * int
val south : int * int
val east : int * int
val west : int * int
(** Four axis aligned directions *)

val dir4 : (int * int) list
(** A list of the four directions from north clockwise to west *)

val ( +! ) : position -> dir -> position
(** Moves a position according to a given direction *)

val north_east : int * int
val north_west : int * int
val south_east : int * int
val south_west : int * int
(** Four diagonal directions *)

val dir8 : (int * int) list
(** All directions from north clockwise to north west *)

val left90 : dir -> dir
(** Rotates a direction left by 90 degrees *)

val right90 : 'a * int -> int * 'a
(** Rotates a direction right by 90 degrees *)

val opposite : int * int -> int * int
(** Rotates a direction by 180 degrees *)

(** The type of grids *)
module type GRID =
sig
  type elt
  (** Type of elements *)

  type line
  (** type of lines *)

  type t
  (** Grid *)

  val width : t -> int
  val height : t -> int
  (** Returns the width / height of a grid *)

  val inside : t -> position -> bool
  (** Tests whether a position is inside the grid *)

  val iter : (position -> elt -> unit) -> t -> unit
  (** Iterates through all the positions in the grid from 0,0 to (width - 1, height - 1) *)

  val iter4 : (position -> elt -> dir -> unit) -> t -> position -> unit
  (** Iterates through all the valid neighbours in the 4 axis aligned directions *)

  val iter8 : (position -> elt -> dir -> unit) -> t -> position -> unit
  (** Iterates through all the valid neighbours in the 8 directions *)

  val iter_lines : (line -> unit) -> t -> unit
  (** Iterates through all the lines *)

  val map_lines : (line -> line) -> t -> t
  (** Returns a new grid by transforming each line. *)

  val ( .!() ) : t -> position -> elt
  (** Indexig operator get *)

  val read_until : ?input:in_channel -> (string -> bool) -> t
  (** Reads an input line-by line. The line is first given to the test function until it returns true. *)

  val read : ?input:in_channel -> unit -> t
  (** Reads an input until the end. *)

  val find_from : (elt -> bool) -> t -> position -> position
  (** Find the position for which the predicate function returns true,
      traversing the grid north to south, west to east.
      Raises Not_found if no such position is found *)

  val find : (elt -> bool) -> t -> position
   (* [find f grid] is equivalent to [find_from f grid (0, 0)]*)
end

(** The type of read-write grids *)
module type RWGRID =
sig
  include GRID
  val ( .!()<- ) : t -> position -> elt -> unit
  (** Indexing operator set *)

  val copy : t ->t
  (** Copy a grid *)
end

module Make (L : LINE) : GRID with type elt = L.elt and type line = L.t
(** Functors that creates a read-only grid of a LINE *)

module MakeRW (L : RWLINE) : RWGRID with type elt = L.elt and type line = L.t
(** Functors that creates a read-only grid of a RWLINE *)

module StringGrid : GRID with type elt = char and type line = string
(** Read-only grids of charcters *)

module BytesGrid : RWGRID with type elt = char and type line = bytes
(** Read-write grids of characters *)