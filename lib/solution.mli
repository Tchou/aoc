val get : string -> (unit -> unit) option
val list : unit -> string list

module type S = sig
  val name : string * string
  val solve_part1 : unit -> unit
  val solve_part2 : unit -> unit
end

val is_prefix : string -> bool
val register_prefix : string -> unit
val register_mod : ?variant:string -> (module S) -> unit

val run : string array -> unit