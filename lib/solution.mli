exception Error of string
module type S = sig
  val name : string * string
  val solve_part1 : unit -> unit
  val solve_part2 : unit -> unit
end

val register_mod : ?variant:string -> (module S) -> unit

val run : string array -> unit

val set_input : in_channel -> unit
val get_input : unit -> in_channel

val printf : ('a, Format.formatter, unit) format -> 'a
