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
val end_of_screen : dev
val start_of_screen : dev
val cursor : dev

val bg : Format.formatter -> color -> unit
val fg : Format.formatter -> color -> unit
val bbg : Format.formatter -> color -> unit
val bfg : Format.formatter -> color -> unit

val clear : Format.formatter -> dev -> unit

val show_cursor : Format.formatter -> unit -> unit
val hide_cursor : Format.formatter -> unit -> unit
val move_cursor : Format.formatter -> (int* int) -> unit

val printf : ('a, Format.formatter, unit) format -> 'a
val eprintf : ('a, Format.formatter, unit) format -> 'a
val sprintf : ('a, Format.formatter, unit, string) format4 -> 'a
val fprintf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a

val set_for_tty : Format.formatter -> unit
val unset_for_tty : Format.formatter -> unit
