let printf = Format.printf
let sprintf = Format.asprintf
let eprintf = Format.eprintf
let fprintf = Format.fprintf

module FTable = Hashtbl.Make(struct type t = Format.formatter let hash = Hashtbl.hash let equal = (==) end)
let for_tty_table = FTable.create 16
let set_for_tty fmt = FTable.replace for_tty_table fmt ()
let unset_for_tty fmt = FTable.remove for_tty_table fmt
let is_a_tty fmt =
  let open Format in
  if FTable.mem for_tty_table fmt then true
  else
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
let cursor = "1;1H"
let screen  = "2J"
let end_of_screen = "0J"
let start_of_screen = "1J"
let line = "2K"
let color = "0m"
let show_cursor fmt () = Format.fprintf fmt "\x1b[?25h"
let hide_cursor fmt () = Format.fprintf fmt "\x1b[?25l"
let move_cursor fmt (i, j) = Format.fprintf fmt "\x1b[%d;%dH" i j
let clear fmt s = (*if is_a_tty fmt then*) Format.fprintf fmt "\x1b[%s" s
let pr fmt d = (*if is_a_tty fmt then *)Format.fprintf fmt "\x1b[%dm" d
let fg fmt d = pr fmt d
let bg fmt d = pr fmt (d + 10)
let bfg fmt d = pr fmt (d + 60)
let bbg fmt d = pr fmt (d + 70)

