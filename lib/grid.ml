module type LINE = sig
  type elt
  type t
  val init : int -> (int -> elt) -> t

  val get : t -> int -> elt
  val length : t -> int
  val of_string : string -> t
  val map : (elt -> elt) -> t -> t

  val unsafe_get : t -> int -> elt

end

module type RWLINE = sig
  include LINE
  val set : t -> int -> elt -> unit
  val copy : t -> t
end

type dir = int * int
type position = int * int
let pp_dir fmt (i, j) = Format.fprintf fmt "<%d, %d>" i j
let pp_position fmt (x, y) = Format.fprintf fmt "(%d, %d)"

let north = (0, -1)
let south = (0, 1)
let east = (1,0)
let west = (-1, 0)
let dir4 = [ north; east; south; west ]

let (+!) (a, b) (c, d) = (a+c, b+d)

let north_east = north +! east
let north_west = north +! west
let south_east = south +! east
let south_west = south +! west

let dir8 = [ north; north_east; east; south_east; south; south_west; west; north_west ]

let left90 (i, j) = (j, -i)
let right90 (i, j) = (-j, i)

let opposite (i, j) = (-i, -j)


module type GRID = sig
  type elt
  type line
  type t
  val init : int -> (int -> line) -> t
  val width : t -> int
  val height : t -> int
  val inside : t -> position -> bool
  val iter : (position -> elt -> unit) -> t -> unit
  val iter4 : (position -> elt -> dir -> unit) -> t -> position -> unit
  val iter8 : (position -> elt -> dir -> unit) -> t -> position -> unit
  val iter_lines : (line -> unit) -> t -> unit
  val map_lines : (line -> line) -> t -> t

  val (.!()) : t -> position -> elt
  val (.!!()) : t -> position -> elt
  val read_until : ?input:in_channel -> (string -> bool) -> t
  val read : ?input:in_channel -> unit -> t
  val find_from : (elt -> bool) -> t -> position -> position
  val find : (elt -> bool) -> t -> position

end
module type RWGRID = sig
  include GRID
  val (.!()<-) : t -> position -> elt -> unit
  val copy : t -> t
end

let get_input = function Some ic -> ic | None -> Solution.get_input ()
let read_until ?input f cast =
  let input = get_input input in
  let l = ref [] in
  let s = ref (input_line input) in
  let () =
    try
      while not (f !s) do
        l := (cast !s) :: !l;
        s := input_line input
      done; ()
    with End_of_file -> ()
  in
  !l
  |> List.rev
  |> Array.of_list

let read ?input cast =
  read_until ?input (fun _ -> false) cast

module Make(L : LINE) = struct

  type elt = L.elt
  type line = L.t
  type t = L.t array

  let width t = L.length t.(0)
  let height t = Array.length t
  let (.!()) t (x, y) = L.get (Array.get t y) x

  let (.!!()) t (x, y) = L.unsafe_get (Array.unsafe_get t y) x

  let inside t (x, y) = y >= 0 && x >= 0 && y < height t && x < width t

  let iter f t =
    for y = 0 to height t - 1 do
      for x = 0 to width t - 1 do
        f (x, y) t.!(x, y)
      done
    done

  let iter_lines = Array.iter

  let map_lines = Array.map

  let apply t w h (p:position) f (d:dir) =
    let (x', y' as p') : position = p +! d in
    if x' >= 0 && x' < w  && y' >= 0 && y' < h then f p' t.!(x', y')  d

  let iter4 f t p =
    let h = height t in
    let w = width t in
    List.iter (apply t w h p f) dir4

  let iter8 f t p =
    let h = height t in
    let w = width t in
    List.iter (apply t w h p f) dir8

  let read_until ?(input) f =
    read_until ?input f L.of_string

  let read ?(input) () =
    read ?input L.of_string

  let find_from f t (x0, y0) =
    let h = height t in
    let w = width t in
    let exception Found of position in
    try
      for x = x0 to w - 1 do
        if f t.!(x, y0) then raise (Found (x, y0))
      done;
      for y = y0 + 1 to h - 1 do
        for x = 0 to w - 1 do
          if f t.!(x, y) then raise (Found (x, y))
        done;
      done;
      raise Not_found
    with Found p -> p

  let find f grid = find_from f grid (0, 0)

  let init h = Array.init h
end
module MakeRW (L : RWLINE) = struct
  include Make (L)
  let (.!()<-) t (x, y) v = L.set (Array.get t y) x v

  let copy = Array.map L.copy
end

module StringGrid : GRID with type elt = char and type line = string = Make(struct include String type elt = char let of_string x = x end)

module BytesGrid : RWGRID with type elt = char and type line = bytes = MakeRW(struct include Bytes type elt = char end)
