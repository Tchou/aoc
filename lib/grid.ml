module type LINE = sig
  type elt
  type t
  val init : int -> (int -> elt) -> t

  val get : t -> int -> elt
  val length : t -> int
  val of_string : string -> t
  val map : (elt -> elt) -> t -> t
  val unsafe_get : t -> int -> elt

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val of_array : elt array -> t

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
  val iter_from : (position -> elt -> unit) -> t -> position -> int -> int -> unit
  val iter4 : (position -> elt -> dir -> unit) -> t -> position -> unit
  val iter8 : (position -> elt -> dir -> unit) -> t -> position -> unit
  val iter_lines : (line -> unit) -> t -> unit
  val map_lines : (line -> line) -> t -> t

  val (.!()) : t -> position -> elt
  val get_line : t -> int -> line
  val (.!!()) : t -> position -> elt
  val read_until : ?input:in_channel -> (string -> bool) -> t
  val read : ?input:in_channel -> unit -> t
  val find_from : (elt -> bool) -> t -> position -> position
  val find : (elt -> bool) -> t -> position

  val rotate_left : t -> t
  val rotate_right : t -> t

  val vertical_flip : t -> t
  val horizontal_flip : t -> t

  val compare : t -> t -> int
  val equal : t -> t -> bool

  val of_array : line array -> t
  val of_matrix : elt array array -> t
  val of_string : string -> t

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

  let of_array la = Array.copy la

  let of_matrix ea =
    Array.map L.of_array ea
  let width t = L.length t.(0)
  let height t = Array.length t
  let (.!()) t (x, y) = L.get (Array.get t y) x

  let get_line t i = t.(i)

  let (.!!()) t (x, y) = L.unsafe_get (Array.unsafe_get t y) x

  let inside t (x, y) = y >= 0 && x >= 0 && y < height t && x < width t


  let iter_from f t (x0, y0) w h =
    if x0 < 0 || y0 < 0 || w > (width t) || h > (height t) then
      invalid_arg "Grid.iter_from";
    for y = y0 to h - 1 do
      let l = Array.unsafe_get t y in
      for x = x0 to w - 1 do
        f (x, y) (L.unsafe_get l x)
      done
    done
  let iter f t = iter_from f t (0, 0) (width t) (height t)

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

  let compare g1 g2 =
    let rec loop i j n1 n2 =
      if n1 = i && n2 = j then 0
      else if n1 = i then -1 else if n2 = j then 1
      else
        let c = Array.(L.compare (unsafe_get g1 i) (unsafe_get g2 j)) in
        if c <> 0 then c else loop (i+1) (j+1) n1 n2
    in
    if g1 == g2 then 0 else
      loop 0 0 (height g1) (height g2)

  let equal g1 g2 = compare g1 g2 = 0


  let rotate_left g =
    let rows = height g in
    let cols = width g in
    Array.init cols (fun i -> L.init rows (fun j -> 
        L.get g.(j) (cols - 1 - i)))
  let rotate_right g =
    let rows = height g in
    let cols = width g in
    Array.init cols (fun i -> L.init rows (fun j -> 
        L.get g.(rows - 1 - j) i))

  let vertical_flip g =
    let rows = height g in
    let cols = width g in
    Array.init rows (fun i -> L.init cols (fun j -> 
        L.get g.(rows - 1 - i) j))

  let horizontal_flip g =
    let rows = height g in
    let cols = width g in
    Array.init rows (fun i -> L.init cols (fun j -> 
        L.get g.(i) (cols - 1 - j)))

   let of_string s =
    let l = String.split_on_char '\n' s in
    let a = Array.of_list l in
    Array.map L.of_string a
end
module MakeRW (L : RWLINE) = struct
  include Make (L)
  let (.!()<-) t (x, y) v = L.set (Array.get t y) x v

  let copy = Array.map L.copy
  let of_array la = copy la
end

module StringGrid : GRID with type elt = char and type line = string = 
  Make(struct 
    include String
    type elt = char
    let of_string x = x 
    let of_array ca = String.init (Array.length ca) (Array.get ca)
  end)

module BytesGrid : RWGRID with type elt = char and type line = bytes = MakeRW(struct
 include Bytes 
 type elt = char
 let of_array ca = Bytes.init (Array.length ca) (Array.get ca) 
end)

module IntLine : RWLINE with type elt = int and type t = int array =
struct
  type elt = int
  type t = int array
  let init = Array.init
  let get = Array.get
  let set = Array.set
  let length = Array.length
  let of_string s =
    s |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list

  let map = Array.map
  let unsafe_get = Array.unsafe_get

  let copy = Array.copy
  let of_array = Array.copy

  let equal = (=)
  let compare = compare
end


module IntGrid : RWGRID with type elt = int and type line = int array =
  MakeRW(IntLine)
