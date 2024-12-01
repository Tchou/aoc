open Utils
module S =
struct

  (* We factor *)
  module type Move =
  sig
    type t
    val initial : t
    val pp : Format.formatter -> t -> unit
    val forward : int -> t -> t
    val north : int -> t -> t
    val east : int -> t -> t
    val south : int -> t -> t
    val west : int -> t -> t

    val left : int -> t -> t
    val right : int -> t -> t
    val dist : t -> int

  end
  module type Dir =
  sig
    val initial : (int * int) array
    val dir_field : int
    val turn_field : int
  end
  module Make (D : Dir) : Move =
  struct
    type t = (int * int) array
    let initial = D.initial
    let position = 0
    let direction = 1
    let set a i v =
      let a = Array.copy a in
      a.(i) <- v; a

    let pp fmt s =
      let x, y = s.(position) in
      let i, j = s.(direction) in
      Format.fprintf fmt "(%d, %d : <%d, %d>)" x y i j
    let forward n s =
      let x, y = s.(position) in
      let i, j = s.(direction) in
      set s position (x+n*i, y+n*j)

    let north n s =
      let i, j = s.(D.dir_field) in
      set s D.dir_field (i, j + n)
    let south n s =
      let i, j = s.(D.dir_field) in
      set s D.dir_field (i, j - n )
    let east n s =
      let i, j = s.(D.dir_field) in
      set s D.dir_field (i + n, j)
    let west n s =
      let i, j = s.(D.dir_field) in
      set s D.dir_field (i - n, j)

    let rec right a s =
      match a with
        90 ->
        let i, j = s.(D.turn_field) in
        set s D.turn_field (j, - i)
      | 180 -> right 90 (right 90 s)
      | 270 -> right 90 (right 180 s)
      | _ -> assert false

    let left a s = right (360 - a) s
    let dist s =
      let x, y = s.(position) in
      (abs x) + (abs y)
  end

  module Dir1 = struct
    let initial = [| 0,0 ; 1, 0|]
    let dir_field = 0
    let turn_field = 1
  end
  module Dir2 = struct
    let initial = [| 0,0 ; 10, 1|]
    let dir_field = 1
    let turn_field = 1
  end

  module Move1 = Make (Dir1)
  module Move2 = Make (Dir2)

  let read_input () =
    Input.fold_scan "%[EWNSFRL]%d" (fun acc s i -> (s.[0], i)::acc) []
    |> List.rev

  let name = Name.mk "s12"

  let solve (module M: Move) =
    let open M in
    let moves = read_input () in
    let s =
      List.fold_left (fun state (c, i) ->
          match c with
          'F' -> forward i state
          | 'N' -> north i state
          | 'S' -> south i state
          | 'E' -> east i state
          | 'W' -> west i state
          | 'R' -> right i state
          | 'L' -> left i state
          | _ -> state
        ) initial moves
    in
    Ansi.(printf "%a%d%a\n" fg green (dist s) clear color)

  let solve_part1 () = solve (module Move1)
  let solve_part2 () = solve (module Move2)
end

let () = Solution.register_mod (module S)