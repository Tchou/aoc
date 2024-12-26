open Utils
open Syntax

module S =
struct
  let name = Name.mk "s16"

  let load_input () =
    Input.fold_lines (Fun.flip List.cons) []
    |> List.rev
    |> Array.of_list

  type dir = Left | Right | Down | Up

  let coord_of_dir = function
    | Left -> (0, -1)
    | Right -> (0,1)
    | Down -> (1, 0)
    | Up -> (-1, 0)

  let add_dir (r, c) d =
    let i, j = coord_of_dir d in
    r + i, c + j

  let traverse_tile dir tile =
    match dir, tile with
    | Left, '/'   -> [ Down ]
    | Left, '\\'  -> [ Up ]
    | Left, '-'   -> [ Left ]
    | Left, '|'   -> [Up;Down]
    | Right, '/'  -> [ Up ]
    | Right, '\\' -> [ Down ]
    | Right, '-'  -> [ Right ]
    | Right, '|'  -> [Up;Down]
    | Down, '/'   -> [ Left ]
    | Down, '\\'  -> [ Right ]
    | Down, '-'   -> [ Left;Right ]
    | Down, '|'   -> [Down]
    | Up, '/'     -> [ Right ]
    | Up, '\\'    -> [ Left ]
    | Up, '-'     -> [ Left;Right ]
    | Up, '|'     -> [Up]
    | d, c -> [ d ]

  let dir_bit = function
      Left -> 0
    | Right -> 1
    | Down -> 2
    | Up -> 3

  let dir_mask d =
    1 lsl (dir_bit d)
  let has_dir c d = c land (dir_mask d) <> 0
  let mark_dir c d = c lor (dir_mask d)


  let (.@[]) g (r,c) =
    Char.code (g.(r).$[c])

  let (.@[]<-) g (r,c) v =
    g.(r).$[c] <- (Char.chr v)

  let (.*[]) g (r, c) =
    g.(r).[c]

  let valid_coord g (r, c) =
    r >= 0 && c >= 0 &&
    r < Array.length g &&
    c < String.length g.(r)
  let run start dir grid =
    let marks = Array.map
        (fun s -> Bytes.make (String.length s) '\x00')
        grid
    in
    let count = ref 0 in
    let rec loop coord dir todo =
      if valid_coord grid coord then 
        let mark = marks.@[coord] in
        if not (has_dir mark dir) then begin
          let nmark = mark_dir mark dir in
          marks.@[coord] <- nmark;
          if mark = 0 then incr count;
          match traverse_tile dir grid.*[coord] with
            [ d ] -> loop (add_dir coord d) d todo
          | [d1; d2] -> loop (add_dir coord d1) d1
                          (((add_dir coord d2), d2)::todo)
          | _ -> assert false
        end
        else loop_todo todo
      else loop_todo todo
    and loop_todo todo =
      match todo with
        [] -> ()
      | (coord', dir')::todo' -> loop coord' dir' todo'
    in
    loop start dir [];
    !count


  let solve_part1 () =
    load_input ()
    |> run (0,0) Right
    |> Solution.printf "%d"

  let solve_part2 () =
    let grid = load_input () in
    let alen = Array.length grid in
    let slen = String.length grid.(0) in
    let n = ref 0 in
    for r = 0 to alen - 1 do
      n := max !n (run (r, 0) Right grid);
      n := max !n (run (r, slen-1) Left grid);
    done;
    for c = 0 to slen - 1 do
      n := max !n (run (0, c) Down grid);
      n := max !n (run (alen-1, c) Up grid);
    done;
    Solution.printf "%d" !n
end

let () = Solution.register_mod (module S)