open Utils
module S =
struct
  let name = Name.mk "s02"
  module G = Grid.StringGrid

  let keypad1 = G.of_string "123\n456\n789"
  let keypad2 = G.of_string
  "00100\n02340\n56789\n0ABC0\n00D00"
  let move keypad pos dir =
    let (x, y) as pos' = Grid.(pos +! dir) in
    if x < 0 || y < 0 || x >= G.width keypad || y >= G.height keypad then pos
    else let c = keypad.G.!(pos') in
    if c = '0' then pos else pos'

  let map_dir = function
    'U' -> Grid.north
    | 'D' -> Grid.south
    | 'L' -> Grid.west
    | 'R' -> Grid.east
    | _ -> assert false

  let get_digit keypad pos s =
    String.fold_left 
      (fun pos c ->
         move keypad pos (map_dir c)
      ) pos s

  let read_input () = Input.list_lines Fun.id

  let compute_code start keypad lines =
    let b = Buffer.create 16 in
    lines |> List.fold_left (fun pos s ->
        let pos' = get_digit keypad pos s in
        Buffer.add_char b keypad.G.!(pos');
        pos' 
      ) start
    |> ignore;
    Buffer.contents b

  let solve start keypad =
    let lines = read_input () in
    let s = compute_code start keypad lines in
    Solution.printf "%s" s
  let solve_part1 () = solve (1,1) keypad1
  let solve_part2 () = solve (0,2) keypad2
end

let () = Solution.register_mod (module S)