open Utils
module S =
struct
  let name = Name.mk "s19"

  module G = Grid.StringGrid

  let read_input () =
    let grid = G.read () in
    let start = G.find (function '|' -> true | _ -> false) grid in
    start, grid

  let run grid start =
    let text = Queue.create () in
    let rec loop dir pos count =
      if G.inside grid pos then
        let count = count + 1 in
        match grid.G.!(pos) with
          '+' -> 
          let dir' = Grid.left90 dir in
          let pos' = Grid.(pos +! dir') in
          if G.inside grid pos' && grid.G.!(pos') <> ' ' then 
            loop dir' pos' count
          else
            let dir' = Grid.right90 dir in
            loop dir' Grid.(pos +! dir') count
        | 'A'..'Z' as c -> Queue.push c text; loop dir Grid.(pos +! dir) count
        | '|' | '-' -> loop dir Grid.(pos +! dir) count
        | ' ' -> count - 1
        | _ -> assert false
      else count
    in
    let c = loop Grid.south start 0 in
    Queue.to_seq text |> String.of_seq, string_of_int c

  let solve f =
    let start, grid = read_input () in
    let s = f (run grid start) in
    Solution.printf "%s" s

  let solve_part1 () = solve fst
  let solve_part2 () = solve snd
end

let () = Solution.register_mod (module S)