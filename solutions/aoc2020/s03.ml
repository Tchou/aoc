open Utils
module S =
struct
  let name = Name.mk "s03"

  let load_input () =
    Input.fold_lines (fun acc s -> s::acc) []
    |> List.rev |> Array.of_list

  let count_trees d r grid =
    let height = Array.length grid in
    let width = String.length grid.(0) in
    let rec loop i j n =
      if i >= height then n else
        loop (i+d) ((j+r) mod width)
          (n+if grid.(i).[j] = '#' then 1 else 0)
    in loop 0 0 0

  let solve_part1 () =
    load_input ()
    |> count_trees 1 3
    |> Ansi.printf "%d\n"
  let solve_part2 () =
    let grid = load_input () in
    let steps = [ 1,1; 1,3; 1,5; 1,7;2, 1] in
    List.fold_left (fun acc (d, r) -> acc * count_trees d r grid ) 1 steps
    |> Ansi.printf "%d\n"
end

let () = Solution.register_mod (module S)