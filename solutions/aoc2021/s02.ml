open Utils

module S =
struct
  let name = Name.mk "s02"

  let solve_part1 () =
    let x, y =
      Input.fold_scan "%s %d" (fun (hpos, depth) s d ->
          match s with
            "forward" -> (hpos + d, depth)
          | "down" -> (hpos, depth + d)
          | "up" -> (hpos, depth - d)
          | _ -> assert false
        ) (0, 0)
    in Ansi.printf "%d\n" (x*y)
  let solve_part2 () =
    let x, y, _ =
      Input.fold_scan "%s %d" (fun (hpos, depth, aim) s d ->
          match s with
            "forward" -> (hpos + d, depth + aim * d, aim)
          | "down" -> (hpos, depth, aim + d)
          | "up" -> (hpos, depth, aim - d)
          | _ -> assert false
        ) (0, 0, 0)
    in Ansi.printf "%d\n" (x*y)

end

let () = Solution.register_mod (module S)