open Utils
module S =
struct
  let name = Name.mk "s02"

  let load_input () =
    Input.fold_scan "%d-%d %c: %[^ ]" (fun acc min_c  max_c c s ->
        (min_c, max_c, c, s)::acc
      )[]

  let count c s =
    let rec loop i len acc =
      if i = len then acc
      else loop (i+1) len (acc + if s.[i] = c then 1 else 0)
    in
    loop 0 (String.length s) 0

  let valid1 (min_c, max_c, c, s) =
    let n = count c s in
    min_c <= n && n <= max_c

  let valid2 (min_c, max_c, c, s) =
    let len = String.length s in
    let c1 = s.[min_c-1] in
    let c2 = s.[max_c-1] in
    (c = c1 && c <> c2) || (c <> c1 && c = c2)

  let solve valid =
    let entries = load_input () in
    List.fold_left (fun acc t -> acc + if valid t then 1 else 0)
      0 entries
    |> Ansi.printf "%d\n"


  let solve_part1 () = solve valid1

  let solve_part2 () = solve valid2

end

let () = Solution.register_mod (module S)