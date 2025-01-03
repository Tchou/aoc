open Utils
module S =
struct
  let name = Name.mk "s25"

  let read_input () =
    Input.fold_scan " %d,%d,%d,%d" (fun acc x y z t -> [[x; y ; z; t]]::acc) []

  let dist p1 p2 =
    List.fold_left2 (fun acc a b -> acc + abs (a-b)) 0 p1 p2

  let merge groups =
    let rec try_merge g others acc found =
      match others with
        [] -> g, acc, found
      | o :: ll ->
        if List.exists (fun p1 ->
            List.exists (fun p2 -> dist p1 p2 <= 3) o) g
        then
          try_merge (List.rev_append o g) ll acc true
        else
          try_merge g ll (o::acc) found
    in
    let rec loop l acc =
      match l with
        [] -> acc
      | g :: ll ->
        let g', rest, found = try_merge g ll [] false in
        if found then
          loop (List.rev_append (g'::rest) acc) []
        else
          loop ll (g::acc)
    in
    loop groups []
    |> List.length



  let solve_part1 () =
    let points = read_input () in
    let n = merge points in
    Solution.printf "%d" n
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)