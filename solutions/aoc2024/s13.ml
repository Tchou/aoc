open Utils
module S =
struct
  let name = Name.mk "s13"
  (*
  Button A: X+94, Y+34
  Button B: X+22, Y+67
  Prize: X=8400, Y=5400

  67B+34A=5400
  22B+94A=8400
  ------------
    22 * 67B + 22 * 34A = 22 * 5400
  - 67 * 22B + 67 * 93A = 67 * 8400
----------------------------------------
  -5483A = 
  *)
  type eq = { a : int * int; b : int * int; prize : int * int }


  let solve part1 {a = xa, ya; b = xb, yb; prize = xp, yp } =
    let xp = if part1 then xp else xp + 10000000000000 in
    let yp = if part1 then yp else yp + 10000000000000 in
    let yya, yyb, yyp = xa * ya, xa * yb, xa * yp in
    let xxa, xxb, xxp = ya * xa, ya * xb, ya * xp in
    let db, dp = yyb - xxb, yyp - xxp in
    let b, rb = dp / db, dp mod db in
    if  rb <> 0 then None
    else if b < 0 || (part1 && b > 100) then None
    else let da = xp - xb * b in
      let a, ra = da / xa, da mod xa in
      if ra <> 0 then None
      else if a < 0 || (part1 && a > 100) then None
      else Some (a, b)

  let dummy = let z = 0,0 in
    { a = z; b = z; prize = z }
  let read_input () =
    Input.fold_lines (fun acc line ->
        match line, acc with
          "", _ -> dummy :: acc
        |_, eq :: acc -> begin
            Scanf.sscanf line "%[^:]: X%[+=]%d, Y%[+=]%d"
              (fun s _ x _ y ->
                 if s = "Prize" then {eq with prize = x, y}::acc
                 else if s = "Button A" then { eq with a = x, y}::acc
                 else { eq with b = x, y }::acc)
          end
        | _ -> acc) [dummy]
    |> List.rev

  let solve_list part1 l =
    List.fold_left (fun acc eq ->
        match solve part1 eq with
          None -> acc
        | Some (a, b) -> (3*a + b + acc)
      ) 0 l
  let solve part1  =
    let eqn = read_input () in
    let n = solve_list part1 eqn in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  let solve_part1 () = solve true
  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S)