open Utils
open Syntax
module S =
struct
  let name = Name.mk "s03"

  (* Why bother with regular expressions ? :D *)
  let read_input () =
    let b = Buffer.create 16 in
    Input.fold_chars (fun () -> Buffer.add_char b) ();
    Buffer.to_bytes b

  let read_muls txt =
    let digit c n = 10 * n + Char.(code c - code '0') in
    let rec loop i state n1 n2 acc =
      if i >= Bytes.length txt then acc
      else match txt.$[i], state with
        | 'm', _             -> loop (i+1) 'u' n1 n2 acc
        | 'u', 'u'           -> loop (i+1) 'l' n1 n2 acc
        | 'l', 'l'           -> loop (i+1) '(' n1 n2 acc
        | '(', '('           -> loop (i+1) '1' n1 n2 acc
        | '0'..'9' as d, '1' -> loop (i+1) '1' (digit d n1) n2 acc
        | ',', '1'           -> loop (i+1) '2' n1 n2 acc
        | '0'..'9' as d, '2' -> loop (i+1) '2' n1 (digit d n2) acc
        | ')', '2'           -> loop (i+1) 'm' 0 0 (n1*n2 + acc)
        | _                  -> loop (i+1) 'm' 0 0 acc
    in
    loop 0 'm' 0 0 0

  let erase_don't txt =
    let rec loop_don't i state =
      if i < Bytes.length txt then
        match txt.$[i], state with
        | 'd', _     -> loop_don't (i+1) 'o'
        | 'o', 'o'   -> loop_don't (i+1) 'n'
        | 'n', 'n'   -> loop_don't (i+1) '\''
        | '\'', '\'' -> loop_don't (i+1) 't'
        | 't', 't'   -> loop_don't (i+1) '('
        | '(', '('   -> loop_don't (i+1) ')'
        | ')', ')'   -> loop_do (i+1) 'd'
        | _ -> loop_don't (i+1) 'd'
    and loop_do i state =
      if i < Bytes.length txt then
        let c = txt.$[i] in txt.$[i] <- ' ';
        match c, state with
        | 'd', _   -> loop_do (i+1) 'o'
        | 'o', 'o' -> loop_do (i+1) '('
        | '(', '(' -> loop_do (i+1) ')'
        | ')', ')' -> loop_don't (i+1) 'd'
        | _        -> loop_do (i+1) 'd'
    in
    loop_don't 0 'd'

  let solve fix =
    let s = read_input () in
    let () = fix s in
    let n = read_muls s in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
  let solve_part1 () = solve ignore
  let solve_part2 () = solve erase_don't

end

let () = Solution.register_mod (module S)