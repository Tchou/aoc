{open Utils}
let digit = ['0'-'9']['0'-'9']?['0'-'9']?

rule mul_reader acc = parse
  | "mul(" (digit as d1) ',' (digit as d2) ')' {
        mul_reader ((int_of_string d1 * int_of_string d2) + acc) lexbuf }
  | eof { acc }
  | _  { mul_reader acc lexbuf }

and search_dont buf = parse
    | "don't()" { search_do buf lexbuf }
    | eof { Buffer.contents buf }
    | _ as c { Buffer.add_char buf c; search_dont buf lexbuf }
and search_do buf = parse
    | "do()"  { Buffer.add_char buf '#'; search_dont buf lexbuf }
    | eof { Buffer.contents buf }
    | _ { search_do buf lexbuf }

{module S =
struct
  let name = Name.mk "s03"

  let read_input () =
    let b = Buffer.create 16 in
    Input.fold_chars (fun () -> Buffer.add_char b) ();
    Buffer.contents b

  let solve part2 =
  let s = read_input () in
  let lb = Lexing.from_string s in
  let lb = if part2 then
          let s = search_dont (Buffer.create 16) lb in
          Lexing.from_string s
          else lb
  in
  let n = mul_reader 0 lb in
  Solution.printf "%d" n
  let solve_part1 () = solve false
  let solve_part2 () = solve true

end
let () = Solution.register_mod ~variant:"ocamllex" (module S)
}