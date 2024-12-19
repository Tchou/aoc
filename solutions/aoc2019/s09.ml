open Utils
module S =
struct
  let name = Name.mk "s09"
  let solve input =
    let code = Intcode.read () in
    let state = Intcode.make_state code in
    Queue.push input state.stdin;
    let _ = Intcode.eval state in
    let s = state.Intcode.stdout
            |> Queue.to_seq
            |> Seq.map string_of_int
            |> List.of_seq
            |> String.concat ","
    in
    Ansi.(printf "%a%s%a\n" fg green s clear color)

    let solve_part1 () = solve 1
  let solve_part2 () = solve 2
end

let () = Solution.register_mod (module S)