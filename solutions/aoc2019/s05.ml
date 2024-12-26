open Utils
module S =
struct
  let name = Name.mk "s05"
  let solve input =
    let code = Intcode.read () in
    let state = Intcode.make_state code in
    let () = Queue.push input state.stdin in
    let _ = Intcode.eval state in
    let n = Queue.take state.stdout in
    Solution.printf "%d" n

  let solve_part1 () = solve 1
  let solve_part2 () = solve 5

end

let () = Solution.register_mod (module S)