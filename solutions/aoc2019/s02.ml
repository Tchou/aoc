open Utils
module S =
struct
  let name = Name.mk "s02"

  let find_inputs code =
    let state = Intcode.make_state code in
    let exception Found of int in
    try
      for i = 0 to 99 do
        for j = 0 to 99 do
          let nstate = Intcode.copy_state state in
          nstate.code.(1) <- i;
          nstate.code.(2) <- j;
          ignore (Intcode.eval nstate);
          if nstate.code.(0) = 19690720 then raise (Found (i * 100 + j))
        done
      done; -1
    with Found n -> n
  let solve_part1 () =
    let code = Intcode.read () in
    let state = Intcode.make_state code in
    let () =
      state.Intcode.@(1) <- 12;
      state.Intcode.@(2) <- 2
    in
    let _ = Intcode.eval state in
    let n = state.Intcode.@(0) in
    Solution.printf "%d" n

  let solve_part2 () =
    let code = Intcode.read () in
    let n = find_inputs code in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)