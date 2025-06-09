open Utils
module S =
struct
  let name = Name.mk "s18"

  let read_input () = Input.read_line ()

  let (.![]) s i = Bytes.unsafe_get s i 
(*

    Its left and center tiles are traps, but its right tile is not.
    Its center and right tiles are traps, but its left tile is not.
    Only its left tile is a trap.
    Only its right tile is a trap.

    So it is doing XOR b'[i] <- (b.[i-1] == b.[i+1])
    If we store '^' as '\x01' and '.' as '\x00' we can make the inner loop
    faster.
    We add an extra '.' on the side to not check bounds.
    Finaly we alternate between two buffers instead of allocating strings.

    *)
  let next_line len b_src b_dst =
    let safe_count = ref 0 in
    for i = 1 to len do
      let m2 = Char.code b_src.![i] in
      let m1 = Char.code b_src.![i-1] in
      let m3 = Char.code b_src.![i+1] in
      safe_count := (1 land (lnot m2)) + !safe_count;
      let m =  (1 land (m1 + m3)) in
      let c = Char.unsafe_chr m in
      Bytes.unsafe_set b_dst i c;
    done;
    !safe_count

  let gen_rows n s =
    let len = String.length s in
    let s = ("." ^ s ^ ".") |> String.map (function '^' -> '\x01' | _ -> '\x00') in
    let b = Bytes.of_string s in
    let visited = Hashtbl.create 16 in
    let b_aux = Bytes.copy b in
    let rec loop n b b_aux count =
      if n = 0 then count else
        let s = Bytes.to_string b in
        let count = count + next_line len b b_aux in
        loop (n-1) b_aux b count
    in
    loop n b b_aux 0


  let solve n  =
    let s = read_input () in
    let n = gen_rows n s in
    Solution.printf "%d" n

  let solve_part1 () = solve 40
  let solve_part2 () = solve 400000
end

let () = Solution.register_mod (module S)