open Utils
module S =
struct
  let name = Name.mk "s15"

  let read_input () =
    let open Scanf in
    let fmt = 
      format_from_string
        "Generator %s starts with %d" "%s%d"
    in
    let a = sscanf (Input.read_line ()) fmt (fun _ d -> d) in
    let b = sscanf (Input.read_line ()) fmt (fun _ d -> d) in
    a, b

  let gen mult base = (mult * base) mod 2147483647

  let count_matching gen1 gen2 limit a b =
    let a = ref a in
    let b = ref b in
    let total = ref 0 in
    for _ = 1 to limit do
      a := gen1 16807 !a;
      b := gen2 48271 !b;
      if (!a land 0xffff) == (!b land 0xffff) then incr total;
    done;
    !total

  let gen_mult m gen =
    let mask = m-1 in
    let rec loop a b =
      let n = gen a b in
      if n land mask = 0 then n
      else loop a n
    in
    loop

  let solve gen1 gen2 limit =
    let a, b = read_input () in
    let n = count_matching gen1 gen2 limit a b in
    Solution.printf "%d" n

  let solve_part1 () =
    solve gen gen 40_000_000
  let solve_part2 () =
    solve (gen_mult 4 gen) (gen_mult 8 gen) 5_000_000
end

let () = Solution.register_mod (module S)