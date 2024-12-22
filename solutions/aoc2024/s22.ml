open Utils
module S =
struct
  let name = Name.mk "s22"

  let read_input () =
    Input.fold_scan "%d" (fun acc d -> d :: acc) []
    |> List.rev

  let mask = 16777216 - 1 (* 1 lsl 24 - 1 *)
  let step64 n =
    let p = n lsl 6 in (* x64 *)
    (p lxor n) land mask

  let step32 n =
    let p = n lsr 5 in (* /32 *)
    (p lxor n) land mask

  let step2048 n =
    let p = n lsl 11 in (* x2048 *)
    (p lxor n) land mask

  let next n =
    step2048 (step32 (step64 n))

  let rec iterate k n = if k = 0 then n else iterate (k-1) (next n)
  let score = List.fold_left (Agg.Left.sum (iterate 2000)) 0
  let solve_part1 () =
    let l = read_input () in
    let n = score l in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  let pack d c b a =
    let p1 = a + 9 in
    let p2 = (b + 9) lsl 5 in
    let p3 = (c + 9) lsl 10 in
    let p4 = (d + 9) lsl 15 in
    p1 lor p2 lor p3 lor p4

  (* semi brute-force works well.

     - keep a global hash table whose keys are 4-number sequences and values the
       sum of the corresponding variation in each sequences
     - for each number iterate through the sequence of remainders keeping the
       last 4 values
     - for each key (for a given number) update the global table the *first time*
       this key is found (use a local hash table)
     - Also while doing this maintain the global maximum

     A few optimizations (to make it run in ~ 120ms):
     - pack the four values of a key as as single 20 bit integer. We don't ever need
       to unpack it so we only manage integers instead of allocating tuples.
     - Instead of using hash tables, use arrays of size 2^20 words, about 8 MB each.
  *)
  let tabulate global seen limit acc_max init =
    let prev4 = init mod 10 in
    let prev3 = (next init) mod 10 in
    let prev2 = (iterate 2 init) mod 10 in
    let prev1 = (iterate 3 init) mod 10 in
    let cur = iterate 4 init in
    let a = prev3 - prev4 in
    let b = prev2 - prev3 in
    let c = prev1 - prev2 in
    let key = pack 0 a b c in
    let rec loop k cur prev1 key acc_max =
      if k = 3 then acc_max else begin
        let m10 = cur mod 10 in
        let d = m10 - prev1 in
        let key = ((key lsl 5) lor (d+9)) land 1048575 in
        let n_acc_max = if seen.(key) = init then acc_max else begin
            let v = m10 + global.(key) in
            global.(key) <- v;
            seen.(key) <- init;
            if v > acc_max then v else acc_max
          end
        in
        loop (k-1) (next cur) m10 key n_acc_max
      end
    in loop limit cur prev1 key acc_max

  let find_max numbers =
    let global = Array.make (1 lsl 20) 0 in
    let seen = Array.copy global in
    List.fold_left (tabulate global seen 2000) 0 numbers

  let solve_part2 () =
    let numbers = read_input () in
    let total = find_max numbers in
    Ansi.(printf "%a%d%a\n%!" fg green total clear color)
end

let () = Solution.register_mod (module S)