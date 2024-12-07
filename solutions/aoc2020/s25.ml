open Utils
open Syntax
module S =
struct
  let name = Name.mk "s25"

  let _PMOD = 20201227

  (* https://en.wikipedia.org/wiki/Baby-step_giant-step *)
  let pow_mod b e m =
    if m = 1 then 0
    else
      let rec loop b e r =
        if e <= 0 then r else
          let r = if e mod 2 = 1 then (r * b) mod m else r in
          loop ((b * b) mod m) (e lsr 1) r
      in
      loop (b mod m ) e 1


  let baby_step_giant_step b x m =
    (* finds n such that b ^ n ≡ x mod m *)
    let k = int_of_float (ceil (sqrt (float (m-1)))) in
    let cache = ~%[] in
    for i = 0 to k * 2 do
      cache.%{pow_mod b i m} <-i;
    done;
    let inv = pow_mod b (k*(m-2)) m in
    let rec loop i =
      if i >= k then -1 else
        let tmp = (x * (pow_mod inv i m)) mod m in
        match cache.%?{tmp} with
          Some n ->
          let r = i * k + n in
          assert (pow_mod b r m = x);r
        | None -> loop (i+1)
    in
    loop 0

  let rec dovetail f i limit =
    for j = 0 to i - 1 do
      f i j;
      f j i;
    done;
    f i i;
    if i < limit then dovetail f (i+1) limit

  let find_solution pk1 pk2 limit =
    let c1 = baby_step_giant_step 7 pk1 _PMOD in
    let c2 = baby_step_giant_step 7 pk2 _PMOD in
    let o = baby_step_giant_step 7 1 _PMOD in
    let exception Found of int in
    try
      dovetail (fun i j ->
          let n1 = c1 + i * o in
          let n2 = c2 + j * o in
          let enc1 = pow_mod pk2 n1 _PMOD in
          let enc2 = pow_mod pk1 n2 _PMOD in
          if enc1 = enc2 then raise (Found enc1)
        ) 0 limit;
      -1
    with Found n -> n

  let solve_part1 () =
    let pk1 = read_line () |> int_of_string in
    let pk2 = read_line () |> int_of_string in
    let n = find_solution pk1 pk2 100 in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)