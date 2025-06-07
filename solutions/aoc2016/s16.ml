open Utils
module S =
struct
  let name = Name.mk "s16"

  type tree = 
      Str of string
    | Inv of int * tree
    | Concat of int * tree * tree

  let length = function
      Str s -> String.length s
    | Inv (len, _) -> len
    | Concat (len, _, _) -> len

  let str s = Str s
  let inv t = Inv (length t, t)

  let concat t1 t2 =
    let len = length t1 + length t2 in
    Concat (len, t1, t2)

  let zero = Str "0"

  let step t =
    concat t (concat zero (inv t))

  let rec expand_until n t =
    let len = length t in
    if len >= n then t
    else
      expand_until n (step t)

  let rec get t i =
    match t with
      Str s -> String.get s i
    | Inv (len, t') ->
      let c = get t' (len - i - 1) in
      if c = '0' then '1' else '0'
    | Concat (_, t1, t2) ->
      let len = length t1 in
      if i < len then get t1 i
      else get t2 (i-len)

  let rec crc t i len =
    if len = 1 then get t i
    else
      let hlen = len / 2 in
      match crc t i hlen, crc t (i+hlen) hlen with
      '0', '0' | '1', '1' -> '1'
               | _ -> '0'
  let factor_power_of_two n =
    let rec loop n acc =
      if n land 1 = 1 then n, acc
      else loop (n/2) (2*acc)
    in
    loop n 1

  let find_crc t len =
    let size, p2 = factor_power_of_two len in
    let s = String.init size (fun i -> crc t (i*p2) p2) in
    s

  let read_input () = Input.read_line ()


  let solve len =
    let pattern = str (read_input ()) in
    let t = expand_until len pattern in
    let s = find_crc t len in
    Solution.printf "%s" s
  let solve_part1 () = solve 272
  let solve_part2 () = solve 35651584
end

let () = Solution.register_mod (module S)