open Utils
module S =
struct
  let name = Name.mk "s16"

  type tree = 
      Str of string
    | Node of tree * int

  let length = function
      Str s -> String.length s
    | Node (_, i) -> 2*i + 1

  let str s = Str s

  let node t = Node (t, length t)

  let rec expand_until n t =
    let len = length t in
    if len >= n then t
    else
      expand_until n (node t)
  let get t i =
    let rec loop t i b =
      match t with
        Str s -> let c = String.unsafe_get s i in if b then c else if c = '0' then '1' else '0'
      | Node (t, j) ->
        if i < j then loop t i b
        else if i = j then if b then '0' else '1'
        else let i' = (j lsl 1) - i in
          loop t i' (not b)
    in
    loop t i true

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