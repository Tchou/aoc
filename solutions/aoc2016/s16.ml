open Utils
module S =
struct
  let name = Name.mk "s16"

  type text = string * int

  let expand_until n s =
    let rec loop len = if n < len then len else
        loop (2*len+1)
    in
    s, loop (String.length s)

  let get (s, len) i =
    let mlen = String.length s in
    let rec loop len i b =
      if i < mlen then let c = String.unsafe_get s i in if b then c else if c = '0' then '1' else '0' 
      else
        let hlen = len lsr 1 in
        if i < hlen then loop hlen i b
        else if i = hlen then if b then '0' else '1'
        else let i' = (len lor 1) - 1 - i in
          loop hlen i' (not b)
    in
    loop len i true
  let rec crc t i len =
    if len = 1 then get t i
    else
      let hlen = len lsr 1 in
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
    let pattern = read_input () in
    let t = expand_until len pattern in
    let s = find_crc t len in
    Solution.printf "%s" s
  let solve_part1 () = solve 272
  let solve_part2 () = solve 35651584
end

let () = Solution.register_mod (module S)