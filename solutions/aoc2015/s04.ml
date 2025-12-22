open Utils
module S =
struct
  let name = Name.mk "s04"
  let read_input () =
    Input.read_line ()

  let is_valid4 (d : Digest.MD5.t) = 
    d.[0] = '\x00' && d.[1] = '\x00'
  let is_valid5 (d : Digest.MD5.t) =
    is_valid4 d &&  d.[2] <= '\x0f'
  let is_valid6 (d : Digest.MD5.t) =
    is_valid4 d && d.[2] = '\x00'

  let blit_num bytes num start len10 =
    let acc = ref num in
    for i = start + len10 - 1 downto start do
      Bytes.unsafe_set bytes i (Char.unsafe_chr (48 + (!acc mod 10)));
      acc := !acc / 10;
    done

  let enumerate is_valid key =
    let buff = Bytes.make 128 '\x00' in
    let start = String.length key in
    Bytes.blit_string key 0 buff 0 start;
    let rec loop n pow10 len10 =
      blit_num buff n start len10;
      let d = Digest.MD5.subbytes buff 0 (start + len10) in
      if is_valid d then n else 
        let n' = n+1 in
        if n' < pow10 then loop n' pow10 len10
        else loop n' (10*pow10) (len10+1)
    in
    loop 0 10 1

  let solve is_valid () =
    let key = read_input () in
    let n = enumerate is_valid key in
    Solution.printf "%d" n

  let solve_part1 = solve is_valid5
  let solve_part2 = solve is_valid6
end

let () = Solution.register_mod (module S)