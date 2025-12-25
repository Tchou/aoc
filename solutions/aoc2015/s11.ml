open Utils
module S =
struct
  let name = Name.mk "s11"
  let read_input () = Input.read_line ()

  let incr_byte b i carry = 
    let c = Bytes.unsafe_get b i in 
    let c' = Char.(unsafe_chr (code c + int_of_bool carry)) in
    let c' = match c' with
      | 'i' -> 'j'
      | 'l' -> 'm'
      | 'o' -> 'p'
      | '{' -> 'a'
      | _ -> c'
    in
    Bytes.set b i c';
    carry && c' = 'a'
  let next b =
    let rec loop i carry =
      if i < 0 then carry else
        let carry = incr_byte b i carry in
        loop (i-1) carry
    in
    loop 7 true

  let has3 b =
    let rec loop i last =
      if i > last then false else
        let c1 = Char.code (Bytes.unsafe_get b i) in
        let c2 = Char.code (Bytes.unsafe_get b (i+1)) in
        if c1 + 1 <> c2 then loop (i+1) last
        else
          let c3 = Char.code (Bytes.unsafe_get b (i+2)) in
          if c2 + 1 <> c3 then loop (i+2) last
          else true
    in
    loop 0 (Bytes.length b - 3)

  let not_found = '\x00'
  let has_pair b except =
    let rec loop i prev len =
      if i >= len then not_found
      else
        let c = Bytes.unsafe_get b i in
        if c = prev && c <> except then c else
          loop (i+1) c len
    in
    loop 1 (Bytes.unsafe_get b 0) (Bytes.length b)

  let has2_pairs b =
    let c1 = has_pair b not_found in
    c1 <> not_found && (has_pair b c1 <> not_found)


  let find_next_valid password =
    let b = Bytes.of_string password in
    let rec loop () =
      next b |> ignore;
      if has2_pairs b && has3 b then ()
      else loop ()
    in 
    loop ();
    Bytes.to_string b

  let solve_part1 () =
    let password = read_input () in
    let s = find_next_valid password in
    Solution.printf "%s" s
  let solve_part2 () =
    let password = read_input () in
    let s = find_next_valid password in
    let s' = find_next_valid s in
    Solution.printf "%s" s'

end

let () = Solution.register_mod (module S)