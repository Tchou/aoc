open Utils
module S =
struct
  let name = Name.mk "s10"

  let char_of_int c = Char.unsafe_chr (c + 48)
  let rle bin bout =
    let rec loop i prev n_prev len =
      if i >= len then begin
        Buffer.add_char bout (char_of_int n_prev);
        Buffer.add_char bout prev;
      end else 
        let c = Buffer.nth bin i in
        if c = prev then loop (i+1) prev (n_prev+1) len
        else begin
          Buffer.add_char bout (char_of_int n_prev);
          Buffer.add_char bout prev;
          loop (i+1) c 1 len
        end
    in
    loop 1 (Buffer.nth  bin 0) 1 (Buffer.length bin)

  let read_input () = Input.read_line ()

  let iterate s n =
    let bin = Buffer.create 16 in 
    let bout = Buffer.create 16 in
    Buffer.add_string bin s;
    let rec loop i bin bout =
      if i = n then Buffer.length bin
      else begin
        rle bin bout;
        Buffer.clear bin;
        loop (i+1) bout bin;
      end
    in
    loop 0 bin bout

  let solve n () =
    let s = read_input () in
    let n = iterate s n in
    Solution.printf "%d" n

  let solve_part1 = solve 40
  let solve_part2 = solve 50
end

let () = Solution.register_mod (module S)