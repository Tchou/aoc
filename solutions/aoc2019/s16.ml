open Utils
module S =
struct
  let name = Name.mk "s16"

  let read_input () =
    let byte = read_line () |> Bytes.of_string in
    for i = 0 to Bytes.length byte - 1 do
      Bytes.set_uint8 byte i (Bytes.get_uint8 byte i - Char.code '0');
    done;
    byte

  let output_byte = Bytes.map (fun c -> Char.chr (Char.code c + Char.code '0'))
  let add_digits n bytes blen =
    let rec loop i k pos acc =
      let o = i+k in
      if o >= blen then acc
      else
        let b = Char.code (Bytes.unsafe_get bytes o) in
        let acc = acc + if pos then b else -b in
        let k1 = k + 1 in
        if k1 = n then loop (i+n+n) 0 (not pos) acc
        else loop i k1 pos acc
    in
    loop (n-1) 0 true 0

  let phase _ byte_in byte_out =
    let blen = Bytes.length byte_in in
    for n = 1 to blen - 1 do
      let d = add_digits n byte_in blen in
      Bytes.unsafe_set byte_out (n-1) (Char.unsafe_chr ((abs d ) mod 10));
    done

  let rec fft phase_fun offset phases byte_in byte_out =
    if phases = 0 then byte_in
    else
      let () = phase_fun offset byte_in byte_out in
      fft phase_fun offset (phases - 1) byte_out byte_in

  let phase2 offset byte _ =
    let len = Bytes.length byte in
    let acc = ref 0 in
    for i = len - 1 downto offset do
      acc := !acc + Char.code (Bytes.unsafe_get byte i);
      Bytes.set_uint8 byte i (!acc mod 10);
    done

  let solve_part1 () =
    let byte_in = read_input () in
    let byte_out = Bytes.copy byte_in in
    let byte_out = fft phase 0 100 byte_in byte_out in
    let s = Bytes.sub_string (output_byte byte_out) 0 8 in
    Ansi.(printf "%a%s%a\n" fg green s clear color)

  let expand byte =
    let blen = Bytes.length byte in
    Bytes.init (blen * 10000) (fun i -> Bytes.unsafe_get byte (i mod blen))

  let solve_part2 () =
    let input = read_input () in
    let offset = Bytes.sub_string (output_byte input) 0 7 |> int_of_string in
    let blen = Bytes.length input in
    let full_length = blen * 10000 in
    let () = assert (full_length/ offset = 1) in
    (* because the offset is so large, the size of the
       sequence is between offset and 2*offset, all numbers
       between offset and the end of the sequence are bound to
       1.
       We can compute directly:
    *)
    let byte = expand input in
    let byte_out = fft phase2 offset 100 byte byte in
    let outb = Bytes.sub byte offset 8 in
    let s = output_byte outb |> Bytes.to_string in
    Ansi.(printf "%a%s%a\n" fg green s clear color)


end

let () = Solution.register_mod (module S)