open Utils
module S =
struct
  let name = Name.mk "s05"


  let incr_bytes b plen ridx =
    let rec loop idx = 
      if idx < plen then begin
        incr ridx;
        Bytes.set b (idx+1) '1';
        for i = idx+2 to !ridx do
          Bytes.set b i '0';
        done;
      end
      else
        let c = Bytes.unsafe_get b idx in
        if c = '9' then begin
          Bytes.set b idx '0';
          loop (idx - 1);
        end else Bytes.set b idx (Char.unsafe_chr (Char.code c + 1))
    in
    loop !ridx

  let read_input () =
    Input.list_scan "%s" Fun.id 
    |> List.hd

  let is_valid md =
    String.unsafe_get md 0 = '\x00' &&
    String.unsafe_get md 1 = '\x00' && 
    String.unsafe_get md 2 < '\x10'

  let hex_char c =
    Char.unsafe_chr (
      if c < '\x0a' then Char.code c + 48 (* Char.code '0' *)
      else Char.code c + 87) (* Char.code 'a' - 10*)

  let find_password h =
    let b = Buffer.create 16 in
    let hb = Bytes.of_string (h ^ String.make 16 '0') in
    let plen = String.length h in
    let rindx = ref plen in
    let rec loop () =
      if Buffer.length b = 8 then Buffer.contents b else
        let md = Digest.subbytes hb 0 (!rindx+1) in
        if is_valid md then Buffer.add_char b (hex_char md.[2]);
        incr_bytes hb plen rindx;
        loop ()
    in loop ()


  let find_password2 h =
    let b = Bytes.make 8 '_' in
    let found = ref 0 in
    let hb = Bytes.of_string (h ^ String.make 16 '0') in
    let plen = String.length h in
    let rindx = ref plen in
    let rec loop () =
      let md = Digest.subbytes hb 0 (!rindx+1) in
      if is_valid md then begin
        let pidx = Char.code md.[2] in
        if pidx >= 0 && pidx <= 7 then
          let c = hex_char (Char.unsafe_chr ((Char.code md.[3]) lsr 4)) in
          if Bytes.unsafe_get b pidx = '_' then begin
            Bytes.set b pidx c;
            incr found;
            if !found == 8 then raise Exit
          end;
      end;
      incr_bytes hb plen rindx;
      loop ()
    in try loop () with Exit -> Bytes.to_string b


  let solve find =
    let h = read_input () in
    let s = find h in
    Solution.printf "%s" s

  let solve_part1 () = solve find_password
  let solve_part2 () = solve find_password2
end

let () = Solution.register_mod (module S)