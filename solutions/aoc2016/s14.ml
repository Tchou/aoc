open Utils
module S =
struct
  let name = Name.mk "s14"

  type md5 = { idx : int ; x3 : char ; x5 : Bytes.t }

  let has3 d =
    let last = String.length d - 3 in
    let rec loop i =
      if i > last then '\x00'
      else
      if d.[i] == d.[i+1] && d.[i+1] == d.[i+2]
      then d.[i]
      else loop (i+1)
    in loop 0

  let has5 d =
    let last = String.length d - 5 in
    let buf = Bytes.make (Char.code 'g') '\x00' in
    let rec loop i =
      if i > last then buf
      else
      if d.[i] == d.[i+1] &&
         d.[i+1] == d.[i+2] && 
         d.[i+2] == d.[i+3] &&
         d.[i+3] == d.[i+4]
      then (Bytes.unsafe_set buf (Char.code d.[i]) '\x01'; loop (i+5))
      else loop (i+1) 
    in loop 0 

  let enumerate compute_digest prefix =
    let dummy = {idx = -1; x5=Bytes.create 0; x3='\x00'} in
    let tab = Array.make 50_000  dummy in  
    let ensure =
      let hb = Bytes.of_string (prefix ^ String.make 16 '0') in
      let plen = String.length prefix in
      let rindx = ref plen in
      let current = ref 0 in
      let index = ref 0 in
      let rec loop idx =
        if idx >= !current then begin
          let md5 = Digest.subbytes hb 0 (!rindx+1) |> compute_digest in
          S05.S.incr_bytes hb plen rindx;
          incr index;
          let x3 = has3 md5 in
          let x5 = has5 md5 in
          let () = if x3 <> '\x00' then
              let () = tab.(!current) <- {idx = !index-1;x3;x5} in
              incr current in
          loop idx
        end
      in loop
    in
    let found = ref 0 in
    let i = ref 0 in
    let rec search_next_1000 idx x3 start stop =
      if start < stop then
        let () = ensure start in
        let mm = tab.(start) in
        if idx + 1001 >= mm.idx then
          if Bytes.unsafe_get  mm.x5 (Char.code x3) != '\x00' then 
            incr found
          else search_next_1000 idx x3 (start+1) stop
    in
    let rec loop () =
      let () = ensure !i in
      let m = tab.(!i) in
      let () = search_next_1000 m.idx m.x3 (!i+1) (!i+1001) in
      incr i;
      if !found = 64 then m.idx
      else loop ()
    in loop ()

  let md5_buff = Bytes.create 32
  let digest_to_buff d =
    for i = 0 to 15 do
      let j = i lsl 1 in
      Bytes.unsafe_set md5_buff j (S05.S.hex_char (Char.unsafe_chr (Char.code d.[i] lsr 4)));
      Bytes.unsafe_set md5_buff (j + 1) (S05.S.hex_char (Char.unsafe_chr (Char.code d.[i] land 0xf)));
    done

  let md5_n n s =
    let rec loop n = if n = 0 then Bytes.to_string md5_buff
      else 
        let s = Digest.subbytes md5_buff 0 32 in
        digest_to_buff s;
        loop (n-1)
    in
    digest_to_buff s;
    loop n

  let read_input () = Input.read_line () 

  let solve rounds = 
    let s = read_input () in
    let n = enumerate (md5_n rounds) s in
    Solution.printf "%d" n

  let solve_part1 () = solve 0
  let solve_part2 () = solve 2016
end

let () = Solution.register_mod (module S)