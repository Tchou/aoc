open Utils
open Syntax

module S =
struct
  let name = Name.mk "s07"

  let split_line s =
    let l = String.split_on_char '[' s in
    let l = List.concat_map (String.split_on_char ']') l in
    let pos = ref 0 in
    List.partition (fun _ -> let p = !pos in incr pos; p mod 2 = 0) l

  let read_input () = 
    Input.list_lines split_line

  let is_abba s i = s.[i] = s.[i+3] &&
                    s.[i+1] = s.[i+2] &&
                    s.[i] <> s.[i+1]

  let has_abba s = 
    let len = String.length s in 
    let rec loop i =
      if i >= len - 3 then false else
        is_abba s i || loop (i+1)
    in loop 0

  let has_tls (lp, ln) =
    List.exists has_abba lp && not (List.exists has_abba ln) 

  let count_tls l = Iter2.(l |> list |> count_if has_tls)

  let solve_part1 () =
    let l = read_input () in
    let n = count_tls l in
    Solution.printf "%d" n

  let collect_aba h s =
    for i = 0 to String.length s - 3 do
      if s.[i] = s.[i+2] && s.[i] <> s.[i+1] then
        let k = String.init 3 (fun j -> s.[i+j]) in
        h.%{k} <- ()
    done


  let invert_aba s =
    let b = Bytes.make 3 s.[1] in
    Bytes.set b 1 s.[0];
    Bytes.unsafe_to_string b

  let has_ssl (lp, ln) =
    let hp = ~%[] in
    let hn = ~%[] in
    List.iter (collect_aba hn) ln;
    if Hashtbl.length hn = 0 then false else begin
      List.iter (collect_aba hp) lp;
      Iter2.(hp |> keys |> exists (fun s -> hn %? (invert_aba s)))
    end
  let count_ssl l = Iter2.(l |> list |> count_if has_ssl)

  let solve_part2 () =
    let l = read_input () in
    let n = count_ssl l in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)