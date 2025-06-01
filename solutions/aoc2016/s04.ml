open Utils
open Syntax

module S =
struct
  let name = Name.mk "s04"

  let read_input () =
    Input.list_scan "%[a-z-]%d[%[^]]" (fun s id crc -> (s, id, crc))

  let compute_crc s =
    let h = ~%[] in
    String.iter (function 'a'..'z' as c ->
        h.%{c} <- 1 + (h.%?{c} or 0)
        | _ -> ()
      ) s;
    h |> Hashtbl.to_seq |> List.of_seq
    |> List.sort (fun (c1, n1) (c2, n2) -> 
        let c = Int.compare n2 n1 in
        if c = 0 then Char.compare c1 c2
        else c)
    |> List.map fst
    |> List.take 5
    |> String.implode
  let sum_valid_ids l =
    l 
    |> List.filter (fun (s, _, c) -> compute_crc s = c)
    |> List.map (fun (_, x, _) -> x)
    |> Iter.(sum list (module Int) )

  let solve_part1 () =
    let l = read_input () in
    let n = sum_valid_ids l in
    Solution.printf "%d" n


  let decypher n s =
    let n = n mod 26 in
    String.map (function 'a'..'z' as c ->
        let x = Char.code c - Char.code 'a' in
        let x = (x + n) mod 26 in
        Char.chr (x + Char.code 'a')
        | '-' -> ' '
        | x -> x) s

  let find_room l =
    List.find_map (fun (room, id, _) ->
        let s = decypher id room in
        if String.starts_with ~prefix:"northpole object storage" s then Some id else None
      ) l
    |> Option.get

  let solve_part2 () =
    let l = read_input () in
    let id = find_room l in
    Solution.printf "%d" id

end

let () = Solution.register_mod (module S)