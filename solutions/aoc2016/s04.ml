open Utils
open Syntax

module S =
struct
  let name = Name.mk "s04"

  let read_input () =
    Input.list_scan "%[a-z-]%d[%[^]]" (fun s id crc -> (s, id, crc))

  let compute_crc s =
    let h = ~%[] in
    Iter2.(
      string s
      |> iter (function 'a'..'z' as c ->
          h.%{c} <- 1 + (h.%?{c} or 0)
          | _ -> ()
        );
      items h
      |> sort ~compare:(fun (c1, n1) (c2, n2) -> 
          let c = Int.compare n2 n1 in
          if c = 0 then Char.compare c1 c2
          else c)
      |> map fst
      |> take 5
      |> to_string
    )
  let sum_valid_ids l =
    Iter2.(
      list l 
      |> fold (fun acc (s, x, c) -> 
          if compute_crc s = c then acc + x else acc) 0
    )

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