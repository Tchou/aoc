open Utils

module S =
struct
  let name = Name.mk "s08"

  let solve_part1 () =
    Input.fold_lines (fun acc l ->
        match String.split_on_char '|' l with
          _::l::_ -> String.split_on_char ' ' l
                     |>List.fold_left (fun acc s ->
                         let l = String.length s in
                         if l = 3 || l = 4 || l = 2 || l = 7 then
                           acc+1
                         else acc) acc
        | _ -> acc) 0
    |> Solution.printf "%d"

  module CharSet = Set.Make(Char)

  let decode l =
    let table = Array.make 8 [] in
    List.iter (fun s ->
        let n = String.length s in
        let cs = CharSet.of_list (String.explode s) in
        table.(n) <- cs :: table.(n)
      ) l;
    let decoded = Array.make 10 CharSet.empty in
    decoded.(1) <- List.hd table.(2);
    decoded.(7) <- List.hd table.(3);
    decoded.(8) <- List.hd table.(7);
    decoded.(4) <- List.hd table.(4);
    let cs74 = CharSet.union decoded.(7) decoded.(4) in
    let d9, rest = List.partition (fun cs ->
        CharSet.subset cs74 cs
      ) table.(6)
    in
    decoded.(9) <- List.hd d9;
    table.(6) <- rest;
    let cs91 = CharSet.diff decoded.(9) decoded.(1) in
    let d5, rest = List.partition (fun cs ->
        CharSet.subset cs91 cs) table.(5)
    in
    decoded.(5) <- List.hd d5;
    table.(5) <- rest;
    let cs815 = CharSet.(union decoded.(5) (diff decoded.(8) decoded.(1))) in
    let d6, rest = List.partition (fun cs ->
        CharSet.equal cs815 cs
      ) table.(6)
    in
    decoded.(6) <- List.hd d6;
    decoded.(0) <- List.hd rest;
    let d3, d2 = List.partition (fun cs ->
        CharSet.subset cs decoded.(9)
      ) table.(5)
    in
    decoded.(2) <- List.hd d2;
    decoded.(3) <- List.hd d3;
    decoded




  let solve_part2 () =
    Input.fold_fields '|' (fun acc ->
        function [s1; s2] ->
          let decoded = String.split_on_char ' ' s1 |> decode in
          let l2 = String.split_on_char ' ' s2 in
          let l2 = l2 |> List.filter_map
                     (function "" -> None
                             | s -> Some (CharSet.of_list (String.explode s)))
          in
          let s = ref 0 in
          l2 |> List.iter (fun cs ->
              Array.iteri (fun i cs2 ->
                  if CharSet.equal cs cs2 then s := !s * 10 + i) decoded
            );
          acc + !s
               | _ -> acc) 0
    |> Solution.printf "%d"
end

let () = Solution.register_mod (module S)