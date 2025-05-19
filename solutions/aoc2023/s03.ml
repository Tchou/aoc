open Utils
open Syntax

module S =
struct
  let name = Name.mk "s03"

  let load_input () =
    let symbols = ~%[] in
    let buff = Buffer.create 8 in
    let number_pos = ref (-1) in
    let lacc = ref [] in
    let reset_refs () =
      number_pos := -1; Buffer.clear buff
    in
    let add_number lnum =
      if !number_pos >= 0 then begin
        let s = Buffer.contents buff in
        let n = int_of_string s in
        lacc := (n, (lnum, !number_pos), String.length s) :: !lacc;
        reset_refs ()
      end
    in
    let _ =
      Input.fold_lines (fun lnum line ->
          reset_refs ();
          for i = 0 to String.length line - 1 do
            match line.[i] with
            | '0'..'9' as c ->
              if !number_pos < 0 then number_pos := i;
              Buffer.add_char  buff c;
            | '.' -> add_number lnum
            | c -> add_number lnum;
              symbols.%{lnum, i} <- c
          done;
          add_number lnum;
          lnum + 1) 0
    in
    !lacc, symbols

  let find_adjacent_stars n (r,c) len symb =
    let star_pos = ref [] in
    let found = ref false in
    let check i j =
      match symb.%?{i, j} with
        Some c ->
        found := true;
        if c = '*' then star_pos := ((i,j), n) :: !star_pos;
      | None -> ()
    in
    check r (c-1);
    check r (c+len);
    for i = c-1 to c+len do
      check (r-1) i;
      check (r+1) i;
    done;
    !found, !star_pos

  let solve_part1 () =
    let num_list, symbols = load_input () in
    let sum =
      List.fold_left (fun acc (n, pos, len) ->
          if fst (find_adjacent_stars n pos len symbols) then
            acc + n
          else acc
        ) 0 num_list
    in
    Solution.printf "%d" sum

  let solve_part2 () =
    let num_list, symbols = load_input () in
    let stars = ~%[] in
    List.iter (fun (n, pos, len) ->
        let _, star_pos = find_adjacent_stars n pos len symbols in
        List.iter (fun ((i,j), n) ->
            stars.%{i, j} <- n :: (stars.%?{i, j} or [])
          ) star_pos
      ) num_list;
    let sum =
      stars
      |> Hashtbl.to_seq_values
      |> Seq.map (function [g1;g2] ->g1*g2 | _ -> 0)
      |> Iter.(sum seq int)
    in
    Solution.printf "%d" sum
end

let () = Solution.register_mod (module S)