open Utils
open Syntax

module S =
struct
  let name = Name.mk "s21"

  let swap_position i j s =
    let tmp = s.$[i] in
    s.$[i] <- s.$[j];
    s.$[j] <- tmp

  let swap_letter x y s =
    for i = 0 to Bytes.length s - 1 do
      let c = s.$[i] in
      if c = x then s.$[i] <- y
      else if c = y then s.$[i] <- x
    done

  let reverse_from i len s =
    let o = i + len - 1 in
    for k = 0 to (len lsr 1) - 1 do
      let tmp = s.$[i+k] in
      s.$[i+k] <- s.$[o-k];
      s.$[o-k] <- tmp;
    done


  let rotate_right n s =
    let len = Bytes.length s in
    let n = ((n mod len ) + len) mod len in
    reverse_from (len - n) n s;
    reverse_from 0 (len - n) s;
    reverse_from 0 len s
  let rotate_left n s = rotate_right (Bytes.length s - n) s

  let rotate_based x s =
    let i = Bytes.index s x in
    let d = i + 1 + (if i >= 4 then 1 else 0) in
    rotate_right d s

  let reverse_position i j s = reverse_from i (j-i+1) s

  let move i j s =
    let tmp = s.$[i] in
    let len = Bytes.length s in
    for k = i+1 to len - 1 do
      s.$[k-1] <- s.$[k];
    done;
    for k = len-2 downto j do
      s.$[k+1] <- s.$[k];
    done;
    s.$[j] <- tmp


  type op =
      Swap_position of int * int
    | Swap_letter of char * char
    | Rotate_left of int
    | Rotate_right of int
    | Rotate_based of char
    | Reverse_position of int * int
    | Move_position of int * int

  let int = int_of_string
  let read_input () =
    Input.list_fields ' ' (
      function
        [ "swap"; "position"; x; "with"; "position"; y ] -> Swap_position (int x, int y)
      | [ "swap"; "letter"; x; "with"; "letter"; y ] -> Swap_letter (x.[0], y.[0])
      | [ "rotate"; "left"; x; "steps"|"step" ] -> Rotate_left (int x)
      | [ "rotate"; "right"; x; "steps"|  "step" ] -> Rotate_right (int x)
      | [ "rotate"; "based"; "on"; "position"; "of"; "letter"; x ] -> Rotate_based x.[0]
      | [ "reverse"; "positions"; x; "through"; y ] -> Reverse_position (int x, int y)
      | [ "move"; "position"; x;"to"; "position"; y] -> Move_position (int x, int y)
      | l -> failwith (String.concat " " ("Parse error:"::l))
    )

  let eval_op op b =
    match op with
      Swap_position (i, j) -> swap_position i j b
    | Swap_letter (x, y) -> swap_letter x y b
    | Rotate_left n -> rotate_left n b
    | Rotate_right n -> rotate_right n b
    | Rotate_based x -> rotate_based x b
    | Reverse_position (i, j) -> reverse_position i j b
    | Move_position (i, j) -> move i j b

  let invert_op s = function
      Swap_position (i, j) -> Swap_position (i, j)
    | Swap_letter (x, y) -> Swap_letter (x, y)
    | Rotate_left n -> Rotate_right n
    | Rotate_right n -> Rotate_left n
    | Reverse_position (i, j) -> Reverse_position (i, j)
    | Move_position (i, j) -> Move_position (j, i)
    | Rotate_based x ->
      let left_offset = [| 9; 1; 6; 2; 7; 3; 8; 4|] in
      Rotate_left (left_offset.(Bytes.index s x))
      (*
      rotate based on position of letter X means that the whole string should
      be rotated to the right based on the index of letter X (counting from 0)
      as determined before this instruction does any rotations. Once the index
      is determined, rotate the string to the right one time, plus a number of
      times equal to that index, plus one additional time if the index was at least 4.

      Original X      Right rotations      New Position              Reverse
      position
       0                    1                   1                      L 1
       1                    2                   3                      L 2
       2                    3                   5                      L 3
       3                    4                   7                      L 4
       4                    6                   10 % 8 = 2             
       5                    7                   12 % 8 = 4
       6                    8                   14 % 8 = 6
       7                    9                   16 % 8 = 0
      ""


        *)


  let apply s l =
    let b = Bytes.of_string s in
    List.iter (fun op -> eval_op op b) l;
    Bytes.to_string b

  let reverse s l =
    let b = Bytes.of_string s in
    List.iter (fun op -> eval_op (invert_op b op) b) (List.rev l);
    Bytes.to_string b

  let solve_part1 () =
    let l = read_input () in
    let s = apply "abcdefgh" l in
    Solution.printf "%s" s


  let solve_part2 () = 
    let l = read_input () in
    let s = reverse "fbgdceah" l in
    Solution.printf "%s" s

end

let () = Solution.register_mod (module S)