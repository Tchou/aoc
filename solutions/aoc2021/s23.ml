open Utils
open Syntax

module S =
struct
  let name = Name.mk "s23"

  module Grid =
  struct
    type t = bytes array
    (* Super ugly. It's really painfull to not have break/continue/return in OCaml.
    *)
    let last_row grid = Array.length grid - 3
    let get grid (r, c) =
      grid.(r+1).$[c+1]
    let set grid (r, c) v =
      grid.(r+1).$[c+1] <- v

    let (.@()) g p = get g p
    let (.@()<-) g p v = set g p v

    let of_char c = Char.code c - 64
    let to_char a = Char.chr (a + 64)

    let is_final summary =
      let rec loop i len =
        if i >= len then true
        else
          let chr = to_char (((i - 11) mod 4) + 1) in
          if chr <> summary.[i] then false
          else loop (i+1) len
      in
      loop 11 (String.length summary)

    let door_of_amph a =
      (0, (of_char a)*2)
    let summary grid =
      let len = 11 + (Array.length grid - 3) * 4 in
      let b = Bytes.make len '.' in
      let last = last_row grid in
      Bytes.blit grid.(1) 1 b 0 11;
      let idx = ref 11 in
      for r = 1 to last do
        for c = 1 to 4 do
          let v = grid.@(r, c*2) in
          b.$[!idx] <- v;
          incr idx;
        done;
      done;
      Bytes.unsafe_to_string b

    let target_room grid a d =
      let pos = ref ~-1 in
      let invalid = ref false in
      let found = ref false in
      let _, c = door_of_amph a in
      for i = 0 to last_row grid do
        let v = grid.@(i, c) in
        if v = '.' && not !invalid && not !found then pos := i
        else if v = a then found := true
        else invalid := true
      done;
      if not !invalid && !pos > 0 then [ ((!pos,c), !pos+d) ]
      else []

    let goto_column grid c_src c_dst dir orig_d door acc_l acc_d =
      let rec loop c d acc_l acc_d =
        if c = c_dst || grid.@(0,c) <> '.' then acc_l, acc_d
        else
          let acc_l =
            if c < 2 || c > 8 || c mod 2 <> 0 then ((0,c),d)::acc_l else acc_l
          in
          let acc_d = if (0, c) = door then d else acc_d in
          loop (c+dir) (d+1) acc_l acc_d
      in
      loop c_src orig_d acc_l acc_d

    let stays_in_room grid (r, c) a =
      let rec loop r x =
        if r = 0 then true
        else if grid.@(r, c) = x then loop (r-1) x
        else if grid.@(r, c) = '.' then loop (r-1) '.'
        else false
      in
      let dr, dc = door_of_amph a in
      if c <> dc then false
      else loop (last_row grid) a
    (* todo *)
    let move_out grid (r,c) =
      assert (r > 0);
      try
        let a = grid.@(r, c) in
        if stays_in_room grid (r, c) a then raise Exit;
        for i = r-1 downto 1 do
          if grid.@(i, c) <> '.' then raise Exit
        done;
        let d = r in
        let door = door_of_amph a in
        let moves, door_dist = goto_column grid c (-1) (-1) d door [] (-1) in
        let moves, door_dist = goto_column grid c 11 1 d door moves door_dist in
        if door_dist >= 0 then
          moves @ target_room grid a door_dist
        else moves
      with Exit -> []

    let move_in grid (r, c) =
      assert (r = 0);
      try
        let a = grid.@(r, c) in
        let rdoor, cdoor = door_of_amph a in
        let () =
          if cdoor < c then
            for i = c-1 downto cdoor do
              if grid.@(0, i) <> '.' then raise Exit;
            done
          else
            for i = c +1 to cdoor do
              if grid.@(0,i) <> '.' then raise Exit;
            done
        in
        let door_dist = abs (cdoor - c) in
        target_room grid a door_dist
      with Exit -> []

    let iter grid f =
      for i = 0 to 10 do
        let pos = 0, i in
        if grid.@(pos) <> '.' then
          List.iter (f pos) (move_in grid pos);
      done;
      let last = last_row grid in
      for i = 1 to last do
        for j = 1 to 4 do
          let pos = i, j * 2 in
          if grid.@(pos) <> '.' then
            List.iter (f pos) (move_out grid pos);
        done;
      done
    let cost = function
      | 'A' -> 1
      | 'B' -> 10
      | 'C' -> 100
      | 'D' -> 1000
      | _ -> assert false
    let pp fmt g =
      Array.iter (fun b -> Ansi.fprintf fmt "%s\n" (Bytes.to_string b)) g
  end

  let load_input part1 =
    let grid =
      Input.fold_lines (fun acc s -> (Bytes.of_string s)::acc) []
      |> List.rev
    in
    let grid = if part1 then grid else
        match grid with
          [l0;l1;l2;l3;l4] ->
          [l0;l1;l2;
           Bytes.of_string "  #D#C#B#A#  ";
           Bytes.of_string "  #D#B#A#C#  ";
           l3;l4]
        | _ -> assert false
    in
    Array.of_list grid
  let omin i = function Some j -> Some (min i j) | None -> Some i
  let oadd i = function None -> None | Some j -> Some (i+j)
  let (++) a b = if a = max_int || b = max_int then max_int else a + b

  let pp_path fmt l =
    let l = List.rev l in
    List.iter (fun (cr,(r,c), (r',c'), d, cst) ->
        Ansi.fprintf fmt "MOVE: %c FROM (%d,%d) to (%d,%d) (%d - %d)\n" cr
          r c r' c'
          d cst
      )l;
    Ansi.fprintf fmt "\n"

  let search_dfs grid =
    let cache = ~%[] in
    let rec loop total =
      let sum = Grid.summary grid in
      if Grid.is_final sum then total
      else
        try total ++ cache.%{sum} with Not_found ->
          let acost = ref max_int in
          Grid.iter grid (fun p (p', d)->
              let open Grid in
              let c = grid.@(p) in
              grid.@(p) <- '.';
              grid.@(p') <- c;
              let cst = d * cost c in
              acost := min !acost ((loop (total+cst)) ++ (-total));
              grid.@(p') <- '.';
              grid.@(p) <- c);
          cache.%{sum} <- !acost;
          total ++ !acost
    in
    loop 0
  let solve part1 =
    load_input part1
    |> search_dfs
    |> Solution.printf "%d"
  let solve_part1 () = solve true
  let solve_part2 () = solve false
end
let () = Solution.register_mod (module S)