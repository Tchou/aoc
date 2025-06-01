open Utils
module S =
struct
  let name = Name.mk "s08"


  let display = Array.make_matrix 6 50 false

  type instr = Rect | Row | Col

  let read_input () =
    Input.list_lines (fun s ->
        let open Scanf in
        if String.starts_with ~prefix:"rect" s then sscanf s "rect %dx%d" (fun i j -> (Rect, (i, j)))
        else if String.starts_with ~prefix:"rotate r" s then sscanf s "rotate row y=%d by %d" (fun i j -> (Row, (i, j)))
        else sscanf s "rotate column x=%d by %d" (fun i j -> (Col, (i, j)))
      )

  let mk_rect w h =
    for j = 0 to h - 1 do
      for i = 0 to w - 1 do
        display.(j).(i) <- true
      done
    done
  let rotate_col =
    let tmp  = Array.make 6 false in
    fun c n ->
      for i = 0 to 5 do
        tmp.(i) <- display.(i).(c);
      done;
      for i = 0 to 5 do
        display.((i+n) mod 6).(c) <- tmp.(i);
      done
  let rotate_row =
    let tmp  = Array.make 50 false in
    fun r n ->
      for i = 0 to 49 do
        tmp.(i) <- display.(r).(i);
      done;
      for i = 0 to 49 do
        display.(r).((i+n) mod 50) <- tmp.(i);
      done

  let show () =
    let buf = Buffer.create 16 in
    let fmt = Format.formatter_of_buffer buf in
    for i = 0 to 5 do
      for j = 0 to 49 do
        Format.fprintf fmt "%s" (if display.(i).(j) then "\u{2588}" else " ")
      done;
      Ansi.fprintf fmt "\n"
    done;
    Format.pp_print_flush fmt ();
    Buffer.contents buf

  let eval l =
    List.iter (fun (i, (x, y)) ->
        match i with
          Rect -> mk_rect x y
        | Col -> rotate_col x y
        | Row -> rotate_row x y

      ) l

  let count_on () =
    let c = ref 0 in
    Array.iter (Array.iter (fun b -> if b then incr c)) display;
    !c
  let solve_part1 () =
    let l = read_input () in
    eval l;
    let c = count_on () in
    Solution.printf "%d" c


  let solve_part2 () =
    let l = read_input () in
    eval l;
    let s = show () in
    Solution.printf "%s" s

end

let () = Solution.register_mod (module S)