open Utils
open Syntax

module S =
struct
  let name = Name.mk "s14"
  let load_input () =
    Input.fold_lines
      (fun acc l -> (Bytes.of_string l) :: acc) []
    |> List.rev |> Array.of_list

  let pp fmt =
    Array.iter (fun b ->
        Ansi.fprintf fmt "%s\n" (Bytes.to_string b))

  let rotate_left_into src dst =
    let slen = Array.length src in
    let dlen = Array.length dst in
    for i = 0 to slen - 1 do
      for j = 0 to dlen - 1 do
        dst.(j).$[slen - i - 1] <- src.(i).$[j]
      done;
    done

  let alloc_rotate src =
    Array.init (Bytes.length src.(0))
      (fun _ -> Bytes.make (Array.length src) '\x00')

  let score arr =
    let alen = Array.length arr in
    let total = ref 0 in
    for i = 0 to alen - 1 do
      Bytes.iter (function 
          | 'O' -> total:= !total + (alen-i)
          | _ -> ()
        ) arr.(i);
    done;
    !total

  let tilt_north ?(modify=true) arr =
    let arr_len = Array.length arr in
    let row_len = Bytes.length arr.(0) in
    let weights =
      Array.make row_len arr_len
    in
    for i = 0 to arr_len - 1 do
      Bytes.iteri (fun j c ->
          match c with
          | 'O' ->
            let new_idx = arr_len - weights.(j) in
            if new_idx < arr_len && new_idx != i then begin
              arr.(new_idx).$[j] <- 'O';
              arr.(i).$[j] <- '.';
            end;
            weights.(j) <- weights.(j) - 1
          | '#' -> weights.(j) <- (arr_len - i - 1)
          | '.' -> ()
          | _ -> assert false) arr.(i)
    done
  let copy = Array.map (Bytes.copy)
  let do_cycle grid_v grid_h =
    ignore @@ tilt_north grid_v;
    rotate_left_into grid_v grid_h;
    ignore @@ tilt_north grid_h;
    rotate_left_into grid_h grid_v;
    ignore @@ tilt_north grid_v;
    rotate_left_into grid_v grid_h;
    ignore @@ tilt_north grid_h;
    rotate_left_into grid_h grid_v


  let blit src dst =
    let len = Bytes.length src.(0) in
    for i = 0 to Array.length src - 1 do
      Bytes.blit src.(i) 0 dst.(i) 0 len;
    done

  let find_cycle grid_v =
    let tortoise = copy grid_v in
    let hare = copy grid_v in
    let grid_h = alloc_rotate grid_v in
    do_cycle hare grid_h;
    let rec loop power period =
      if tortoise = hare then period else
        let power, period =
          if power = period then begin
            blit hare tortoise;
            power * 2, 0
          end else power, period
        in
        do_cycle hare grid_h;
        loop power (period+1)
    in
    let period = loop 1 1 in
    blit grid_v tortoise;
    blit grid_v hare;
    for _ = 0 to period - 1 do
      do_cycle hare grid_h;
    done;
    let rec loop start =
      if tortoise <> hare then begin
        do_cycle tortoise grid_h;
        do_cycle hare grid_h;
        loop (start+1)
      end else start
    in
    loop 1, period

  let solve_part1 () =
    let grid = load_input () in
    tilt_north grid;
    score grid
    |> Solution.printf "%d"

let solve_part2 () =
    let grid_v = load_input () in
    let start, period = find_cycle grid_v in
    let rem = (1_000_000_000 - start) mod period in
    let grid_h = alloc_rotate grid_v in
    for _ = 1 to start + rem do
      do_cycle grid_v grid_h;
    done;
    Solution.printf "%d" (score grid_v)

end

let () = Solution.register_mod (module S)