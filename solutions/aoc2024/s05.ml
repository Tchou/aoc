open Utils
open Syntax
module S =
struct
  let name = Name.mk "s05"
  let read_input () =
    let order = ~%[] in
    InputUntil.fold_lines (fun () l ->
        if l = "" then (false, ())
        else
          Scanf.sscanf l "%d|%d" (fun a b ->true, order.%{a,b}<-())) ();
    let updates =
      Input.fold_fields ',' (fun acc l ->
          (List.map int_of_string l)::acc) []
      |> List.rev
    in
    order, updates

  let is_sorted order update =
    let rec loop l =
      match l with
      | [] | [ _ ] -> Some (update)
      | u1 :: (u2 :: _ as l) ->
        if order %? (u1, u2) then loop l 
        else None
    in
    loop update

  let bubble_sort order update = (* Bubble sort ! *)
    let swaped = ref false in
    let rec swap l =
      match l with
        [] | [ _ ] -> l, false
      | u1 :: (u2 :: ll) ->
        if order %? (u1, u2) then
          let l, s = swap (u2::ll) in u1::l, s
        else
          u2::u1::ll, true
    in
    let rec loop l acc =
      let l, swaped = swap l in
      if swaped then loop l (acc+1)
      else l, acc
    in
    match loop update 1 with
      _, 1 -> None
    | l, _ -> Some l

  let sum order sort updates =
    List.fold_left (Agg.Left.sum (fun l ->
        match sort order l with
          Some l -> List.nth l (List.length l /2)
        | None -> 0)) 0 updates

  let solve sort =
    let order, updates = read_input () in
    let n = sum order sort updates in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part1 () = solve is_sorted
  let solve_part2 () = solve bubble_sort
end

let () = Solution.register_mod (module S)