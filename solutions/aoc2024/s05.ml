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
      Input.list_fields ',' (List.map int_of_string)
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

  let sort_ord order update =
    match is_sorted order update with
      Some _ -> None
    | None ->
      let compare x y =
        if x = y then 0
        else if order %? (x, y) then -1 else 1
      in
      Some (List.sort compare update)

  let sum order sort updates =
    Iter2.(updates
           |> list
           |> map (fun l ->
               match sort_ord order l with
                 Some l -> List.nth l (List.length l /2)
               | None -> 0)
           |> sum int)

  let solve sort =
    let order, updates = read_input () in
    let n = sum order sort updates in
    Solution.printf "%d" n

  let solve_part1 () = solve is_sorted
  let solve_part2 () = solve sort_ord
end

let () = Solution.register_mod (module S)