open Utils
open Syntax
module S =
struct
  let name = Name.mk "s16"
  type interval = { name : string;
                    range1 : (int*int);
                    range2 : (int*int) }
  let read_input () =
    let intervals =
      InputUntil.fold_lines (fun acc line ->
          if line = "" then (false, acc) else
            Scanf.sscanf line "%[^:]: %d-%d or %d-%d"
              (fun name i1 i2 i3 i4 ->
                 (true, {name; range1=(i1, i2); range2=(i3, i4)}:: acc)))
        []
    in
    let my_ticket =
      Input.read_line () |> ignore;
      Input.read_line ()
      |> String.split_on_char ','
      |> List.map int_of_string
    in
    let other_tickets =
      Input.read_line () |> ignore;
      Input.read_line () |> ignore;
      Input.list_fields ',' (List.map int_of_string)
    in
    intervals, my_ticket, other_tickets

  let in_range v {range1; range2;_} =
    ((fst range1) <= v && v <= (snd range1))
    || ((fst range2) <= v && v <= (snd range2))


  let contains interval v = List.exists (in_range v) interval
  let solve_part1 () =
    let intervals, _, tickets = read_input () in
    let n =
      let open Iter in
      tickets
      |> list
      |> map (fun l ->
          l
          |> list
          |> filter (Fun.negate (contains intervals))
          |> sum int)
      |> sum int
    in
    Solution.printf "%d" n


  let is_compatible_interval all_tickets i inter =
    let rec loop k =
      k >= Array.length all_tickets ||
      (in_range all_tickets.(k).(i) inter && loop (k+1))
    in
    loop 0

  exception Found of interval array
  let find_arrangement all_tickets intervals =
    let dummy = { name = ""; range1 = (-1,-1); range2=(-1,-1) } in
    let len_intervals = List.length intervals in
    let arr = Array.make len_intervals dummy in
    let compatible_intervals = Array.init len_intervals (fun i ->
        List.filter (is_compatible_interval all_tickets i) intervals)
    in
    (* Heuristic, sort to start backtracking from those with most choices to
       those with less choices *)
    let order =
      compatible_intervals
      |> Array.to_list
      |> List.mapi (fun i l -> (List.length l, i))
      |> List.sort Compare.fst
      |> List.map snd
    in
    let used = ~%[] in
    let rec loop = function
      | [] -> raise (Found (arr))
      | i :: todo ->
        compatible_intervals.(i)
        |> List.iter (fun inter ->
            if (not (used %? inter.name)) then begin
              used.%{inter.name} <- ();
              arr.(i) <- inter;
              loop todo;
              used %- inter.name
            end)
    in
    try
      loop order; None
    with Found a -> Some a

  let solve_part2 () =
    let intervals, my_ticket, other_tickets = read_input () in
    let valid_tickets =
      other_tickets
      |> List.filter (List.for_all (contains intervals))
    in
    let all_tickets =
      (my_ticket :: valid_tickets)
      |> List.map Array.of_list
      |> Array.of_list
    in
    let arr = find_arrangement all_tickets intervals or [||] in
    let my_ticket = Array.of_list my_ticket in
    let n = ref 1 in
    let () = Array.iteri (fun i inter ->
        if String.starts_with ~prefix:"departure" inter.name then
          n := !n * my_ticket.(i)
      ) arr
    in
    Solution.printf "%d" !n
end

let () = Solution.register_mod (module S)