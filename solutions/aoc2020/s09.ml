open Utils
open Syntax
module S =
struct
  let name = Name.mk "s09"

  let load_input () =
    Input.fold_scan "%d" (fun acc i -> i::acc) []
    |> List.rev |> Array.of_list

  let rec find_in_range tab i j f =
    if i >= j then None
    else
    if f tab.(i) then Some (i, tab.(i))
    else find_in_range tab (i+1) j f
  let find_number prefix tab =
    let mem = ~%[] in
    for i = 0 to prefix - 1 do
      mem.%{tab.(i)} <- ();
    done;
    let rec loop i =
      if i >= Array.length tab then assert false
      else
        let v = tab.(i) in
        match find_in_range tab (i-prefix) i (fun w -> mem %? (v-w)) with
        | None -> i, v
        | Some _ ->
          mem %- tab.(i-prefix);
          mem.%{v} <- ();
          loop (i+1)
    in loop prefix

  let find_subset_before tab i v =
    let rec loop last first acc =
      if acc = 0 then first, last
      else
        let acc' = acc - tab.(first - 1) in
        if acc' >= 0 then loop last (first-1) acc' 
        else
          loop (last-1) (first) (acc+tab.(last))
    in
    let rec find_last tab i v =
      if tab.(i) + tab.(i-1) <= v then i
      else find_last tab (i-1) v
    in
    let i = find_last tab (i-1) v in
    let first, last = loop i (i-1) (v - (tab.(i) + tab.(i-1))) in
    let vmin = ref max_int in
    let vmax = ref min_int in
    for i = first to last do
      vmin := min !vmin tab.(i);
      vmax := max !vmax tab.(i);
    done;
    !vmin + !vmax


  let solve_part1 () =
    load_input ()
    |> find_number 25
    |> snd
    |> Ansi.printf "%d\n"
  let solve_part2 () =
    let tab = load_input () in
    let i, v = find_number 25 tab in
    find_subset_before tab i v
    |> Ansi.printf "%d\n"
end

let () = Solution.register_mod (module S)