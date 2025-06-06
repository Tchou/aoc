open Utils
open Syntax

module S =
struct
  let name = Name.mk "s10"

  type dest = Bot | Output
  type output = (int, int list) Hashtbl.t
  type bot_spec = { id : int; low : dest * int; high : dest * int ; mutable values : int list }
  type bots = (int, bot_spec) Hashtbl.t


  let dest_of_string = function "bot" -> Bot | _ -> Output
  let read_input () : bots =
    let open Scanf in
    let lvalues = ref [] in
    let bots = ~%[] in
    Input.fold_lines (fun () s ->
        if s.[0] = 'v' then
          sscanf s "value %d goes to bot %d" 
            (fun v id -> lvalues := (id, v)::!lvalues )
        else
          sscanf s "bot %d gives low to %[^ ] %d and high to %[^ ] %d"
            (fun id lo lid ho hid ->
               bots.%{id} <-
                 { id; low = (dest_of_string lo, lid); high=(dest_of_string ho, hid); values = []}
            )
      ) ();
    !lvalues
    |> List.iter (fun (id, v) -> 
        let b = bots.%{id} in
        b.values <- v :: b.values;
      );
    bots


  let has2 b = match b.values with
      [] | [ _ ] -> false
    | _ -> true

  let get_values l =
    let rec loop l amin amax al =
      match l with
        [] -> amin, amax, al
      | v :: ll  ->
        if v < amin then loop ll v amax (amin :: al)
        else if v > amax then loop ll amin v (amax :: al)
        else loop ll amin amax (v :: al)
    in
    match l with
      vmin::vmax :: ll ->
      if vmin < vmax then loop ll vmin vmax []
      else loop ll vmax vmin []
    | _ -> assert false

  let move_value bots outputs v (d, id) =
    match d with
      Output -> outputs.%{id} <- v :: (outputs.%?{id} or [])
    | Bot -> 
      let b = bots.%{id} in
      b.values <- v :: b.values

  let eval_bots part1 v1 v2 bots =
    let outputs = ~%[] in
    let v1, v2 = if v1 > v2 then v2, v1 else v1, v2 in
    let lbots = bots |> Hashtbl.to_seq_values |> List.of_seq in
    let exception Found of int in
    let rec loop () =
      let _ =
        lbots |> List.find (fun b ->
            if has2 b then begin
              let vlow, vhigh, nvalues = get_values b.values in
              if part1 && vlow = v1 && vhigh = v2 then raise (Found b.id);
              b.values <- nvalues;
              move_value bots outputs vlow b.low;
              move_value bots outputs vhigh b.high;
              true
            end else false
          )
      in
      loop ()
    in
    try loop () with 
      Not_found ->
        let l = outputs.%{0} @ outputs.%{1} @ outputs.%{2} in
        Iter.(prod list (module Int) l)
    | Found id -> id

  let solve_part1 () =
    let bots = read_input () in
    let id = eval_bots true 17 61  bots in
    Solution.printf "%d" id

  let solve_part2 () =
    let bots = read_input () in
    let id = eval_bots false 17 61  bots in
    Solution.printf "%d" id

end

let () = Solution.register_mod (module S)