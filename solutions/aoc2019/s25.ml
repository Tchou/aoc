open Utils
module S =
struct
  let name = Name.mk "s25"

  let commands = [
    (* Found by exploring manually *)
    "west";
    "take semiconductor";
    "west";
    "take planetoid";
    "west";
    "take food ration";
    "west";
    "take fixed point";
    "west";
    "take klein bottle";
    "east";
    "south";
    "west";
    "take weather machine";
    "east";
    "north";
    "east";
    "east";
    "south";
    "south";
    "south";
    "take pointer";
    "north";
    "north";
    "east";
    "take coin";
    "east";
    "north";
    "east";
  ]
  let items =
    commands
    |> List.filter_map (fun s -> if String.starts_with ~prefix:"take " s then
                           Some (String.sub s 5 (String.length s - 5))
                         else None
                       )

  let commands = commands @ List.map (fun s -> "drop " ^s) items


  let display fmt state =
    let open Intcode in
    Queue.iter (fun c -> Format.fprintf fmt "%c" (Char.chr c)) state.stdout;
    Queue.clear state.stdout;
    Format.fprintf fmt "%!"

  let enter command state =
    let open Intcode in
    String.iter (fun c -> Queue.push (Char.code c) state.stdin) command;
    Queue.push 10 state.stdin

  let nethack code =
    let exception Found of Intcode.state in
    let state = Intcode.make_state code in
    let rec auto_play com state k =
      match com, Intcode.eval state with
        order::ccom, `need_input ->
        Queue.clear state.stdout;
        enter order state;
        auto_play ccom state k
      | _, `full_output -> assert false
      | _, `halt -> raise (Found state)
      | [], `need_input ->
        k state
    in
    let rec try_items state =
      (Queue.clear state.Intcode.stdout);
      items
      |> Comb.powerset
      |> List.of_seq
      |> List.iter (fun litems ->
          let orders = (List.map (fun s -> "take " ^ s) litems) @ ["north"]   in
          auto_play orders (Intcode.copy_state state) (fun state ->
              let s = Format.asprintf "%a" display (Intcode.copy_state state) in ()
            )
        )
    in
    try
      auto_play commands state try_items;"FAILED"
    with Found state -> Format.asprintf "%a" display state
  let solve_part1 () =
    let code = Intcode.read () in
    let message = nethack code in
    Solution.printf "%s" message


  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)