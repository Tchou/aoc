open Utils
module S =
struct

  type t = { target : int;
             length : int;
             len_mask : int;
             buttons : int array;
             joltages : int array;
           }

  let set_bit w i = w lor (1 lsl i)
  let read_target s =
    let length = String.length s - 2 in
    let acc = ref 0 in
    for i = 1 to length do
      if s.[i] = '#' then
        acc := set_bit !acc (i-1);
    done;
    !acc, length

  let read_buttons l =
    let rec loop l acc =
      match l with
        [] -> assert false
      | [ j ] -> (acc|> List.rev |> Array.of_list), j
      | b :: ll ->
        loop ll
          ((String.sub b 1 (String.length b - 2)
            |> String.split_on_char ','
            |> List.fold_left (fun acc i -> set_bit acc (int_of_string i)) 0)::acc)
    in
    loop l []

  let read_joltages s =
    String.sub s 1 (String.length s - 2)
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list

  let read_line l =
    let target, length = read_target (List.hd l) in
    let len_mask = (1 lsl length) - 1 in
    let buttons, j_str = read_buttons (List.tl l) in
    let joltages = read_joltages j_str in
    { target; length; len_mask; buttons; joltages}

  let read_input () =
    Input.list_fields ' ' read_line

  let apply_button lights mask button =
    (lights lxor button) land mask

  let find_target machine =
    let open Syntax in
    let queue = Queue.create () in
    let visited = ~%[0, ()] in
    Queue.add (0,0) queue;
    let rec loop () =
      if Queue.is_empty queue then assert false;
      let current, steps = Queue.take queue in
      if machine.target = current then steps
      else
        begin
          Array.iter (fun b ->
              let n = apply_button current machine.len_mask b in
              if not (visited %? n) then begin
                visited.%{n} <- ();
                Queue.add (n, steps+1) queue;
              end) machine.buttons;
          loop ()
        end
    in
    loop ()

  let count_steps machines =
    machines
    |> List.fold_left (fun acc c -> acc + find_target c) 0

  let[@inline] is_set w i = (w lsr i) land 1 = 1

  let pp fmt a =
    Format.fprintf fmt "[";
    Array.iter (Format.fprintf fmt "%d ") a;
    Format.fprintf fmt "]"

  let pp_b fmt b =
    Format.fprintf fmt "(";
    for i = 0 to 10 do
      if is_set b i then Format.fprintf fmt "%d " i;
    done;
    Format.fprintf fmt ")"

  let failure = 10_000_000
  let apply_joltage_button n b jolts =
    let rec loop i len mx =
      if i < len then
        let set = is_set b i in

        if set && jolts.(i) < n then failure
        else
          let v = if set then jolts.(i) - n else jolts.(i) in
          let mx = if v > mx then v else mx in
          loop (i+1) len mx
      else
        (for i = 0 to len - 1 do
           if is_set b i then jolts.(i) <- jolts.(i) - n;
         done;
         mx)
    in
    loop 0 (Array.length jolts) 0


  let sort =
    let cmp (_, la) (_, lb) = List.compare_lengths la lb in
    List.sort cmp
  let prune jolt l j =
    let rec loop = function
      | [] -> []
      | (k, lk) :: ll ->
        match List.filter (fun b -> not (is_set b j)) lk with
          [] -> if jolt.(k) = 0 then loop ll
          else raise_notrace Exit
        | nlk -> (k, nlk) :: loop ll
    in
    try
      Some (sort (loop l))
    with Exit -> None
  let dfs machine =
    let buttons = machine.buttons |> Array.to_list in
    let buttons_by_jolt =
      machine.joltages
      |> Array.mapi (fun i _ -> i, List.filter (fun b -> is_set b i) buttons )
      |> Array.to_list |> sort
    in
    let rec loop current buttons max_current n min_steps =
      if max_current + n >= min_steps then min_steps
      else  match buttons with
        | [] -> n
        | (j, l) :: rem_buttons ->
          let current_j = current.(j) in
          if current_j = 0 then
            match prune current rem_buttons j with
              None -> min_steps
            | Some rem_buttons -> loop current rem_buttons max_current n min_steps
          else match l with
            | [] -> min_steps
            | b :: buttons_j ->
              let s = if buttons_j == [] then current_j else 1 in
              let min_steps =
                let max_next = apply_joltage_button s b current in
                let res = loop current buttons max_next (n+s) min_steps in
                if max_next < failure then for i = 0 to Array.length current - 1 do
                    if is_set b i then current.(i) <- current.(i) + s;
                  done;
                res
              in
              loop current ((j,buttons_j)::rem_buttons) max_current n min_steps
    in
    let max_joltage= apply_joltage_button 1 0 machine.joltages in
    loop machine.joltages buttons_by_jolt max_joltage 0 failure

  let count_steps2 machines =
    machines
    |> List.fold_left (fun acc c -> acc + dfs c) 0

  let name = Name.mk "s10"
  let solve_part1 () =
    let machines = read_input () in
    let n = count_steps machines in
    Solution.printf "%d" n
  let solve_part2 () =
    let machines = read_input () in
    let n = count_steps2 machines  in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)
