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

  let is_set w i = (w lsr i) land 1 = 1

  let apply_joltage_button b jolts =
    let r = Array.copy jolts in
    for i = 0 to Array.length jolts - 1 do
      if is_set b i then
        r.(i) <- r.(i) - 1;
    done;
    r
  let max_joltage (j : int array) : int =
    let mx : int ref = ref j.(0) in
    for i = 1 to Array.length j - 1 do
      if j.(i) > !mx then mx:= j.(i)
    done;
    !mx
  let has_neg j =  Array.exists (fun x -> x < 0) j

  let bits_set v =
    let rec loop v c =
      if v = 0 then c
      else loop (v land (v-1)) (c+1)
    in loop v 0
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

  let apply_joltage_button n b jolts =
    try
      let last = Array.length jolts - 1 in
      for i = 0 to last do
        if is_set b i && jolts.(i) < n then raise_notrace Exit
      done;
      let mx = ref 0 in
      let r = Array.copy jolts in
      for i = 0 to last do
        let v = 
          if is_set b i then 
            let v = r.(i) - n in r.(i) <- v;v 
          else r.(i) 
        in
        if v > !mx then mx := v
      done;
      Some (!mx, r)
    with Exit -> None


  let sort l = List.sort (fun (_, la) (_, lb) -> List.compare_lengths la lb) l 
  let prune l j =
    let changed = ref false in
    let rec loop = function
      | [] -> []
      | (k, lk) :: ll ->
        (k, List.filter (fun b -> if is_set b j then (changed := true; false)else true) lk):: loop ll
    in
    let l' = loop l in
    if !changed then sort l'
    else l

  let dfs machine =
    let buttons = machine.buttons |> Array.to_list in
    let buttons_by_jolt =
      machine.joltages
      |> Array.mapi (fun i _ -> i, List.filter (fun b -> is_set b i) buttons )
      |> Array.to_list |> sort
    in
    let rec loop current buttons remaining n min_steps =
      if remaining + n >= min_steps then min_steps
      else
        match buttons with
        | [] -> Format.printf "FOUND => %a %d\n%!" pp current n; n
        | (j, l) :: rem_buttons ->
          match l with
            _ when  current.(j) = 0 -> loop current (prune rem_buttons j) remaining n min_steps
          | b::buttons_j ->
            let s = if buttons_j = [] then current.(j) else 1 in
            let min_steps =
              match apply_joltage_button s b current with
                None -> min_steps
              | Some (n_remaining, next) -> loop next buttons n_remaining (n+s) min_steps
            in
            loop current ((j,buttons_j)::rem_buttons) remaining n min_steps
          | _ -> min_steps


    in
    let remaining, _ = apply_joltage_button 1 0 machine.joltages |> Option.get in
    loop machine.joltages buttons_by_jolt remaining 0 max_int

  let count_steps2 machines =
    let total = List.length machines in
    let i = ref 1 in
    machines
    |> List.fold_left (fun acc c -> Format.printf "Current sum %d, computing %d/%d\n%!" acc !i total;incr i;acc + dfs c) 0

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
