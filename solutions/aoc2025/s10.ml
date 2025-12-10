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

  let apply_joltage_button ?(n=1) b jolts =

    let r = Array.copy jolts in
    for i = 0 to Array.length jolts - 1 do
      if is_set b i then
        r.(i) <- r.(i) - n;
    done;
    r

  let dfs machine =
    let min_steps = ref max_int in
    let buttons = machine.buttons |> Array.to_list in
    let sort = List.sort (fun (_, la) (_, lb) -> Int.compare (List.length la) (List.length lb) ) in
    let buttons_by_jolt = 
      machine.joltages 
      |> Array.mapi (fun i _ -> i, List.filter (fun b -> is_set b i) buttons )
      |> Array.to_list |> sort
    in
    let prune l j = List.map (fun (k, lk) -> (k, List.filter (fun b -> not (is_set b j) ) lk)) l |> sort in
    let rec loop current buttons n =
      if buttons == [] then (Format.printf "FOUND => %a %d\n%!" pp current n;min_steps := n; n)
      else 
        let remaining = max_joltage current in
        if remaining + n < !min_steps then
          let res =
            match buttons with
            | (j, _) :: rem_buttons when current.(j) = 0 -> loop current (prune rem_buttons j) n
            | (j, (b::buttons_j)) :: rem_buttons ->
              let next = apply_joltage_button b current in
              let res1 = if not (has_neg next) then loop next buttons (n+1) else max_int in
              min res1 (loop current ((j,buttons_j)::rem_buttons) n)
            | _ -> max_int
          in
          res
        else max_int
    in
    loop machine.joltages buttons_by_jolt 0 |> ignore;
    !min_steps

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