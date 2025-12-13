open Utils
module S =
struct

  type t = { lights : int;
             length : int; (* size of lights and joltages *)
             buttons : string array;
             joltages : int array;
           }

  let set_bit w i = w lor (1 lsl i)
  let read_lights s =
    let length = String.length s - 2 in
    let acc = ref 0 in
    for i = 1 to length do
      if s.[i] = '#' then 
        acc := set_bit !acc (i-1);
    done;
    !acc, length

  let read_buttons len l =
    let rec loop l acc =
      match l with
        [] -> assert false
      | [ j ] -> (acc|> List.rev |> Array.of_list), j
      | b :: ll ->
        loop ll 
          ((String.sub b 1 (String.length b - 2)
            |> String.split_on_char ','
            |> List.map (fun s -> 
                int_of_string s
                |> Char.chr)
            |> String.implode)::acc)

    in
    loop l []

  let read_joltages s =
    String.sub s 1 (String.length s - 2)
    |> String.split_on_char ','
    |> List.map int_of_string
    |> Array.of_list

  let read_line l =
    let lights, length = read_lights (List.hd l) in 
    let buttons, j_str = read_buttons length (List.tl l) in
    let joltages = read_joltages j_str in
    { lights; length; buttons; joltages}

  let read_input () =
    Input.list_fields ' ' read_line

  let apply_button lights button =
    (* 0 0 -> 0
       0 1 -> 1
       1 0 -> 1
       1 1 -> 0

       String.fold_left (fun acc c -> 
        acc lxor (1 lsl (Char.code c))) lights button
    *)
    let res = ref lights in
    for i = 0 to String.length button - 1 do
      res := !res lxor  (1 lsl (Char.code (String.unsafe_get button i)));
    done;
    !res
  let find_target machine =
    let open Syntax in
    let queue = Queue.create () in
    let visited = ~%[0, ()] in
    Queue.add (0,0) queue;
    let rec loop () =
      if Queue.is_empty queue then assert false;
      let current, steps = Queue.take queue in
      if machine.lights = current then steps
      else
        begin
          Array.iter (fun b -> 
              let n = apply_button current b in
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

  let pp fmt a = 
    Format.fprintf fmt "{";
    Array.iter (Format.fprintf fmt "%d ") a;
    Format.fprintf fmt "}"

  let pp_b fmt b =
    Format.fprintf fmt "(";
    for i = 0 to 10 do
      if is_set b i then Format.fprintf fmt "%d " i;
    done;
    Format.fprintf fmt ")"

  let apply_joltage_button ?(n=1) (b:string) jolts =

    let r = Array.copy jolts in
    for i = 0 to String.length b - 1 do
      let idx = Char.code (String.unsafe_get b i) in
      r.(idx) <- r.(idx) - n;
    done;
    r

  let diff_zeroes prev next =
    let zeroes = ref [] in
    for i = 0 to Array.length prev - 1 do
      if next.(i) = 0 && prev.(i) <> 0 then zeroes := i :: !zeroes;
    done;
    !zeroes

  let has_joltage button pos =
    String.contains button (Char.chr pos)

  let dfs machine =
    let min_steps = ref max_int in
    let buttons = machine.buttons |> Array.to_list in
    let sort = List.sort (fun (_, la) (_, lb) -> Int.compare (List.length la) (List.length lb) ) in
    let buttons_by_jolt = 
      machine.joltages 
      |> Array.mapi (fun i _ -> i, List.filter (fun b -> has_joltage b i) buttons )
      |> Array.to_list |> sort
    in
    let prune l j = List.map (fun (k, lk) -> (k, List.filter (fun b -> not (has_joltage b j) ) lk)) l |> sort in
    let rec loop current buttons n =
      let remaining = max_joltage current in
      if remaining = 0 then (Format.printf "FOUND => %a %d\n%!" pp current n;min_steps := n; n)
      else 
      if remaining + n < !min_steps then
        let res =
          match buttons with
          (* current joltage is at 0, remove all buttons that inpact it *)
          | (j, _) :: rem_buttons when current.(j) = 0 -> loop current (prune rem_buttons j) n
          | (j, []) :: _ -> max_int (* current joltage is non zero but no more buttons, impossible *)
         
          | (j, (b::buttons_j)) :: rem_buttons ->
            let s = if buttons_j = [] then current.(j) else 1 in
            let next = apply_joltage_button ~n:s b current in
            let res1 = if has_neg next then max_int else
                let new_buttons = match diff_zeroes current next with
                    [] -> buttons
                  | l -> List.filter_map (fun (k, lk) ->
                      let llk = List.filter (fun b -> not (List.exists (has_joltage b) l)) lk in
                      if llk = [] then None else Some (k, llk) 
                    ) buttons |> sort
                in loop next new_buttons (n+s) 
            in
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
    |> List.fold_left (fun acc c -> Format.printf "Current sum %d, computing %d/%d = %a\n%!" acc !i total pp c.joltages;incr i;acc + dfs c) 0

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