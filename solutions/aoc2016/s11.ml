open Utils
open Syntax

module S =
struct
  let name = Name.mk "s11"

  module IDevSet = struct
    type t = int
    let empty = 0
    let chip_len = 24
    let first_chip = 1 lsl chip_len
    let gen_mask = first_chip - 1
    let is_empty set = set == 0
    let is_chip d = d >= first_chip
    let is_valid set =
      (* A set is valid iff *)
      (* Or every chip is coupled to its generator *)
      let chips = set lsr chip_len in
      let gens = set land gen_mask in
      (((lnot chips) lor gens)) land gen_mask == gen_mask
    let add d set = 
      set lor d
    let remove d set = 
      set land (lnot d)

    let union set1 set2 = set1 lor set2
    let diff set1 set2 = set1 land (lnot set2)
    let to_seq set =
      let rec f n () =
        if n == 0 then Seq.Nil
        else 
          let n' = n land (n - 1) in
          let d = n - n' in
          Seq.Cons (d, f n')
      in
      f set
    let iter f set = Seq.iter f (to_seq set)

    let iter12 f set =
      let rec loop2 b n m =
        if n != 0 then begin
          let n' = n land (n - 1) in
          let d = n - n' in
          f (d lor b);
          loop2 b n' m
        end
        else loop1 m 
      and loop1 n =
        if n != 0 then begin
          let n' = n land (n - 1) in
          let d = n - n' in
          f d;
          loop2 d n' n'
        end
      in
      loop1 set

  end

  type configuration = IDevSet.t array

  let get_floor conf = conf.(0)
  let iter_next f conf =
    let floor = get_floor conf in
    let src_dev = conf.(floor) in
    let iter_floor nfloor =
      let dst_dev = conf.(nfloor) in
      if nfloor > floor || dst_dev != 0 then
        src_dev |> IDevSet.iter12 (fun ss -> 
            let dst_dev' = IDevSet.union ss dst_dev in
            if IDevSet.is_valid dst_dev' then begin
              let nconf = Array.copy conf in
              nconf.(nfloor) <- dst_dev';
              nconf.(floor) <- IDevSet.diff src_dev ss;
              nconf.(0) <- nfloor;
              f nconf
            end)
    in
    match floor with
      1 -> iter_floor 2
    | 2 -> iter_floor 3; iter_floor 1
    | 3 -> iter_floor 4; iter_floor 2
    | 4 -> iter_floor 3
    | _ -> assert false

  let device_by_id = Hashtbl.create 16
  let device_by_name = Hashtbl.create 16

  let pp fmt (n, conf) =
    for i = 3 downto 0 do 
      Format.fprintf fmt "F%d %s [%d]" i (if i = n then "E" else ".") conf.(i);
      IDevSet.iter (fun d ->
          let n = try Hashtbl.find device_by_id d with Not_found -> "NF"^string_of_int d in
          Format.fprintf fmt "%s(%d) " n d )
        conf.(i);
      Format.fprintf fmt "\n%!"
    done

  let bfs init final =
    let len = ref 0 in
    let dummy = [||] in
    let queue = Queue.create () in
    Queue.add init queue;
    Queue.add dummy queue;
    let visited = ~%[init, ()] in (* Configurations can be hashed with generic hash tables *)
    let rec loop () = 
      (* Don't check for empty queue, will fail if empty which should not happen if the
         final config is reachable
      *)
      let config = Queue.pop queue in
      let config = 
        if config == dummy then begin 
          incr len;
          Queue.add dummy queue;
          Queue.pop queue 
        end else config
      in
      if config = final then !len else begin
        iter_next (fun nconf ->
            if not (visited %? nconf) then begin
              visited.%{nconf} <- ();
              Queue.add nconf queue
            end) config;
        loop ()
      end
    in loop ()
  let fix_and s =
    let rec loop l =
      match l with 
        s::"and"::ll when s.[String.length s - 1] <> ',' ->
        (s^",")::"and"::loop ll
      | s::ll -> s::loop ll
      | [] -> []
    in
    String.concat " " (loop (String.split_on_char ' ' s))
  let read_device s =
    let s = String.remove_prefix ~prefix:" and" s in
    let s = String.remove_prefix ~prefix:" a" s in
    let s = String.remove_suffix ~suffix:"." s in
    let s = String.map (function '-' -> ' ' | c -> c) s in
    let s = String.trim s in
    match String.split_on_char ' ' s with
      [ s; "generator"] -> `Gen,s
    | [ s; "compatible"; "microchip"] -> `Chip,s
    | s -> failwith (String.concat "|" s) 

  let read_devices s =
    match String.split_on_char ',' (fix_and s) with
      [" nothing relevant." ]-> []
    | l -> List.map read_device l
  let read_line s =
    Scanf.sscanf s
      "The %s floor contains%[a-z, .-]" (fun a b -> 
          let n = match a with 
              "first" -> 0
            | "second" -> 1
            | "third" -> 2
            | _ -> 3
          in
          n, read_devices b)

  let read_input () = 
    let init = Array.make 4 [] in
    Input.fold_lines (fun () s -> let n, d = read_line s in init.(n) <- d) ();
    init

  let build_config init =
    let names = ~%[] in
    let count = ref 0 in
    Array.iter (List.iter (fun (_,s) ->
        if not (names %? s) then begin
          incr count;
          names.%{s} <- !count;
        end
      )) init;
    let make_dev (c, s) =
      let pref, d = 
        match c with
          `Gen -> "G", 1 lsl names.%{s}
        | `Chip -> "C", (1 lsl names.%{s}) lsl IDevSet.chip_len
      in
      let n = pref ^ s in
      device_by_id.%{d} <- n;
      device_by_name.%{n} <- d;
      d
    in
    let full = Hashtbl.fold (fun s _ acc -> 
        List.fold_left (fun acc c -> IDevSet.add (make_dev (c,s)) acc)  acc [`Gen;`Chip]) names IDevSet.empty
    in
    let init = Array.append [|1|] (Array.map (List.fold_left (fun acc d -> IDevSet.add (make_dev d) acc) IDevSet.empty) init) in
    init, IDevSet.[|4;empty;empty;empty;full|]


  let solve l =
    let init = read_input () in
    init.(0) <- init.(0) @ l;
    let init, final = build_config init in
    let n = bfs init final in
    Solution.printf "%d" n

  let solve_part1 () = solve []
  let solve_part2 () = solve [`Chip, "elerium"; `Chip, "dilithium"; `Gen, "elerium"; `Gen, "dilithium"]
end


let () = Solution.register_mod (module S)