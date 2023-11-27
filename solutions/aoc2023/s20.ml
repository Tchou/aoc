open Utils
open Syntax

module S =
struct
  let name = Name.mk "s20"


  type kind =
    | Broadcast
    | FlipFlop of { mutable state : bool }
    | Conj of { mem : (string,bool) Hashtbl.t; }

  let remove_sym s =
    String.sub s 1 (String.length s - 1)

  let load_input () =
    let config = ~%[] in
    let rev = ~%[] in
    Input.fold_scan "%[%%&a-z] -> %[ a-z,]" (fun () name dests ->
        let dests = String.split_on_char ',' dests in
        let dests = List.map String.trim dests in
        let kind, name =
          match name.[0] with
          | '%' -> (FlipFlop {state = false }, remove_sym name)
          | '&' -> (Conj { mem = ~%[]}, remove_sym name)
          | 'b' -> (Broadcast, "broadcaster")
          | _ -> assert false
        in
        config.%{name} <- kind, dests;
        List.iter (fun d -> rev.%{d}<- name::(rev.%?{d} or [])) dests
      ) ();
    Hashtbl.iter (fun name -> function
          (Conj { mem },_) ->
          assert (Hashtbl.length mem = 0);
          List.iter (fun i -> mem.%{i}<- false) rev.%{name}
        | _ -> ()) config;
    config, rev

  let run watch_list config =
    let num_low = ref 0 in
    let num_high = ref 0 in
    let monitors = ~%(List.map (fun m -> (m,false)) watch_list) in
    let queue = Queue.create () in
    let send pulse input =
      if pulse && monitors %? input then
        monitors.%{input} <- true;
      List.iter (fun m ->
          incr (if pulse then num_high else num_low);
          Queue.add (pulse, input, m) queue)
    in
    send false "button" ["broadcaster"];
    while not (Queue.is_empty queue) do
      let pulse, input, src = Queue.take queue in
      match config.%{src} with
        Broadcast, dests -> send pulse src dests
      | FlipFlop r, dests ->
        if not pulse then begin
          r.state <- not r.state;
          send r.state src dests
        end
      | Conj { mem }, dests ->
        mem.%{input} <- pulse;
        let out = Hashtbl.to_seq_values mem |> Seq.for_all Fun.id |> not in
        send out src dests
      | exception Not_found -> ()
    done;
    !num_high, !num_low,
    Hashtbl.fold (fun k b acc -> if b then k::acc else acc) monitors []

  let solve_part1 () =
    let config,_ = load_input () in
    let high = ref 0 in
    let low = ref 0 in
    for _ = 1 to 1000 do
      let h, l, _ = run [] config in
      high := h + !high;
      low := l + !low;
    done;
    let total = !high * !low in
    Ansi.printf "%d\n" total

  (* By using the function below which prints the paths
     from rx to broadcaster, we see that the graph has this shape:
     The graph has the shape:
           +-- &y0 <- &z0 <-{ several %paths to broadcaster
           |-- &y1 <- &z1 <-{ several %paths to broadcaster
     rx <- &x <-|-- &y2 <- &z2 <-{ several %paths to broadcaster
           +-- &y3 <- &z3 <-{ several %paths to broadcaster

     We simply monitor all the &yi and take the LCM.
  *)
  let print_paths config rev =
    let res = ref [] in
    let rec loop s path mem =
      if  s = "broadcaster" then
        res := (List.rev mem)::!res
      else
        let  dests = rev.%?{s} or ([]) in
        List.iter (fun d ->
            let skind = match fst config.%{d} with
                Conj {mem} -> "&"
              | FlipFlop _ -> "%"
              | _ | exception Not_found -> ""
            in

            let d' = skind ^ d in
            if not (List.mem d path) then
              loop d (d::path) (d'::mem)) dests
    in
    loop "rx" ["rx"] ["rx"];
    let res = List.sort compare !res in
    List.iter (fun r ->
        Ansi.eprintf "%s\n%!" (String.concat "->" r)
      ) res

  let solve_part2 () =
    let config, rev = load_input () in
    let watch_list =
      match rev.%{"rx"} with
        [ x ] -> ref (rev.%{x})
      | _ -> assert false
    in
    let lcm = ref 1 in
    try
      for i = 1 to 1_000_000 do
        let _, _, found = run !watch_list config in
        List.iter (fun m ->
            watch_list := List.filter (fun x -> x <> m) !watch_list;
            lcm := Math.lcm !lcm i
          ) found;
        if !watch_list = [] then raise Exit
      done;
      Ansi.printf "NOT FOUND\n"
    with Exit ->
      Ansi.printf "%d\n" !lcm
end

let () = Solution.register_mod (module S)