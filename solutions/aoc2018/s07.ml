open Utils
open Syntax
module S =
struct
  let name = Name.mk "s07"

  let read_input () =
    let deps = ~%[] in
    Input.fold_scan "Step %c must be finished before step %c can begin."
      (fun () c1 c2 ->
         deps.%{c2} <- c1 :: (deps.%?{c2} or []);
         deps.%{c1} <- deps.%?{c1} or []
      ) ();
    deps

  (* Poor man's topological sort *)
  let rec sort deps =
    let table = Hashtbl.copy deps in
    let order = ~%[] in
    let[@tail_mod_cons] rec loop () =
      match
        Hashtbl.fold (fun task prereq acc ->
            if List.for_all (Hashtbl.mem order) prereq then
              min  acc task
            else acc
          ) table '\xff'
      with
        '\xff' -> []
      | task ->
        table %- task;
        order.%{task} <- ();
        task :: loop ()
    in
    loop () |> String.implode

  let solve_part1 () =
    let deps = read_input () in
    let s = sort deps in
    Solution.printf "%s" s

  type worker = { mutable task : char; mutable time : int }
  let simulate cost num_workers deps =
    let workers = Array.init num_workers (fun _ ->{task='\xff'; time = 0}) in
    let done_ = ~%[] in
    let all_tasks = Hashtbl.copy deps in
    let rec loop time =
      (* simulate one second *)
      (* Decrement the time for all workers *)
      workers |> Array.iter (fun w ->
          if w.time = 1 then done_.%{w.task} <- ();
          w.time <- max 0 (w.time - 1) (* idle workers have time 0 *)
        );
      (* Get all idle workers *)
      let idle_workers = Array.fold_left (fun acc w -> if w.time = 0 then w::acc else acc) [] workers in
      (* Get all tasks forwhich the dependencies are satisfied *)
      let tasks = Hashtbl.fold (fun task dl acc ->
          if List.for_all (Hashtbl.mem done_) dl then task::acc else acc
        ) all_tasks [] |> List.sort compare
      in
      match tasks, idle_workers with
        _ :: _, [] -> loop (time + 1) (* some tasks remaining, but no worker, continue *)
      | [], _ when List.length idle_workers < num_workers -> loop (time+1) (* no tasks but some idle workers *)
      | _ :: _, _ ->(* some tasks and some workers, assign them and continue *)
        let () = try List.iter2 (fun w t ->
            w.task <- t;
            w.time <- Char.code t - Char.code 'A' + cost + 1;
            all_tasks %- t)
            idle_workers tasks
          with _ -> ()
        in
        loop (time+1)
      | _ -> time (* we are finished *)
    in
    loop 0


  let solve_part2 () =
    let deps = read_input () in
    let n = simulate 60 5 deps in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)