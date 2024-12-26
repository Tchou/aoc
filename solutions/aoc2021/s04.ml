open Utils

module S =
struct
  let name = Name.mk "s04"

  let full_line g =
    Array.exists (Array.for_all (fun i -> i < 0)) g

  let full_column g =
    try
      for i = 0 to 4 do
        if Array.for_all (fun l -> l.(i) < 0) g then
          raise_notrace Exit
      done;
      false
    with Exit -> true
  let sum g =
    Array.fold_left (fun acc l ->
        Array.fold_left (fun acc i -> if i > 0 then acc+i else acc) acc l) 0 g
  let solve stop =
    let numbers =
      Input.read_line ()
      |> String.split_on_char ','
      |> List.map int_of_string
    in
    let _ = Input.read_line () in
    let grids, _ =
      Input.fold_fields ' ' (fun (grids, tmp) ->
          function [] | [ "" ] ->
            (Array.of_list (List.rev tmp)) :: grids, []
                 | l ->
                   let line =
                     l |> List.filter_map
                       (function "" -> None
                               | i -> Some (int_of_string i))
                   in
                   (grids, (Array.of_list line) :: tmp))
        ([], [])
    in
    let res = ref 0 in
    let grids = ref grids in
    try
      numbers
      |> List.iter (fun n ->
          grids := !grids |> List.filter (fun g ->
              for i = 0 to 4 do
                for j = 0 to 4 do
                  if g.(i).(j) = n then
                    g.(i).(j) <- min_int + g.(i).(j);
                done;
              done;
              if full_column g || full_line g then begin
                res := n * sum g;
                stop ();
                false;
              end
              else true)
        ); Solution.printf "%d" !res
    with Exit -> Solution.printf "%d" !res


  let solve_part1 () = solve (fun () -> raise Exit)
  let solve_part2 () =  solve (fun () -> ())
end

let () = Solution.register_mod (module S)