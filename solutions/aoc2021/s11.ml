open Utils
open Syntax

module S =
struct
  let name = Name.mk "s11"

  let load_grid () =
    Input.fold_lines (fun acc s ->
        let line = Array.init (String.length s)
            (fun i -> Char.code s.[i] - Char.code '0')
        in line ::acc
      ) []
    |> List.rev
    |> Array.of_list

  let iter_update grid f =
    for i = 0 to  9 do
      for j = 0 to 9 do
        grid.(i).(j) <- f grid i j
      done;
    done

  let adjacent r c =
    let l = [(-1,-1);(-1, 0);(-1,1);
             (0, -1);(0,1);
             (1,-1); (1,0); (1,1)]
    in
    List.filter_map (fun (i, j) ->
        let ri = i + r in
        let cj = j + c in
        if ri < 0 || cj < 0 || ri >= 10 || cj >= 10 then None
        else Some (ri, cj)
      ) l


  let debug n grid =
    let str =
      Array.map (fun l ->
          (String.join "" (Array.map string_of_int l) )) grid
      |> String.join "\n"
    in
    Printf.printf "%d\n" n;
    Printf.printf "%s\n" str;
    Printf.printf "----\n"

  let simulate c stop grid  =
    let count = ref 0 in
    let visited = ~%[] in
    let rec loop n =
      if stop grid n then begin
        iter_update grid (fun g r c -> (g.(r).(c) + 1) mod 10);
        let repeat = ref true in
        Hashtbl.clear visited;
        while !repeat do
          repeat := false;
          iter_update grid (fun g r c ->
              let v = g.(r).(c) in
              if v = 0 && not (visited %? (r,c)) then begin (* r, c flashed, increase neighbours *)
                visited.%{r, c}<-();
                let adj = adjacent r c in
                List.iter (fun (r', c') ->
                    let v' = grid.(r').(c') in
                    if v' <> 0 then begin
                      repeat := true;
                      grid.(r').(c') <- (v' + 1) mod 10
                    end) adj

              end;v)
        done;
        iter_update grid (fun g r c ->
            let v = g.(r).(c) in
            if v = 0 then incr count;v);
        loop (n-1)
      end
    in loop c; !count
  let solve_part1 () =
    load_grid ()
    |> simulate 100 (fun _ n -> n > 0)
    |> Printf.printf "%d\n"

  let solve_part2 () =
    let step = ref 0 in
    try
      load_grid ()
      |> simulate max_int (fun grid _ ->
          if Array.for_all (fun t -> Array.for_all ((=) 0) t) grid
          then raise Exit;
          incr step;true
        )
      |> ignore
    with Exit -> Printf.printf "%d\n" !step

end

let () = Solution.register_mod (module S)