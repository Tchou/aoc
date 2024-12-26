open Utils
module S =
struct
  let name = Name.mk "s08"

  let load_input () =
    Input.fold_scan "%s %d" (fun acc op n ->
        (op, n)::acc
      ) []
    |> List.rev |> Array.of_list

  let rec eval visited i acc code =
    if i = Array.length code then acc, [] else
    if List.mem  i visited then acc,visited else begin
      let visited = i :: visited in
      let op, n = code.(i) in
      let i, acc =
        match op with
          "nop" -> (i+1), acc
        | "acc" -> (i+1), (acc+n)
        | "jmp" -> (i+n), acc
        | _ -> assert false
      in
      eval visited i acc code
    end

  let inv (op, n) =
    if op = "jmp" then ("nop", n)
    else ("jmp", n)

  let fix_loop code =
    let _, cycle = eval [] 0 0 code in
    let to_fix = List.filter (fun i -> fst code.(i) <> "acc") cycle in
    List.find_map (fun i ->
        code.(i) <- inv code.(i);
        let acc, l = eval [] 0 0 code in
        if l = [] then Some acc
        else begin
          code.(i) <- inv code.(i);
          None
        end
      ) to_fix
    |> Option.get

  let solve_part1 () =
    load_input ()
    |> eval [] 0 0
    |> fst
    |> Solution.printf "%d"

  let solve_part2 () =
    load_input ()
    |> fix_loop
    |> Solution.printf "%d"

end

let () = Solution.register_mod (module S)