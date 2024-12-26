open Utils
module S =
struct
  let name = Name.mk "s05"

  let parse_binary =
    String.fold_left (fun acc c ->
        let b = match c with 'R'|'B' -> 1 | _ -> 0 in
        (acc lsl 1) lor b) 0
  let load_input f =
    Input.fold_lines (fun acc s ->(f s)::acc) []

  let solve_part1 () =
    load_input parse_binary
    |> List.fold_left (Agg.Left.max Fun.id) 0
    |> Solution.printf "%d"
  let solve_part2 () =
    let arr = Array.make 1024 false in
    Input.fold_lines (fun () s -> arr.(parse_binary s) <- true) ();
    let exception Found of int in
    try
      for i = 1 to Array.length arr - 2 do
        if arr.(i-1) && arr.(i+1) && not arr.(i) then raise (Found i)
      done;
      Solution.printf "NOT FOUND"
    with Found n -> Solution.printf "%d" n

end

let () = Solution.register_mod (module S)