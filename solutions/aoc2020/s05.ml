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
    Iter2.(load_input parse_binary
           |> list |> max_)
    |> Solution.printf "%d"
  let solve_part2 () =
    let arr = Array.make 1024 false in
    Input.fold_lines (fun () s -> arr.(parse_binary s) <- true) ();
    Iter2.(
      range int ~start:1 (Array.length arr - 2)
      |> find (fun i ->
          arr.(i-1) && arr.(i+1) && not arr.(i)
        ))
    |>
    Solution.printf "%d" 

end

let () = Solution.register_mod (module S)