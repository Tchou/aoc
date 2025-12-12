open Utils
module S =
struct
  let name = Name.mk "s12"

  let read_input () =
    Input.fold_lines
      (fun acc s ->
         if String.length s < 3 || (s.[1] <> 'x' && s.[2] <> 'x') then acc else
           Scanf.sscanf s "%dx%d:%[ ]%[0-9 ]" (fun w h _ s -> 
               let approx = 
                 s 
                 |> String.split_on_char ' '
                 |> List.map int_of_string
                 |> Iter.(sum list int)
               in
               acc + int_of_bool (approx * 9 <= w * h)
             )
      ) 0 
  let solve_part1 () =
    let n = read_input () in
    Solution.printf "%d" n
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)