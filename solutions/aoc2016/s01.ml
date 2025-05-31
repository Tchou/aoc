open Utils
open Syntax
module S =
struct
  let name = Name.mk "s01"


  let forward ((i,j) as dir) pos n =
    Grid.(pos +! (i*n, j*n))

  let read_input () =
    Input.read_line ()
    |> String.split_on_char ','
    |> List.map (fun s ->
        let s = String.trim s in
        match s.[0], String.sub s 1 (String.length s - 1) with
        | 'R', n  -> (Grid.right90, int_of_string n)
        | 'L', n -> (Grid.left90, int_of_string n)
        | _ -> failwith ("Invalid instruction: " ^ s) 
      )


  let follow1 dir steps =
    List.fold_left
      (fun (dir, pos) (f, n) ->
         let dir' = f dir in
         let pos' = forward dir' pos n in
         (dir', pos')
      ) (dir, (0,0)) steps |> snd 
  let follow2 dir steps =
    let memo = ~%[(0,0), ()] in
    let exception Found of Grid.position in
    let rec hforward dir pos n =
      if n = 0 then pos else
        let pos' = forward dir pos 1 in
        let () = if memo %? pos' then raise (Found pos')
          else memo.%{pos'} <- ()
        in
        hforward dir pos' (n-1)
    in
    try
      List.fold_left
        (fun (dir, pos) (f, n) ->
           let dir' = f dir in
           let pos' = hforward dir' pos n in
           (dir', pos')
        ) (dir, (0,0)) steps |> snd 
    with Found pos' -> pos'

  let solve follow_part =
    let steps = read_input () in
    let (x, y) = follow_part Grid.north steps in
    let d = abs x + abs y in
    Solution.printf "%d" d

  let solve_part1 () = solve follow1
  let solve_part2 () = solve follow2
end

let () = Solution.register_mod (module S)