open Utils
module S =
struct
  let name = Name.mk "s25"

  let read_input () =
    Input.fold_lines (fun acc line ->
        match line, acc with
          ".....",(l,true) -> (Array.make 5 1, '.')::l,false
        | "#####",(l,true) -> (Array.make 5 1, '#')::l,false
        | "", (l, _) -> (l, true)
        | s, ((a, c)::_,false) -> String.iteri (fun i x ->
            if x = c then a.(i) <- a.(i)+ 1) s; acc
        | s, _ -> Format.printf ">>> %s\n%!" s; assert false
      ) ([],true)
    |> fst |> List.partition_map
      Either.(fun (t, c) -> if c = '.' then Left t else Right t)

  let count_fit keys locks =
    keys |> List.fold_left (fun acc k ->
        locks |> List.fold_left (fun acc l ->
            acc + (int_of_bool (Array.for_all2 (fun x y -> (7-x) + y <= 7) k l))) acc
      ) 0

  let solve_part1 () =
    let keys, locks = read_input () in
    let n = count_fit keys locks in
    Solution.printf "%d" n
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)