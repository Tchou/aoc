open Utils

module S =
struct
  let name = Name.mk "s24"

  let sum l = Iter.(l |> list |> sum int)
  let prod l = Iter.(l |> list |> prod int)

  let can_distribute num_split target numbers =
    let rec loop used unused target round l =
      if target = 0 then 
        if round = num_split then unused = []
        else loop [] [] target (round+1) unused
      else
        match l with
          [] -> false
        | n :: ll ->
          if n > target then loop used (n::unused) target round ll
          else
            (loop (n::used) unused (target-n) round ll) ||
            (loop used (n::unused) target round ll)
    in
    loop [] [] target 0 numbers

  let distribute_for num_split target numbers =
    let rec loop used unused target l acc =
      if target = 0 then 
        let rest = List.rev_append unused l in 
        if can_distribute (num_split - 1) target rest 
        then (used,List.length used, prod used, rest) :: acc
        else acc
      else
        match l with
          [] -> acc
        | n :: ll ->
          if n > target then loop used (n::unused) target ll acc
          else
            let acc = loop (n::used) unused (target-n) ll acc in
            loop used (n::unused) target ll acc
    in
    loop [] [] target numbers []


  let split_in_k k numbers =
    let total = sum numbers in
    let total_length = List.length numbers in
    let target = if total mod k <> 0 then fail "Impossible repartition" else total / k in
    let splits = distribute_for k target numbers in
    let _,_,res,_ = Iter.(
        splits 
        |> list 
        |> min_ ~compare:(fun (_,l1,p1,_) (_,l2,p2,_) ->
            let c = Int.compare l1 l2 in
            if c <> 0 then c else Int.compare p1 p2
          )
      )
    in
    res
  let read_input () =
    Input.list_scan "%d" Fun.id

  let solve k () =
    let l = read_input () in
    let n = split_in_k k l in
    Solution.printf "%d" n
  let solve_part1 = solve 3
  let solve_part2 = solve 4

end

let () = Solution.register_mod (module S)