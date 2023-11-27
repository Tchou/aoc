open Utils
module S =
struct
  let name = Name.mk "s07"

  let mult l =
    let rec loop last n l acc =
      match l with
        [] -> (last, n)::acc
      | v::ll ->
        if v = last then loop last (n+1) ll acc
        else loop v 1 ll ((last,n)::acc)
    in
    match List.sort compare l with
      [] -> []
    | v::ll ->
      List.sort (fun x y -> compare (snd y) (snd x)) (loop v 1 ll [])


  let compare_cards ((x,_),_) ((y,_),_) = compare x y
  let classify m =
    let m' =
      match List.partition (fun c -> fst c = '0') m with
      | [], _ -> m
      | _, [] -> [('A', 5)]
      | ['0',n], (c, k)::l -> (c,k+n)::l
      | _ -> assert false
    in
    match m' with
      (_,5)::_ -> 7
    | (_,4)::_ -> 6
    | (_,3)::(_,2)::_ -> 5
    | (_,3)::_ -> 4
    | (_,2)::(_,2)::_ -> 3
    | (_,2)::_ -> 2
    | (_,1)::_ -> 1
    | _ -> assert false

  let mk_card joker s =
    let l = String.explode s in
    let l = List.map (function
        'T' ->'A'
        |'J'-> if joker then '0' else 'B'
        |'Q'->'C'
        |'K'->'D'
        |'A'->'E'
        | c -> c) l
    in
    (classify (mult l),l),s

  let load_input joker =
    let cards = Input.fold_scan "%s %d" (fun acc s bid ->
        ((mk_card joker s),bid)::acc) []
    in
    Array.of_list cards

  let solve joker =
    let cards = load_input joker in
    Array.sort compare_cards cards;
    cards
    |> Array.fold_left (fun (i, total) (_, bid) ->
        (i+1, i*bid + total)) (1, 0)
    |> snd
    |> Ansi.printf "%d\n"

  let solve_part1 () = solve false
  let solve_part2 () = solve true

end

let () = Solution.register_mod (module S)