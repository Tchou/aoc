open Utils
module S =
struct
  let name = Name.mk "s04"

  let numbers s =
    String.split_on_char ' ' s
    |> List.filter_map int_of_string_opt

  let load_input f =
    Input.fold_scan "Card %d:%[^|]|%[ 0-9]"
      (fun acc id l1 l2 ->
         f acc id (numbers l1) (numbers l2))

  module IntSet = Set.Make(Int)

  let count_winning win my =
    let common = IntSet.(inter (of_list win) (of_list my)) in
    IntSet.cardinal common

  let score1 acc _id win my =
    acc + (1 lsl (count_winning win my - 1))

  let solve_part1 () =
    load_input score1 0
    |> Solution.printf "%d"

  let solve_part2 () =
    let card_list =
      load_input (fun acc id win my -> (id - 1, (win, my))::acc) []
    in
    let len = 1 + fst (List.hd card_list) in
    let card_list = List.rev card_list in
    let cards = Array.make len 1 in
    Iter2.(
      card_list
      |> list
      |> map (fun (id, (win, my))->
          let sc = count_winning win my in
          let num_id = cards.(id) in
          (* card id has sc winning numbers and num_id copies*)
          for i = id + 1 to id + sc do
            cards.(i) <- cards.(i) + num_id;
          done;
          num_id)
      |> sum int)
    |> Solution.printf "%d"
end

let () = Solution.register_mod (module S)