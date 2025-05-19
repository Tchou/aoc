open Utils
module S =
struct
  let name = Name.mk "s05"

  let rec map lst n =
    match lst with
      [] -> n
    | (d, s, l)::_ when n >= s && n < s + l -> n + (d - s)
    | _ :: ll ->  map ll n

  let load_input map =
    let seeds =
      match String.split_on_char ' ' (Input.read_line ()) with
        [] -> []
      | _ :: l -> List.map int_of_string l
    in
    let _ = Input.read_line () in (* empty line *)
    let maps,_ =
      Input.fold_lines (fun (accf, accl) ->  function
            "" -> ((map (List.rev accl))::accf), []
          | s when s.[0] < '0' || s.[0] > '9' -> (accf, accl)
          | s ->
            Scanf.sscanf s "%d %d %d" (fun a b c -> accf, (a,b,c)::accl)
        ) ([],[])
    in
    seeds,List.rev maps

  let solve_part1 () =
    let seeds, maps = load_input map in
    Compare.min_list (List.fold_right (@@) maps) seeds
    |> Solution.printf "%d"

  (*
  For part two, we need to map interval to interval since
  the ranges are too big to enumerate. The possible cases are:

       src.......................len
  x---l
  x-------........l
  x-------........................--------l
            x..............l
            x.....................--------l
                                    x----l
   We can make things a bit clearer by converting
   start+len into (start, end)
  *)
  let split_interval (dst, src, len) (acc_tr, acc_rest) (x, l) =
    let y = x + l - 1 in
    let t = src + len - 1 in
    if y < src || x > t then acc_tr, (x, l)::acc_rest else
      let x, acc_rest =
        if x < src then src, (x, src-x)::acc_rest else x, acc_rest
      in
      let y, acc_rest =
        if y > t then t, (t+1, y-t)::acc_rest else y, acc_rest
      in
      (dst + (x-src), y+1-x)::acc_tr, acc_rest

  (* applies an interval to seeds, accumulates transformed
     parts of seeds in acc_tr, and returns the unmatched
     seeds *)
  let apply_interval (acc_tr, seeds) int =
    List.fold_left (split_interval int) (acc_tr,[]) seeds

  (* Applies all the intervals in a map to all the seeds.
     At the end, unmatched seeds a put back together
     with transformed ones. *)
  let apply_map seeds map =
    let matched, rest =
      List.fold_left apply_interval ([], seeds) map
    in
    List.rev_append rest matched

  let rec make_seeds l acc =
    match l with
    | [] | [_] -> acc
    | x1 :: x2 :: ll -> make_seeds ll ((x1,x2)::acc)

  let solve_part2 () =
    let seeds, maps = load_input Fun.id in
    let seeds = make_seeds seeds [] in
    let () =
      seeds
      |> Iter.(snd list)
      |> Iter.(sum seq int)
      |> Solution.printf "%d"
    in
    let res = List.fold_left apply_map seeds maps in
    Compare.min_list fst res
    |> Solution.printf "%d"
end
let () = Solution.register_mod (module S)