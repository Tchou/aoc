open Utils
open Syntax
let int_of_string s =
  try
    int_of_string s
  with _ -> failwith ("int_of_string:>" ^ s ^"<")
module S =
struct
  let name = Name.mk "s19"
  let read_input () =
    let rules = ~%[] in
    let () = InputUntil.fold_lines (fun acc line ->
        if line = "" then (false, ()) else
          Scanf.sscanf line "%d: %[ab\"0-9 |]" (fun n line -> 
              let rule = String.split_on_char '|' line in
              let rule = rule |> List.map (fun s ->
                  s |> String.trim |> String.split_on_char ' ')
              in
              let rule =
                match rule with
                  [ [("\"a\"" | "\"b\"") as s] ] -> `letter s.[1]
                | _ -> `rule (rule |> List.map (fun l -> List.map int_of_string l))
              in
              (true, rules.%{n} <- rule))
      ) ()
    in
    let words = Input.fold_lines (fun acc l -> l :: acc) []
                |> List.rev
    in rules, words

  let pp_rules fmt rules =
    Hashtbl.iter (fun n r ->
        Format.fprintf fmt "%d: " n;
        let s =
          match r with
            `letter c -> (Printf.sprintf "\"%c\"" c)
          | `rule l ->
            let l = List.map (fun l -> List.map string_of_int l |> String.concat " " ) l
            in
            String.concat "|" l
        in
        Format.fprintf fmt "%s\n" s
      ) rules

  module IntSet = struct
    include Set.Make(Int)
    let pp fmt set = Format.fprintf fmt "{%s}"
        (elements set |> List.map string_of_int |> String.concat ", ")
  end

  let pp_sizes fmt sizes =
    Hashtbl.iter (fun n s -> Format.fprintf fmt "%d: %d\n%!" n s) sizes
  let add_sizes set1 set2 =
    IntSet.fold (fun i acc ->
        IntSet.fold (fun j acc ->
            IntSet.add (i+j) acc) set1 acc) set2 IntSet.empty
  let rule_sizes rules =
    let sizes = ~%[] in
    let rec compute_size n =
      try sizes.%{n} with Not_found ->
        let s =
          match rules.%{n} with
            `letter _ -> 1
          | `rule [] -> 0
          | `rule (l::_) ->
            List.fold_left (fun acc ni -> acc + compute_size ni) 0 l
        in
        sizes.%{n} <-s; s
    in
    Hashtbl.iter (fun i r -> sizes.%{i} <- compute_size i) rules;
    sizes

  let match_rule rules sizes txt =
    let txtlen = String.length txt in
    let rec loop i n =
      if i >= txtlen then -1
      else
        let r = rules.%{n} in
        match r with
          `letter c ->
          if txt.[i] = c then i+1 else -1
        | `rule unions -> loop_unions i unions
    and loop_unions i unions =
      match unions with
        [] -> -1
      | seq :: unions' ->
        let i' = loop_seq i seq in
        if i' < 0 then loop_unions i unions'
        else i'
    and loop_seq i seq =
      match seq with
        [] -> i
      | n :: seq' ->
        let i' = loop i n in
        if i' < 0 then -1 else loop_seq i' seq'
    in
    let r = loop 0 0 in
    r = txtlen

  (* We remark that on the example as well as our input:
     0: 8 11
     and also:
     8: 42 | 42 8
     11: 42 31| 42 11 31
     So:
     - 8 must match a repetition of 42 42 42 42 ...
       but the number of repetition is limited by the size of the
       words recognized by rule 42 (which is unique) and the size
       of the maximal length of a word in our text
     - 11 must match 42 42 42 ... 31 31 31, with the same constraint
       on the number of repetitions

     So we compute an approximation on the number of copies of rule 8 and 11
     We then compute all the combinations of those (since they are in sequence in
     rule 0, we distribute union over concatenation)
     42 . 42 31
     42 . 42 42 31 31
     ...
     42 42 42 . 42 31
     42 42 42 . 42 42 31 31
     ...
     We filter out the rules that match words longer than the maximal word in our list
     and we reorder by longest repetition.
     This seems to work well enough.
  *)
  let l = Seq.init

  let mk_seq n v =
    Seq.init n (fun i -> Array.(make (i+1) v|> to_list))

  let alt_rule8 sizes max_len =
    let len42 = sizes.%{42} in
    let nrep = max_len / len42 in
    mk_seq nrep 42

  let alt_rule11 sizes max_len =
    let len42 = sizes.%{42} in
    let len31 = sizes.%{31} in
    let nrep = max_len / (len42 + len31) in
    Seq.zip
      (mk_seq nrep 42)
      (mk_seq nrep 31)
    |> Seq.map (fun (a,b) -> a@b)

  let alt_rule0 sizes max_len =
    let rule8 = alt_rule8 sizes max_len in
    let rule11 = alt_rule11 sizes max_len in
    let all_combs =
      Seq.product rule8 rule11
      |> Seq.map (fun (a,b) -> a@b)
    in
    let len_seq s = List.fold_left (Agg.Left.sum (fun r -> sizes.%{r})) 0 s in
    let rule0 = all_combs |> Seq.filter (fun s -> len_seq s <= max_len ) in
    rule0 |> List.of_seq |> List.sort (fun l1 l2 -> compare (List.length l2) (List.length l1))

  let solve part2 =
    let rules, words = read_input () in
    let max_len = List.fold_left (Agg.Left.max (String.length)) 0 words in
    let sizes = rule_sizes rules in
    let () = if part2 then begin
        rules.%{0} <- `rule (alt_rule0 sizes max_len);
      end
    in
    let n = List.fold_left (Agg.Left.sum (fun s ->
        int_of_bool (match_rule rules sizes s ))) 0 words
    in
    Solution.printf "%d" n

  let solve_part1 () = solve false

  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)