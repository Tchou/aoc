open Utils
open Syntax
module S =
struct
  let name = Name.mk "s02"

  let has2_3 s =
    let map = ~%[] in
    String.iter (fun c -> map.%{c} <- 1 + (map.%?{c} or 0)) s;
    let c2, c3 = Hashtbl.fold (fun _ n (acc2, acc3) ->
        (acc2 + int_of_bool (n=2)), (acc3 + int_of_bool (n = 3))) map (0,0)
    in
    (c2 > 0, c3 > 0)

  let read_input () =
    Input.list_lines Fun.id


  let count l =
    let i2, i3 = List.fold_left (fun (acc2, acc3) s ->
        let i2, i3 = has2_3 s in
        acc2 + int_of_bool i2, acc3 + int_of_bool i3
      ) (0, 0) l
    in i2 * i3

  let solve_part1 () =
    let letters  = read_input () in
    let n = count letters in
    Solution.printf "%d" n

  let differ_by_one s1 s2 =
    assert (String.length s1 = String.length s2);
    let count = ref 0 in
    let acc = ref [] in
    String.iteri (fun i c ->
        if c = s2.[i] then acc := c ::!acc
        else incr count) s1;
    !count = 1, List.rev !acc |> String.implode

  let find_ids ids =
    let rec loop s =
      match s () with
        Seq.Nil -> assert false
      | Seq.Cons ((s1, s2), sq) ->
        let b, res = differ_by_one s1 s2 in
        if b then res else loop sq
    in
    loop (Comb.pairs ~sym:false ~refl:false ids)

  let solve_part2 () =
    let ids = read_input () in
    let s = find_ids ids in
    Solution.printf "%s" s
end

let () = Solution.register_mod (module S)