open Utils
module S =
struct
  let name = Name.mk "s18"

  type number = Cst of int
              | Pair of number * number
  let rec pp fmt n =
    match n with
      Cst i -> Ansi.fprintf fmt "%d" i
    | Pair (n1, n2) ->
      Ansi.fprintf fmt "@[[%a,%a]@]" pp n1 pp n2
  let split_cst n =
    let l = n / 2 in
    let r = n - l in
    Pair(Cst l, Cst r)

  let rec propagate_left n i =
    match n with
      Cst j -> Cst (i + j)
    | Pair (l, r) -> Pair (l, propagate_left r i)

  let rec propagate_right n i =
    match n with
      Cst j -> Cst (i + j)
    | Pair (l, r) -> Pair (propagate_right l i, r)

  let explode n =
    let rec loop n d =
      match n with
      | Cst _ -> n, `Unchanged
      | Pair (Cst il, Cst ir) when d = 4 -> Cst 0, `LeftRight (il, ir)
      | Pair (l, r) ->
        let ll, lo = loop l (d+1) in
        match lo with
          `LeftRight (il, ir) -> Pair (ll, propagate_right r ir),`Left il
        | `Left _ as il -> Pair (ll, r), il
        | `Right ir -> Pair (ll, propagate_right r ir),`Done
        | `Done -> Pair (ll, r), `Done
        | `Unchanged ->
          let rr, ro = loop r (d+1) in
          match ro with
            `LeftRight (il, ir) -> Pair (propagate_left ll il, rr), `Right ir
          | `Left il -> Pair (propagate_left ll il, rr), `Done
          | `Right _ as ir -> Pair (ll, rr),ir
          | `Done -> Pair (ll, rr), `Done
          | `Unchanged -> n, `Unchanged
    in
    let rec repeat n =
      match loop n 0 with
        n, `Unchanged -> n
      | n, _ -> repeat n
    in repeat n
  let split n =
    let rec loop n =
      match n with
        Cst i when i < 10 -> n, false
      | Cst i -> split_cst i, true
      | Pair(l, r) ->
        let ll, bl = loop l in
        if bl then Pair (ll, r), true
        else
          let rr, br = loop r in
          Pair (ll, rr),br
    in
    loop n
  let reduce n =
    let rec repeat n =
      let n1 = explode n in
      (* n1 cannot explode anymore *)
      (* Try one split: *)
      let n2,changed = split n1 in
      (* Repeat if split changed the tree *)
      if changed then repeat n2 else n2
    in repeat n

  let add n1 n2 = reduce (Pair (n1, n2))

  let rec magnitude = function
      Cst i -> i
    | Pair (l, r) -> 3 * magnitude l + 2 * magnitude r

  let rec parse_num s i =
    match s.[i] with
    '0'..'9' as c -> Cst (Char.code c - Char.code '0'), i+1
    | '[' ->
      let l, i = parse_num s (i+1) in
      assert (s.[i]=',');
      let r, i = parse_num s (i+1) in
      assert (s.[i]= ']');
      Pair (l, r), i + 1
    | _ -> assert false
  let parse_num s = fst (parse_num s 0)
  let solve_part1 () =
    let n1 = parse_num (read_line ()) in
    Input.fold_lines (fun acc line ->
        let n = parse_num line in
        let nacc = add acc n in
        nacc) n1
    |> magnitude
    |> Ansi.printf "%d\n"

  let solve_part2 () =
    let numbers =
      Input.fold_lines (fun acc line ->
          (parse_num line)::acc) []
      |> Array.of_list
    in
    let max_m = ref 0 in
    for i = 0 to Array.length numbers - 1 do
      for j = i+1 to Array.length numbers - 1 do
        max_m := max !max_m (magnitude (add numbers.(i) numbers.(j)));
        max_m := max !max_m (magnitude (add numbers.(j) numbers.(i)));
      done;
    done;
    Ansi.printf "%d\n" !max_m

end

let () = Solution.register_mod (module S)