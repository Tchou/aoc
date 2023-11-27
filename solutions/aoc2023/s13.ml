open Utils
open Syntax
module S =
struct
  let name = Name.mk "s13"

  let rec check_sym arr l i =
    let x = l - i in
    let y = l + i + 1 in
    if x < 0 || y >= Array.length arr then true
    else
      arr.(x) = arr.(y) && check_sym arr l (i+1)
  let find_hsym arr l =
    let len = Array.length arr in
    let rec loop i =
      if i >= len - 1 then -1
      else
      if i <> l && check_sym arr i 0 then i
      else loop (i+1)
    in
    loop 0

  let rotate arr =
    let alen = Array.length arr in
    let blen = Bytes.length arr.(0) in
    let narr = Array.init blen (fun _ -> Bytes.make alen '.') in
    for i = 0 to alen - 1 do
      for j = 0 to blen - 1 do
        narr.(j).$[alen - i - 1] <- arr.(i).$[j]
      done;
    done;
    narr

  let find_vsym arr l =
    find_hsym (rotate arr) l

  let v n = Some (`vertical, n+1, n)
  let h n = Some (`horizontal, 100*(n+1), n)
  let find_vfirst arr prev =
    let prevv, prevh =
      match prev with
        None -> -1, -1
      | Some(`vertical,_, l) -> l, -1
      | Some(`horizontal,_, l) -> -1, l
    in
    let n = find_vsym arr prevv in
    if n >= 0 then v n else
      let n = find_hsym arr prevh in
      if n >= 0 then h n
      else None

  let score arr prev = find_vfirst arr prev

  let score1 arr =
    match score arr None with
      Some(_, n, _)  -> n
    | None -> assert false

  exception Found of int
  let score2 arr =
    try
      let other = score arr None in
      for i = 0 to Array.length arr - 1 do
        for j = 0 to Bytes.length arr.(0) - 1 do
          let c = arr.(i).$[j] in
          arr.(i).$[j] <- if c = '.' then '#' else '.';
          let s = score arr other in
          Option.iter (fun (_,n,_) -> raise (Found n)) s;
          arr.(i).$[j] <- c;
        done;
      done;
      assert false
    with Found s -> s

  let load_input () =
    let _, lines =
      Input.fold_lines (fun (rows, lst) line ->
          if line = "" then
            [], (Array.of_list(List.rev rows)::lst)
          else
            ((Bytes.of_string line)::rows, lst)
        ) ([],[])
    in
    List.rev lines

  let solve scoref =
    let maps = load_input () in
    List.fold_left (fun acc m -> acc + scoref m) 0 maps
    |> Ansi.printf "%d\n"

  let solve_part1 () = solve score1
  let solve_part2 () = solve score2
end

let () = Solution.register_mod (module S)