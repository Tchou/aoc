open Utils
module S =
struct
  let name = Name.mk "s15"

  let hash acc c  =
    let x = Char.code c in
    let acc = x + acc in
    ((acc lsl 4) + acc) land 255

  let hash_string s =
    String.fold_left hash 0 s


  let solve_part1 () =
    let acc = Input.fold_fields ',' (
        List.fold_left (fun acc s -> acc + hash_string s)

      ) 0 in
    Ansi.printf "%d\n" acc

  let[@tail_mod_cons] rec remove lab box =
    match box with
      [] -> []
    | ((l1, f1) as e1)::box1 ->
      if l1 = lab then box1
      else e1 :: remove lab box1

  let[@tail_mod_cons] rec insert ((l, _) as e) box =
    match box with
      [] -> [ e ]
    | ((l1, _) as e1) :: box1 ->
      if l = l1 then (e::box1) else
        e1 :: (insert e box1)

  let pp fmt boxes =
    Array.iteri (fun i b ->
        if b <> [] then begin
          Ansi.fprintf fmt "Box %d: [" i;
          List.iter (fun (l, f) -> Ansi.fprintf fmt "(%s, %d) " l f) b;
          Ansi.fprintf fmt "]\n" end
      ) boxes;
    Ansi.fprintf fmt "\n"

  let run boxes instr =
    let last = String.length instr - 1 in
    let lab, rem =
      match instr.[last] with
        '-' -> String.sub instr 0 last, true
      | '0'..'9' -> String.sub instr 0 (last - 1), false
      | c -> Ansi.eprintf ">>>%s<<<\n" instr;assert false
    in
    let idx = hash_string lab in
    boxes.(idx) <-
      if rem then remove lab boxes.(idx)
      else
        let f = Char.code instr.[last] - Char.code '0' in
        insert (lab, f) boxes.(idx)

  let score_box box l =
    let b = box + 1 in
    fst (List.fold_left (fun (acc,i) (l, f) ->
        acc + b * i * f, i+1) (0, 1) l)

  let score boxes =
    Array.fold_left
      (fun (acc, i) box -> acc + score_box i box, i+1) (0,0)
      boxes
    |> fst
  let solve_part2 () =
    let boxes = Array.make 256 [] in
    let () =
      Input.fold_fields ',' (fun () l ->
          List.iter (fun i -> run boxes i) l) ()
    in
    score boxes
    |> Ansi.printf "%d\n"

end

let () = Solution.register_mod (module S)