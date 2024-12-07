open Utils
module S =
struct
  let name = Name.mk "s14"
  type instr = { mask : int * int ;
                 smask : string;
                 instrs : (int * int) list
               }
  let m36 = (1 lsl 36) - 1

  let pr_bin fmt n =
    for i = 35 downto 0 do
      let b = (n lsr i) land 1 in
      let c =
        if b = 0 then '0' else '1'
      in
      Format.fprintf fmt "%c" c
    done

  let parse_mask m =
    let mask1 = ref 0 in (* XX101X -> 001010 *)
    for i = 0 to String.length m - 1 do
      if m.[i] = '1' then mask1 := !mask1 + 1;
      mask1 := !mask1 lsl 1
    done;
    let maskx = ref 0 in (* X01X0X -> 100101 *)
    for i = 0 to String.length m - 1 do
      if m.[i] = 'X' then maskx := !maskx + 1;
      maskx := !maskx lsl 1;
    done;
    (!mask1 lsr 1) land m36, (!maskx lsr 1) land m36

  let apply_mask (m1, mx) i =
    (m1 lor (i land mx)) land m36

  let read_input () =
    Input.fold_scan "%s = %s" (fun acc l r ->
        match l, acc with
          "mask", _ -> { mask = parse_mask r; smask = r; instrs = [] }::acc
        | m, { mask; smask ;instrs} :: acc ->
          Scanf.sscanf m "mem[%d]" (fun addr -> {mask; smask;instrs = (addr, int_of_string r)::instrs}::acc)
        | _ -> acc
      ) []
    |> List.map (fun i -> { i with instrs = List.rev i.instrs })
    |> List.rev

  let eval_instrs instrs =
    let open Syntax in
    let mem = ~% [] in
    instrs |> List.iter (fun i ->
        i.instrs |> List.iter (fun (add, v) ->
            let nv = apply_mask i.mask v in
            mem.%{add} <- nv
          );
      );
    Hashtbl.fold (fun _ v acc -> v + acc) mem 0

  let solve_part1 () =
    let instrs = read_input () in
    let s = eval_instrs instrs in
    Ansi.(printf "%a%d%a\n" fg green s clear color)


  type trie =
      Empty
    | Leaf of int
    | Zero of trie
    | One of trie
    | X of trie
    | ZeroOne of trie * trie
  type bit = B0 | B1 | BX
  let bit_of_char = function '0' -> B0
    | '1' -> B1
    | _ -> BX


  let count = ref 0

  let insert mask t (n, v) =
    let rec loop t i =
      if i < 0 then Leaf v
      else
        let b = (n lsr i) land 1 in
        let c = mask.[35-i] in
        let cb = bit_of_char (
            if c = '0' then Char.chr (b + Char.code '0')
            else c) (* can be '1' or 'X' *)
        in
        match t, cb with
          Empty, B0 -> Zero(loop Empty (i-1))
        | Empty, B1 -> One (loop Empty (i-1))
        | Empty, BX -> X (loop Empty (i-1))
        | Zero t', B0 -> Zero (loop t' (i-1))
        | Zero t', B1 -> ZeroOne (t', loop Empty (i-1))
        | Zero t', BX -> ZeroOne (loop t' (i-1), loop Empty (i-1))
        | One t', B0 -> ZeroOne (loop Empty (i-1), t')
        | One t', B1 -> One (loop t' (i-1))
        | One t', BX -> ZeroOne(loop Empty (i-1), loop t' (i-1))
        | ZeroOne (t0, t1), B0 -> ZeroOne (loop t0 (i-1), t1)
        | ZeroOne (t0, t1), B1 -> ZeroOne (t0, loop t1 (i-1))
        | ZeroOne (t0, t1), BX -> ZeroOne (loop t0 (i-1), loop t1 (i-1))
        | X t', B0 -> ZeroOne (loop t' (i-1), t')
        | X t', B1 -> ZeroOne (t', loop t' (i-1))
        | X t', BX -> X (loop t' (i-1))
        | Leaf _, _ -> assert false
    in
    loop t 35

  let rec sum_trie t =
    match t with
      Empty -> 0
    | Leaf v -> v
    | Zero t | One t -> sum_trie t
    | X t -> 2 * sum_trie t
    | ZeroOne (t0, t1) ->
      sum_trie t0 + sum_trie t1

  let solve_part2 () =
    let instrs = read_input () in
    let trie = List.fold_left (fun t instr ->
        List.fold_left (insert instr.smask) t instr.instrs
      ) Empty instrs in
    let n = sum_trie trie in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
end


let () = Solution.register_mod (module S)