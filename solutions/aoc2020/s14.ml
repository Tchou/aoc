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
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)