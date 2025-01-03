open Utils
module S =
struct
  let name = Name.mk "s16"
  let read_input () =
    let open Scanf in
    let samples = ref [] in
    let continue = ref true in
    while !continue do
      let l1 = Input.read_line () in
      if l1 = "" then continue := false
      else begin
        let l2 = Input.read_line () in
        let l3 = Input.read_line () in
        let _blank = Input.read_line () in
        let a1 = sscanf l1 "Before: [%d, %d, %d, %d]"
            (fun a b c d -> [|a;b;c;d;|])
        in
        let a2 = sscanf l2 "%d %d %d %d"
            (fun a b c d -> [|a;b;c;d|])
        in
        let a3 = sscanf l3 "After: [%d, %d, %d, %d]"
            (fun a b c d -> [|a;b;c;d|])
        in
        samples := (a1,a2,a3)::!samples;
      end;
    done;
    let _ = Input.read_line () in
    let code = Input.fold_lines (fun acc l ->
        sscanf l "%d %d %d %d" (fun a b c d ->
            [|a;b;c;d|]::acc)) []
    in
    List.rev !samples, List.rev code

  let alloc src dst =
    if Array.length dst = 0 then Array.copy src else dst

  let opr op src dst a b c =
    let dst = alloc src dst in
    dst.(c) <- op src.(a) src.(b);
    dst

  let opi op src dst a b c =
    let dst = alloc src dst in
    dst.(c) <- op src.(a) b;
    dst

  let addr = opr (+)
  let addi = opi (+)

  let mulr = opr ( * )
  let muli = opi ( * )

  let banr = opr ( land )
  let bani = opi ( land )

  let borr = opr ( lor )
  let bori = opi ( lor )

  let setr src dst a _b c =
    let dst = alloc src dst in
    dst.(c) <- src.(a);
    dst

  let seti src dst a _b c =
    let dst = alloc src dst in
    dst.(c) <- a;
    dst

  let cmpir op src dst a b c =
    let dst = alloc src dst in
    dst.(c) <- int_of_bool (op a src.(b));
    dst

  let cmpri op src dst a b c =
    let dst = alloc src dst in
    dst.(c) <- int_of_bool (op src.(a) b);
    dst

  let cmprr op src dst a b c =
    let dst = alloc src dst in
    dst.(c) <- int_of_bool (op src.(a) src.(b));
    dst

  let gtir = cmpir (>)
  let gtri = cmpri (>)
  let gtrr = cmprr (>)

  let eqir = cmpir (=)
  let eqri = cmpri (=)
  let eqrr = cmprr (=)

  let ops = [
    addr, "addr"; addi, "addi"; mulr, "mulr"; muli, "muli"; banr, "banr"; bani, "bani";
    borr, "borr"; bori, "bori"; setr, "setr"; seti, "seti"; gtir, "gtir"; gtri, "gtri";
    gtrr, "gtrr"; eqir, "eqir"; eqri, "eqri" ; eqrr, "eqrr"]

  let pp_array fmt a =
    Format.fprintf fmt "[%d, %d, %d, %d]"
      a.(0) a.(1) a.(2) a.(3)
  let pp fmt (op_name, a1, a2, a3, res) =
    Format.fprintf fmt "(%a)(%a)(%a) behaves like %s,res=%a"
      pp_array a1 pp_array a2 pp_array a3 op_name pp_array res

  let test_opcode op (a1, a2, a3) =
    match a2 with
      [|_; a;b;c|] ->
      let res = op a1 [| |] a b c in
      res = a3
    | _ -> assert false

  let count_samples samples =
    List.fold_left (fun acc arg ->
        let n = List.fold_left (fun acc (op, op_name)->
            acc + int_of_bool (test_opcode op arg)) 0 ops
        in acc + int_of_bool (n >= 3)) 0 samples
  let solve_part1 () =
    let samples, _ = read_input () in
    let n = count_samples samples in
    Solution.printf "%d" n

  module StrSet = Set.Make(String)
  let op_set =
    ops
    |> List.to_seq
    |> Seq.map snd
    |> StrSet.of_seq

  let pp_codes fmt codes =
    Array.iteri (fun i s ->
        Format.fprintf fmt "%d -> %s\n"
          i (StrSet.elements s |> String.concat ", ")) codes;
    Format.fprintf fmt "--\n"
  let find_opcodes samples =
    let codes = Array.make 16 op_set in
    samples
    |> List.iter (fun (a1, a2, a3) ->
        let valid_ops =
          List.fold_left (fun acc (op, op_name) ->
              if test_opcode op (a1, a2, a3) then
                StrSet.add op_name acc else acc
            ) StrSet.empty ops
        in
        codes.(a2.(0)) <- StrSet.inter codes.(a2.(0)) valid_ops
      );
    let rec simplify codes found =
      match Array.find_opt (fun s -> StrSet.cardinal s = 1 && (StrSet.disjoint s found)) codes with
        None -> codes
      | Some s -> let op = StrSet.choose s in
        simplify (Array.map (fun ops ->
            if StrSet.cardinal ops = 1 then ops else
              StrSet.remove op ops) codes) StrSet.(add op found)
    in
    let final_codes = simplify codes StrSet.empty in
    assert (Array.for_all (fun s -> StrSet.cardinal s = 1) final_codes);
    let by_name = List.map (fun (a, b) -> (b, a)) ops in
    Array.map (fun s -> let op = StrSet.choose s in List.assoc op by_name) final_codes

  let eval opcodes prog =
    let reg = [| 0; 0; 0; 0 |] in
    List.iter (fun instr ->
        ignore (opcodes.(instr.(0)) reg reg instr.(1) instr.(2) instr.(3))
      ) prog;
    reg.(0)

  let solve_part2 () =
    let samples, prog = read_input () in
    let opcodes = find_opcodes samples in
    let n = eval opcodes prog in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)