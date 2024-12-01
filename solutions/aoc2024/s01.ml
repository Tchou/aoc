open Utils
module S =
struct
  let name = Name.mk "s01"
  let read_input () =
    Input.fold_scan "%d %d" (fun (acc1, acc2) i1 i2 ->
        (i1::acc1, i2::acc2)
      ) ([], [])

  let mk_sol f =
    let l1, l2 = read_input () in
    let res = f l1 l2 in
    Ansi.(printf "%a%d%a\n" fg green res clear color)

  let solve_part1 () =
    mk_sol (fun l1 l2 ->
        List.fold_left2 (fun acc i1 i2 ->
            acc + (abs (i1 - i2)))
          0
          (List.sort compare l1) 
          (List.sort compare l2))

  let solve_part2 () =
    mk_sol (fun l1 l2 ->
        let count = Hashtbl.create 16 in
        let open Syntax in
        List.iter (fun i2 ->
            Hashtbl.update count i2
              (fun c -> Some (1 + (c or 0)))) l2;
        List.fold_left
          (Agg.Left.sum (fun i -> i * ((Hashtbl.find_opt count i) or 0))) 0 l1
      )
end

let () = Solution.register_mod (module S)