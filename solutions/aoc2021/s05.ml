open Utils
open Syntax

module S =
struct
  let name = Name.mk "s05"

  let rec iter_pair start step stop f =
    if start = stop then f stop
    else
      let () = f start in
      let a,b = start in
      let c,d = step in
      iter_pair (a+c, b+d) step stop f

  let solve select_step =
    let table =
      Input.fold_scan "%d,%d -> %d,%d"
        (fun acc x1 y1 x2 y2 ->
           match select_step x1 y1 x2 y2 with
             None -> acc
           | Some step ->
             iter_pair (x1, y1) step (x2, y2) (fun p ->
                 acc.%{p} <- 1 + (acc.%?{p} or 0));acc
        ) ~%[]
    in
    let num = Hashtbl.fold (fun _ n acc -> if n > 1 then acc+1 else acc) table 0 in
    Solution.printf "%d" num

  let select1 x1 y1 x2 y2 =
    if x1 = x2 then
      Some (if y1 < y2 then (0,1) else (0,-1))
    else
    if y1 = y2 then Some (if x1 < x2 then (1,0) else (-1, 0))
    else None

  let solve_part1 () = solve select1

  type cmp = Lt | Eq | Gt
  let cmp a b = if a < b then Lt else if a = b then Eq else Gt
  let select2 x1 y1 x2 y2 =
    match cmp x1 x2, cmp y1 y2 with
      Eq, Eq -> Some (0,0)
    | Lt, Eq -> Some (1, 0)
    | Gt, Eq -> Some (-1, 0)
    | Eq, Lt -> Some (0, 1)
    | Eq, Gt -> Some (0, -1)
    | _   when abs (x1 - x2) <> abs (y1 - y2) -> None
    | Lt, Lt -> Some (1, 1)
    | Lt, Gt -> Some (1, -1)
    | Gt, Lt -> Some (-1, 1)
    | Gt, Gt -> Some (-1, -1)
      let solve_part2 () = solve select2
end

let () = Solution.register_mod (module S)