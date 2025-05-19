open Utils
open Syntax
module S =
struct
  let name = Name.mk "s06"
  (*
                 COM                0
                  |
                  B                 1 42
                 / \
                C   G               2 41
               /    |
              D     H               3  3
             / \
            I   E                   4  4,4+5+18
                | \
                F  J                5  5, 5+13
                   |
                   K                6   6 + 7
                   |
                   L                7    7
  *)

  let read_input () =
    let map = ~%[] in
    Input.fold_scan "%[^)])%[^)]" (fun () parent child ->
        map.%{parent} <- child :: (map.%?{parent} or [])
      ) ();
    map

  let count_orbits map =
    let rec loop n node =
      let children = map.%?{node} or [] in
      n + (children
           |> List.map (loop (n+1))
           |> Iter.(sum list int))
    in
    loop 0 "COM"


  let min_orbital_transfers map =
    let merge o1 o2 = match o1, o2 with
        None, None -> None
      | None, Some x | Some x, None -> Some x
      | _ -> assert false
    in
    let exception Found of int in
    let rec loop n node =
      if node = "YOU" then (None, Some n)
      else if node = "SAN" then (Some n, None)
      else
        let children = map.%?{node} or [] in
        match
          List.fold_left (fun ((acs, acy) as acc) child ->
              let os, oy = loop (n+1) child in
              merge acs os, merge acy oy) (None, None) children
        with
          Some santa, Some you -> raise (Found (santa - n - 1 + you - n - 1))
        | res -> res
    in
    try
      ignore (loop 0 "COM");
      -1
    with Found n -> n


  let solve count  =
    let map = read_input () in
    let n = count map in
    Solution.printf "%d" n

  let solve_part1 () = solve count_orbits
  let solve_part2 () = solve min_orbital_transfers
end

let () = Solution.register_mod (module S)