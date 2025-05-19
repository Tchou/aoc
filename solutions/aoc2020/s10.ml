open Utils
open Syntax

module S =
struct
  let name = Name.mk "s10"

  let load_input () =
    Input.fold_scan "%d" (fun acc d -> d :: acc) []
    |> List.rev

  let count_diff l =
    let l = 0::List.sort compare l in
    let rec loop l diff1 diff3 =
      match l with
        [] -> assert false
      | [ _ ] -> diff1 * (diff3 + 1)
      | v1 :: (v2 :: _  as ll) ->
        let d = v2 - v1 in
        assert (d <= 3);
        let diff1, diff3 =
          if d = 1 then diff1 + 1, diff3
          else if d = 3 then diff1, diff3 + 1
          else diff1, diff3
        in
        loop ll diff1 diff3
    in
    loop l 0 0


  let count_arrangements l =
    let max_v = Iter.(max list l) in
    let l = List.sort compare (0::max_v+3::l) in
    let cache = ~%[] in
    let rec loop l =
      let v = List.hd l in
      try cache.%{v} with Not_found ->
        let res =
          match l with
          | [ _ ] -> 1
          | v1::(_::(_::(v4::_ as l4) as l3) as l2) when v4 - v1 = 3 ->
            loop l2 + loop l3 + loop l4
          | v1::(_::(v3::_ as l3) as l2) when v3 - v1 <= 3 ->
            loop l2 + loop l3
          | v1::(v2::_ as l2) when v2 - v1 <= 3 -> loop l2
          | _ -> 0
        in cache.%{v} <- res; res
    in
    loop l


  let solve_part1 () =
    load_input ()
    |> count_diff
    |> Solution.printf "%d"
  let solve_part2 () =
    load_input ()
    |> count_arrangements
    |> Solution.printf "%d"
end

let () = Solution.register_mod (module S)