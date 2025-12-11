open Utils
open Syntax
module S =
struct
  let name = Name.mk "s11"

  let read_input () =
    let g = ~%[] in
    Input.fold_scan "%[^:]: %[a-z ]"
      (fun () src line ->
         String.split_on_char ' ' line
         |> List.iter (fun dst -> g.%{src} <- dst :: (g.%?{src} or []))) ();
    g

  let rec count_path g src dst path =
    if src = dst then 1
    else if List.mem src path then assert false (* to detect cycles *)
    else
      (g.%?{src} or [])
      |> List.fold_left (fun acc v -> acc + count_path g v dst (src::path)) 0

  (*
   The graph is a dag that gives an exponential number
   of paths when naively unfolded, we just memoize the result.
   The only interesting information when returning a count is
   whether we have seen fft and dac on the current path, not
   the path itself.
  *)
  let count_path2 g src dst =
    let memo = ~%[] in
    let rec loop src has_dac has_fft =
      match memo.%?{src, has_dac, has_fft} with
        Some r -> r
      | None ->
        let res =
          if src = dst then
            int_of_bool (has_dac && has_fft)
          else
            let has_dac = has_dac || src = "dac" in
            let has_fft = has_fft || src = "fft" in
            (g.%?{src} or [])
            |> List.fold_left (fun acc v -> acc + loop v has_dac has_fft) 0
        in memo.%{src, has_dac, has_fft} <- res; res
    in
    loop src false false

  let solve_part1 () =
    let g = read_input () in
    let n = count_path g "you" "out" [] in
    Solution.printf "%d" n

  let solve_part2 () =
    let g = read_input () in
    let n = count_path2 g "svr" "out"  in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)