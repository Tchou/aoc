open Utils
open Syntax
module S =
struct
  let name = Name.mk "s09"

  let read_input () =
    let dist = ~%[] in
    Input.fold_scan "%s to %s = %d" (fun () s1 s2 d ->
        dist.%{s1} <- (s2, d) :: (dist.%?{s1} or []);
        dist.%{s2} <- (s1, d) :: (dist.%?{s2} or [])) ();
    dist

  let enumerate_paths f graph =
    let total = Hashtbl.length graph in
    let rec loop n v path =
      if n = total then f (List.rev path)
      else
        graph.%{v}
        |> List.iter (fun (w, d) ->
            if not (List.exists (fun (x, _) -> w = x) path) then
              loop (n+1) w ((w,d)::path)          
          )
    in
    Hashtbl.iter (fun v _ -> loop 1 v [(v,0)]) graph

  let cost f init graph =
    let c = ref init in
    enumerate_paths (fun p -> c := f !c Iter2.(p |> list |> map snd |> sum int)) graph;
    !c
  let solve f init () =
    let graph = read_input () in
    let n = cost f init graph in
    Solution.printf "%d" n
  let solve_part1 = solve min max_int
  let solve_part2 = solve max 0
end

let () = Solution.register_mod (module S)