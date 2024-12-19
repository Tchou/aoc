open Utils
open Syntax
module S =
struct
  let name = Name.mk "s19"

  let comp_length s1 s2 =
    let c = compare (String.length s2) (String.length s1) in
    if c <> 0 then c  else compare s1 s2
  let read_input () =
    let s = read_line () in
    let ls = String.split_on_char ',' s in
    let map = ~%[] in
    List.iter (fun s -> let s = String.trim s in
                map.%{s.[0]} <- List.sort comp_length (s :: (map.%?{s.[0]} or []));
              ) ls;
    let  _ = read_line () in

    map, Input.fold_lines (fun acc s -> s :: acc) [] |> List.rev

  let explore f map txt =
    let len = String.length txt in
    let rec loop i acc =
      if i = len then f acc
      else
        try
          map.%{txt.[i]}
          |> List.iter (fun p ->
              if String.compare_from txt i p = 0 then
                let i' = i + String.length p in
                if i' <= len then
                  loop i' (p::acc)
            )
        with Not_found -> ()
    in loop 0 []

  let count_valid map l =
    List.fold_left (Agg.Left.sum (fun txt ->
        try explore (fun _ -> raise Exit) map txt;0
        with Exit -> 1)) 0 l

  let explore_all cache map txt =
    let len = String.length txt in
    let rec loop i =
      if i = len then 1
      else
        let key = String.sub txt i (len - i) in
        try cache.%{key} with Not_found ->
          let r =
            try
              map.%{txt.[i]}
              |> List.fold_left (fun acc p ->
                  if String.compare_from txt i p = 0 then
                    let i' = i + String.length p in
                    if i' <= len then
                      acc + loop i'
                    else acc
                  else acc
                ) 0
            with Not_found -> 0
          in cache.%{key} <- r; r
    in loop 0

  let count_all_comb_valid map l =
    let cache = ~%[] in
    List.fold_left (Agg.Left.sum (explore_all cache map)) 0 l

  let solve count =
    let map, l = read_input () in
    let n = count map l in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  let solve_part1 () = solve count_valid
  let solve_part2 () = solve count_all_comb_valid

end

let () = Solution.register_mod (module S)