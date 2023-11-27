open Utils
open Syntax

module S =
struct
  let name = Name.mk "s08"
  let load_input () =
    let dirs =
      String.map
        (function 'L' -> '\x00' | _ -> '\x01')
        (read_line ())
    in
    let _ = read_line () (* empty*) in
    let map = ~%[] in
    Input.fold_scan
      "%[^ ] = (%[^,], %[^)])"
      (fun () s d0 d1 -> map.%{s} <- [|d0;d1|]) ();
    (dirs, map)

  let walk dirs map start =
    let dirs_len = String.length dirs in
    let rec loop i count node =
      if node.[2] = 'Z' then count
      else
        let d = String.get_uint8 dirs i in
        let dst = map.%{node}.(d) in
        loop ((i+1) mod dirs_len) (count+1) dst
    in
    loop 0 0 start

  let solve_part1 () =
    let dirs, map = load_input () in
    walk dirs map "AAA"
    |> Ansi.printf "%d\n"
  let solve_part2 () =
    let dirs, map = load_input () in
    Hashtbl.to_seq_keys map
    |> List.of_seq
    |> List.filter (fun s -> s.[2] = 'A')
    |> List.rev_map (walk dirs map)
    |> List.fold_left Math.lcm 1
    |> Ansi.printf "%d\n"
end

let () = Solution.register_mod (module S)