open Utils
open Syntax
module S =
struct
  let name = Name.mk "s19"

  let read_repl s =
    Scanf.sscanf s "%s => %s" (fun x y -> (x,y))
  let read_input () =
    let repls = 
      InputUntil.list_lines (fun s -> if s <> "" then Some s else None)
    in
    let mol = Input.read_line () in
    let t = ~%[] in
    repls |> List.iter (fun s ->
        let src, dst = read_repl s in
        t.%{src} <- dst :: (t.%?{src} or [])
      );
    t, mol

  type kind = Term of string | Rule of string

  let count_variants table mol start stop =
    let variants = ~%[] in
    for i = start to stop do
      table |> Hashtbl.iter (fun k rl ->
          if String.compare_from mol i k = 0 then
            let prefix = String.sub mol 0 i in
            let ssuf = i+String.length k in
            let suffix = String.sub mol ssuf (String.length mol - ssuf) in
            rl |> List.iter (fun r -> variants.%{prefix ^ r ^ suffix} <- ())
        )
    done;
    variants

  let solve_part1 () =
    let table, mol = read_input () in
    let n = count_variants table mol 0 (String.length mol - 1)|> Hashtbl.length in
    Solution.printf "%d" n

  (* For part2, inverting the table and reducing from the end works,
     unexpectedly.
  *)

  let invert_table table =
    let res = ~%[] in
    Hashtbl.iter (fun k l ->
        List.iter (fun s -> res.%{s} <- k) l
      ) table;
    res

  let count_steps orig_table mol target =
    let table = invert_table orig_table in
    let exception Found of string in
    let rec loop n mol =
      if mol = target then n
      else
        try
          for i =String.length mol -1 downto 0 do
            table |> Hashtbl.iter (fun k v ->
                if String.compare_from mol i k = 0 then
                  let prefix = String.sub mol 0 i in
                  let ssuf = i+String.length k in
                  let suffix = String.sub mol ssuf (String.length mol - ssuf) in
                  raise_notrace (Found (prefix ^ v ^ suffix))
              )
          done;
          assert false
        with Found nmol -> loop (n+1) nmol
    in
    loop 0 mol


  let solve_part2 () =
    let table, mol = read_input () in
    let n = count_steps table mol "e" in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)