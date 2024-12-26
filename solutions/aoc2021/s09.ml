open Utils
open Syntax


module S =
struct
  let name = Name.mk "s09"

  let load_grid () =
    Input.fold_lines (fun acc l ->
        l::acc) []
    |> List.rev
    |> Array.of_list

  let adjacent nr nc r c =
    let l = [(1,0); (-1,0); (0,1); (0, -1)]in
    List.filter_map (fun (i, j) ->
        let ri = i + r in
        let cj = j + c in
        if ri < 0 || cj < 0 || ri >= nr || cj >= nc then None
        else Some (ri, cj)
      ) l


  let solve_part1 () =
    let grid = load_grid () in
    let res = ref 0 in
    let nr = Array.length grid in
    for i = 0 to nr - 1 do
      let nc = String.length grid.(i) in
      for j = 0 to nc - 1 do
        let v = grid.(i).[j] in
        let adj = adjacent nr nc i j in
        if List.for_all (fun (ai, aj) ->
            grid.(ai).[aj] > v) adj then
          res := Char.code v - Char.code '0' + 1 + !res;
      done;
    done;
    Solution.printf "%d" !res

  let rec find_basin grid nr nc r c visited count =
    if not (visited %? (r, c)) then begin
      visited.%{r, c} <- ();
      if grid.(r).[c] <> '9' then begin
        incr count;
        adjacent nr nc r c
        |> List.iter (fun (r', c') -> find_basin grid nr nc r' c' visited count)
      end;
    end

  let solve_part2 () =
    let grid = load_grid () in
    let res = ref [] in
    let visited = ~%[] in
    let nr = Array.length grid in
    for i = 0 to nr - 1 do
      let nc = String.length grid.(i) in
      for j = 0 to nc - 1 do
        let count = ref 0 in
        find_basin grid nr nc i j visited count;
        res := !count :: !res;
      done;
    done;
    match List.sort (fun a b -> compare b a) !res with
      x :: y :: z :: _ ->
      Solution.printf "%d" (x * y * z)
    | _ -> assert false
end

let () = Solution.register_mod (module S)