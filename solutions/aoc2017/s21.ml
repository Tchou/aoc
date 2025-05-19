open Utils
open Syntax
module S =
struct
  let name = Name.mk "s21"

  module G = Grid.StringGrid

  let pp_grid fmt =
    G.iter_lines (Format.fprintf fmt "%s\n%!")

  type t = { size : int;
             cell_size : int;
             grid : G.t array array 
           }

  type s = 
      G of G.t
    | V of {
        size : int;
        cell_size : int; (* size/cell_size elements of size cell_size *)
        depth : int;
        grid : s array array 
      }

  let rec get g i j =
    match g with
      G g -> g.G.!(i, j)
    | V { cell_size; grid;_ } ->
      let x = i / cell_size in
      let y = j / cell_size in
      let b = grid.(y).(x) in
      get b (i mod cell_size) (j mod cell_size)

  let (.@@()) g (i, j) = get g i j
  
  let (.@()) g (i, j) =
    let x = i / g.cell_size in
    let y = j / g.cell_size in
    let b = g.grid.(y).(x) in
    b.G.!(i mod g.cell_size, j mod g.cell_size)

  let sub g i j s =
    G.init s (fun y ->
        String.init s (fun x -> g.@(i+x, j+y)))

  let pp fmt t =
    for j = 0 to t.size - 1 do
      if j <> 0 && j mod t.cell_size = 0 then begin
        for i = 0 to t.size - 1 do
          let c = if i <> 0 && i mod t.cell_size= 0 then "+-" else "-" in
          Format.fprintf fmt "%s" c
        done;
        Format.fprintf fmt "\n";
      end;  
      for i = 0 to t.size - 1 do
        if i <> 0 && i mod t.cell_size = 0 then Format.fprintf fmt "|";
        Format.fprintf fmt "%c" t.@(i,j);
      done;
      Format.fprintf fmt "\n";
    done

  let pp_rules fmt r =
    let cmp (g1,_) (g2,_) =
      let c = Int.compare G.(width g1 * height g1) G.(width g2 * height g2) in
      if c <> 0 then c else
        G.compare g1 g2
    in
    let l = Hashtbl.to_seq r |> List.of_seq |> List.sort cmp in
    List.iter(fun (g, t) ->
        Format.fprintf fmt "RULE:\n%a\nOUTPUT:\n%a\n--\n%!" pp_grid g pp_grid t) l
  let parse_grid l =
    let lines = String.split_on_char '/' l |> Array.of_list in
    G.init (Array.length lines) (Array.get lines)
  let sym g =
    let g1 = G.rotate_left g in
    let g2 = G.rotate_left g1 in
    let g3 = G.rotate_left g2 in
    List.fold_left (fun acc g ->
        g :: G.vertical_flip g :: G.horizontal_flip g :: acc)
      [] [g;g1;g2;g3]

  let read_input () =
    let h = ~%[] in
    Input.fold_scan "%[.#/] => %[.#/]" (fun acc s d ->
        let gs = parse_grid s in
        let gd = parse_grid d in
        List.iter (fun g -> h.%{g} <- gd) (sym gs)
      ) ();
    h
  let init = {
    size = 3;
    cell_size = 3;
    grid = [| [|G.init 3 (function
          0 -> ".#."
        | 1 -> "..#"
        | 2 -> "###"
        | _ -> assert false) |] |]
  }
  let dummy = G.init 0 (fun _ -> "")
  let apply rules g =
    let split = if g.size mod 2 = 0 then 2 else 3 in
    let cell_size = if split = 2 then 3 else 4 in
    let len = g.size / split in
    let size = len * cell_size in
    let grid = Array.make_matrix len len dummy in
    for j = 0 to len - 1 do
      for i = 0 to len - 1 do
        let s = sub g (i*split) (j*split) split in
        let ns = rules.%{s} in
        grid.(j).(i) <- ns
      done;
    done;
    {size; cell_size; grid}

  let rec iterate rules g n =
    if n = 0 then g else
      let g' = apply rules g in
      let () = if false then Format.printf "%a\n--\n%!" pp g' in
      iterate rules g' (n-1)

  let count g =
    let total = ref 0 in
    for j = 0 to g.size - 1 do
      for i = 0 to g.size - 1 do
        if g.@(i,j) = '#' then incr total
      done;
    done;
    !total

  let solve k =
    let rules = read_input () in
    let n = count (iterate rules init k) in
    Solution.printf "%d" n

  let solve_part1 () = solve 5
  let solve_part2 () = solve 18
end

let () = Solution.register_mod (module S)