open Utils
open Syntax
module S =
struct
  module G = Grid.BytesGrid
  let name = Name.mk "s24"


  let step grid =
    let dest = G.copy grid in
    G.iter (fun p c ->
        let spaces = ref 4 in
        G.iter4 (fun _ x _ -> if x = '#' then decr spaces) grid p;
        if c = '#' && !spaces <> 3 then dest.G.!(p) <- '.';
        if c = '.' && (!spaces = 2 || !spaces = 3) then dest.G.!(p) <- '#';
      ) grid;
    dest

  let score grid =
    let i = ref 0 in
    let acc =ref 0 in
    G.iter (fun _ c -> if c = '#' then acc := !acc + (1 lsl !i); incr i) grid;
    !acc

  let pp fmt g =
    G.iter_lines Format.(fprintf fmt "%a\n" pp_print_bytes) g

  let evolve grid =
    let cache = ~%[] in
    let rec loop grid s =
      if cache %? s then s else begin
        Format.printf "%a\n--\n%!" pp grid;
        let grid' = step grid in
        let s' = score grid' in
        cache.%{s} <- s';
        loop grid' s'
      end
    in
    loop grid (score grid)

  let solve_part1 () =
    let grid = G.read () in
    let n = evolve grid in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)


  module Coord =
  struct
    type t = int * int * int

    let indices = [ -2; -1; 0; 1; 2 ]
    let left (l, x, y) =
      if x = -2 then [(l+1, -1, 0)]
      else if x = 1 && y = 0 then indices |> List.map (fun y -> (l-1,2, y))
      else [(l, x-1, y)]

    let right (l, x, y) =
      if x = 2 then [(l+1, 1, 0)]
      else if x = -1 && y = 0 then indices |> List.map (fun y -> (l-1,-2, y))
      else [(l, x+1, y)]

    let up (l, x, y) =
      if y = -2 then [(l+1,0,-1)]
      else if y = 1 && x = 0 then indices |> List.map (fun x -> (l-1,x,2))
      else [(l, x, y-1)]

    let down (l, x, y) =
      if y = 2 then [(l+1,0,1)]
      else if y = -1 && x = 0 then indices |> List.map (fun x -> (l-1,x,-2))
      else [(l, x, y+1)]

    let neighbours pos =
      List.flatten (List.map (fun m -> m pos) [ up; right; left; down])
  end


  let rec_step min_l max_l grid =
    let open Coord in
    let dest = ~%[] in
    for l = max_l downto min_l do
      for y = -2 to 2 do
        for x = -2 to 2 do
          if x <> 0 || y <> 0 then
            let pos = (l, x, y) in
            let bug = grid.%?{pos} or false in
            let nl = neighbours pos in
            let nbugs = List.fold_left (fun acc p -> acc + int_of_bool (grid.%?{p} or false)) 0 nl in
            if bug then if nbugs <> 1 then dest.%{pos} <- false else dest.%{pos} <- true
            else if (nbugs = 1 || nbugs = 2) then dest.%{pos} <- true
            else dest.%{pos}<- false
        done;
      done;
    done;
    dest
  let iterate grid n =
    let hgrid = ~%[] in
    G.iter (fun (x, y) c ->
        if x <> 2 || y <> 2 then
          hgrid.%{0,x-2,y-2} <- c = '#') grid;

    let rec loop min_l max_l grid n =
      if n = 0 then Hashtbl.fold (fun _ b acc -> acc + int_of_bool b) grid 0
      else
        loop (min_l - 1) (max_l+1) (rec_step min_l max_l grid) (n-1)
    in
    loop (-20) (20) hgrid n

  let solve_part2 () =
    let grid = G.read () in
    let n = iterate grid 200 in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)
end

let () = Solution.register_mod (module S)