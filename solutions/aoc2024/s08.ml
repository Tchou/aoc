open Utils
open Syntax
module S =
struct
  let name = Name.mk "s08"
  let read_input () =
    Input.fold_lines (fun acc line -> line :: acc) []
    |> List.rev
    |> Array.of_list

  let count_antinodes resonnance grid =
    let h = Array.length grid in
    let w = String.length grid.(0) in
    let rmin,rmax = if resonnance then 0, max h w else 1,1 in
    let valid (x, y) =
      x >= 0 && y >= 0 && x < w && y < w
    in
    let map = ~%[] in
    for j = 0 to h - 1 do
      for i = 0 to w - 1 do
        let c = grid.(j).[i] in
        if c <> '.' then map.%{c} <- (i,j)::(map.%?{c} or [])
      done;
    done;
    let unique_pos = ~%[] in
    map
    |> Hashtbl.iter (fun _ coords ->
        coords
        |> Comb.pairs ~sym:false ~refl:false
        |> Seq.iter (fun ((x1, y1),(x2, y2)) ->
            for r = rmin to rmax do
              let dx = x2 - x1 in
              let dy = y2 - y1 in
              let p1 = (x1 - r * dx, y1 - r * dy) in
              let p2 = (x2 + r * dx, y2 + r * dy) in
              if valid p1 then unique_pos.%{p1} <- ();
              if valid p2 then unique_pos.%{p2} <- ();
            done
          ));
    Hashtbl.length unique_pos
  let solve resonnance =
    let grid = read_input () in
    let n = count_antinodes resonnance grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)


  let solve_part1 () = solve false
  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)