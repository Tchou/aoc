open Utils
module S =
struct

  (*

  https://www.redblobgames.com/grids/hexagons/
  We use cube coordinates which have a handy function to compute distance.

  *)
  type hex = { q : int; r : int; s : int }

  let dirs = [
    "n",  { q = 0;  r = -1; s = 1 };
    "ne", { q = 1;  r = -1; s = 0 };
    "se", { q = 1;  r = 0;  s = -1};
    "s",  { q = 0;  r = 1;  s = -1};
    "sw", { q = -1; r = 1;  s = 0 };
    "nw", { q = -1; r = 0;  s = 1 }
  ]
  let origin = { q = 0; r = 0; s = 0 }
  let (+$) a b = { q = a.q+b.q; r=a.r+b.r; s=a.s+b.s}
  let (-$) a b = { q = a.q-b.q; r=a.r-b.r; s=a.s-b.s}
  let dist h1 h2 =
    let d = h1 -$ h2 in
    (abs (d.q) + abs (d.r) + abs(d.s))/2

  let read_input () =
    Input.read_line () |> String.split_on_char ','

  let follow hex l = 
    let max_d = ref 0 in
    let dest = List.fold_left (fun hex d -> 
        let rhex = hex +$ (List.assoc d dirs) in
        max_d := max !max_d (dist origin rhex);
        rhex
      ) hex l
    in dist origin dest, !max_d

  let name = Name.mk "s11"
  let solve f =
    let l = read_input () in
    let n = f (follow origin l) in
    Solution.printf "%d" n

  let solve_part1 () = solve fst
  let solve_part2 () = solve snd
end

let () = Solution.register_mod (module S)