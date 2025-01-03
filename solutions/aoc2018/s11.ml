open Utils
module S =
struct
  module G = Grid.IntGrid
  let name = Name.mk "s11"

  let cell sn x y =
    let n = x + 10 in
    let n = n * y in
    let n = n + sn in
    let n = n * (x + 10) in
    let n = (n / 100) mod 10 in
    n - 5

  (* https://en.wikipedia.org/wiki/Summed-area_table *)

  let read_input () =
    let sn = Input.read_line () |> int_of_string in
    let g_sum = G.init 301 (fun _ -> Array.make 301 0) in
    G.iter_from  (fun (x, y) _ ->
        g_sum.G.!(x, y) <-
          (cell sn x y)
          + g_sum.G.!(x, y - 1)
          + g_sum.G.!(x-1, y)
          - g_sum.G.!(x-1, y-1);
      ) g_sum (1, 1) 300 300;
    sn, g_sum

  let find_max pos_max v_max n g_sum =
    let w = G.width g_sum in
    let h = G.height g_sum in
    let local_max = ref 0 in
    G.iter_from (fun (x, y) _ ->
        let v = g_sum.G.!(x-1, y-1)
                + g_sum.G.!(x+n-1, y+n-1)
                - g_sum.G.!(x+n-1, y-1)
                - g_sum.G.!(x-1, y+n-1)
        in
        local_max := max !local_max v;
        if v > !v_max then begin
          v_max := v;
          pos_max := ((x, y), n);
        end)
      g_sum (1, 1) (w - n - 1) (h - n - 1);
    !pos_max, !local_max

  let find_max_in_range f t g_sum =
    let pos_max = ref ((-1,-1),0) in
    let v_max = ref min_int in
    for i = f to t do
      ignore (find_max pos_max v_max i g_sum);
    done;
    !pos_max

  let solve t f =
    let sn, g = read_input () in
    let (x, y), n = find_max_in_range t f g in
    Solution.printf "%d,%d,%d" x y n

  let solve_part1 () = solve 3 3
  let solve_part2 () = solve 1 300

end

let () = Solution.register_mod (module S)