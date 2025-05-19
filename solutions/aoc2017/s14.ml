open Utils
module S =
struct
  let name = Name.mk "s14"

  let hash = S10.S.knot_hash

  let count_bits n =
    let rec loop n acc =
      if n = 0 then acc
      else loop (n lsr 1) (acc + (n land 1))
    in
    loop n 0
  let popcount =
    let t = Array.init 16 count_bits in
    fun i -> assert (i >= 0 && i < 16); Array.unsafe_get t i

  let hex = function 
    | '0'..'9' as c -> Char.code c - Char.code '0'
    | 'a'..'f' as c -> 10 + Char.code c - Char.code 'a'
    | _ -> assert false 

  let iter_hashes f seed =
    let seed = seed ^ "-" in
    for i = 0 to 127 do
      let s = seed ^ string_of_int i in
      let h = hash s in
      for j = 0 to 31 do
        for k = 3 downto 0 do
          let c = Bytes.unsafe_get h j in
          f ( ((hex c) lsr k) land 1 <> 0);
        done;
      done;
    done

  let count_bits_in_hashes seed =
    let total = ref 0 in
    let f b = if b then incr total in
    iter_hashes f seed;
    !total

  let solve_part1 () =
    let s = Input.read_line () in
    let n = count_bits_in_hashes s in
    Solution.printf "%d" n


  module G = Grid.IntGrid
  let mk_grid seed =
    let grid = G.init 128 (fun _ -> Array.make 128 0) in
    let y = ref 0 in
    let x = ref 0 in
    let f b =
      if b then grid.G.!(!x, !y) <- -1;
      incr x;
      if !x = 128 then begin
        x := 0;
        incr y;
      end
    in
    iter_hashes f seed;
    grid

  let count_regions grid =
    let num_region = ref 0 in
    let rec dfs pos =
      if grid.G.!(pos) = -1 then begin
        grid.G.!(pos) <- !num_region;
        G.iter4 (fun pos' _ _ -> 
            dfs pos') grid pos
      end
    in
    G.iter (fun pos c -> 
        if c = -1 then begin
          incr num_region;
          dfs pos;
        end
      ) grid;
    !num_region
  let solve_part2 () =
    let s = Input.read_line () in
    let n = count_regions (mk_grid s) in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)