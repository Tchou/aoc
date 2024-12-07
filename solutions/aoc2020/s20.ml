open Utils
open Syntax
module S =
struct
  let name = Name.mk "s20"

  type tile = { id : int;
                grid : bytes array;
                borders : int array;
              } (* left top right down*)
  let dummy = { id = -1; borders = [||];grid = [||]; }

  let left = 0
  let top = 1
  let right = 2
  let down = 3
  let pp_bin fmt n =
    for i = 9 downto 0 do
      Format.fprintf fmt "%d" ((n lsr i) land 1)
    done 
  let pp_tile fmt tile =
    Format.fprintf fmt "Tile %d:\n" tile.id;
    Array.iter (fun b ->
        Format.pp_print_bytes fmt b;
        Format.pp_print_char fmt '\n') tile.grid;
    Format.fprintf fmt "left: 0b%a\ntop: 0b%a\nright: 0b%a\ndown: 0b%a\n%!"
      pp_bin tile.borders.(left)
      pp_bin tile.borders.(top)
      pp_bin tile.borders.(right)
      pp_bin tile.borders.(down)


  let get grid dir fix var =
    if dir mod 2 = 0 then
      grid.(var).$[fix]
    else
      grid.(fix).$[var]
  let set grid dir fix var v =
    if dir mod 2 = 0 then
      grid.(var).$[fix] <- v
    else
      grid.(fix).$[var] <- v

  let compute_borders grid dir =
    let c = if dir <= 1 then 0 else 9 in
    let acc = ref 0 in
    for i = 0 to 9 do
      let x = int_of_bool ((get grid dir c i)= '#') in
      acc := (!acc lsl 1) lor x
    done;
    !acc
  let update_borders tile =
    for i = 0 to 3 do
      tile.borders.(i) <- compute_borders tile.grid i
    done;
    tile

  let rotate_tile90 tile =
    let grid = Array.map Bytes.copy tile.grid in
    for x = 0 to 9 do
      for y = 0 to 9 do
        grid.(y).$[9-x]<- tile.grid.(x).$[y]
      done
    done;
    update_borders { tile with grid; borders = [|0;0;0;0|]}
  let flip_tile dir tile =
    let grid = Array.map Bytes.copy tile.grid in
    for i = 0 to 9 do
      for j = 0 to 4 do
        let a = get grid dir j i in
        let b = get grid dir (9-j) i in
        set grid dir (9-j) i a;
        set grid dir j i b;
      done
    done;
    update_borders { tile with grid; borders = [|0;0;0;0|]}
  module TileSet = struct
    include Set.Make (
      struct
        type t = tile
        let compare = compare
      end
      )
    let pp fmt ts =
      iter (fun t -> pp_tile fmt t; Format.pp_print_char fmt '\n') ts;
      Format.pp_print_string fmt "--\n"
  end

  let all_versions tile =
    let rec loop set =
      let set_rot =
        TileSet.fold (fun t acc ->
            let acc = TileSet.add t acc in
            let acc = TileSet.add (rotate_tile90 t) acc in
            let acc = TileSet.add (rotate_tile90 (rotate_tile90 t)) acc in
            TileSet.add (rotate_tile90 (rotate_tile90 (rotate_tile90 t))) acc
          ) set TileSet.empty
      in
      let set_fv = TileSet.map (flip_tile 0) set_rot in
      let set_fh = TileSet.map (flip_tile 1) set_rot in
      let new_set = TileSet.(union set_rot (union set_fv set_fh)) in
      if TileSet.(cardinal new_set = cardinal set) then set
      else loop new_set
    in
    loop (TileSet.singleton tile)

  let read_tile () =
    let l = read_line () in
    Scanf.sscanf l "Tile %d:"
      (fun id ->
         let i = ref 0 in
         let borders = [|0;0;0;0|]in
         let grid = Array.make 10 (Bytes.empty) in
         InputUntil.fold_lines (fun acc line ->
             if line = "" then (false, ())
             else
               let () = grid.(!i) <- Bytes.of_string line in
               (true, incr i)
           ) ();
         update_borders{id;grid;borders}
      )

  let read_input () =
    let tiles = ~%[] in
    let rec loop () =
      match read_tile () with
        tile ->
        let set = all_versions tile in
        tiles.%{tile.id} <- set;
        loop ()
      | exception End_of_file -> tiles
    in
    loop ()

  let find_arrangement f tiles =
    let n = Hashtbl.length tiles in
    let k = int_of_float (sqrt (float n)) in
    assert (n = k*k);
    let grid = Array.init k (fun i -> Array.make k dummy) in
    let used = ~%[] in
    let rec loop i j =
      if j = k then f grid
      else
        Hashtbl.iter (fun tid set ->
            if not (used %? tid) then begin
              TileSet.iter (fun tile ->
                  if j = 0 || grid.(j-1).(i).borders.(down) = tile.borders.(top) then
                    if i = 0 || grid.(j).(i-1).borders.(right) = tile.borders.(left) then
                      begin
                        used.%{tile.id}<- ();
                        grid.(j).(i) <- tile;
                        let i', j' = if i < k-1 then i+1, j else 0, (j+1) in
                        loop i' j';
                        grid.(j).(i) <- dummy;
                        used %- tile.id
                      end
                ) set
            end
          ) tiles
    in
    loop  0 0

  let stitch_images grid =
    let k = Array.length grid in
    let ilen = k * 8 (* 10 - minus 2 borders *) in
    let image = Array.init ilen (fun _ -> Bytes.create ilen) in
    let dy = ref 0 in
    let dx = ref 0 in
    let count = ref 0 in
    for j = 0 to k - 1 do
      for sy = 1 to 8 do
        dx := 0;
        for i = 0 to k - 1 do
          for sx = 1 to 8 do
            let c = grid.(j).(i).grid.(sy).$[sx] in
            if c = '#' then incr count;
            image.(!dy).$[!dx] <- c;
            incr dx;
          done;
        done;
        incr dy;
      done;
    done;
    { id = !count; grid = image; borders = [|0;0;0;0|]}

  let has_monster last grid x y =
    let coords = [(0,0); (1,1); (4,1); (5,0); 
                  (6,0); (7,1); (10,1); (11,0);
                  (12,0);(13,1); (16,1); (17,0);
                  (18,0); (18,-1);(19,0)]
    in
    List.for_all (fun (i, j) ->
        let xi = x + i in
        let yj = y + j in
        xi >= 0 && xi <= last &&
        yj >= 0 && yj <= last &&
        grid.(yj).$[xi] = '#') coords

  let count_monsters image =
    let count = ref 0 in
    let last = Array.length image.grid - 1 in
    for y = 1 to last -1 do
      for x = 0 to last - 19 do
        if has_monster last image.grid x y then incr count
      done
    done;
    !count * 15

  exception Found of int
  let find_monsters image =
    let set = all_versions image in
    TileSet.iter (fun img ->
        let c = count_monsters img in
        if c <> 0 then raise (Found (img.id - c))) set
  let solve trigger =
    let tiles = read_input () in
    let n = try find_arrangement trigger tiles;0 with Found k -> k in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part1 () =
    let f grid =
      let k = Array.length grid - 1 in
      raise (Found (grid.(0).(0).id * grid.(0).(k).id *
                    grid.(k).(0).id * grid.(k).(k).id))
    in
    solve f

  let solve_part2 () =
    let f grid =
      let image = stitch_images grid in
      find_monsters image
    in
    solve f

end

let () = Solution.register_mod (module S)