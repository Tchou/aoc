open Utils
module S =
struct
  let name = Name.mk "s04"
  let read_input () =
    Input.fold_lines (fun acc line -> line::acc) []
    |> List.rev
    |> Array.of_list

  let valid w h x y =
    x >= 0 && x < w && y >= 0 && y < h

  let iter_all_dirs grid n x y f  =
    let h = Array.length grid in
    let w = String.length grid.(0) in
    for i = -1 to 1 do
      for j = -1 to 1 do
        if i <> 0 || j <> 0 then
          let xi = ref x in
          let yj = ref y in
          try
            for k = 1 to n do
              xi := !xi + i;
              yj := !yj + j;
              if valid w h !xi !yj then
                f k !xi !yj
            done;
          with Exit -> ()
      done;
    done
  let iter_grid grid f =
    let h = Array.length grid in
    let w = String.length grid.(0) in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        f x y
      done
    done
  let count_xmas grid =
    let count = ref 0 in
    iter_grid grid (fun x y ->
        if grid.(y).[x] = 'X' then
          iter_all_dirs grid 3 x y
            (fun i x y ->
               match i, grid.(y).[x] with
                 1, 'M' | 2, 'A' -> ()
               | 3, 'S' -> incr count
               | _ -> raise Exit
            )
      );
    !count

  let test_oposites_corner grid i j x y f =
    let h = Array.length grid in
    let w = String.length grid.(0) in
    let x1 = x + i in
    let y1 = y + j in
    let x2 = x - i in
    let y2 = y - j in
    valid w h x1 y1 && valid w h x2 y2 &&
    ((f x1 y1 x2 y2) ||
     (f x2 y2 x1 y1))

  let count_2mas grid =
    let count = ref 0 in
    let test_ms x1 y1 x2 y2 =
      grid.(y1).[x1] = 'M' && grid.(y2).[x2] = 'S'
    in
    iter_grid grid (fun x y ->
        if grid.(y).[x] = 'A' &&
           test_oposites_corner grid (-1) (-1) x y test_ms &&
           test_oposites_corner grid (-1)  1 x y test_ms
        then incr count
      );
    !count

  let solve count  =
    let grid = read_input () in
    let n = count grid in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part1 () =
    solve count_xmas

  let solve_part2 () =
    solve count_2mas
end

let () = Solution.register_mod (module S)