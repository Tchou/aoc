open Utils
module S =
struct
  let name = Name.mk "s09"

  let read_input () =
    Input.list_scan "%d,%d" (fun x y -> x,y)
    |> Array.of_list

  let dist a b = 1 + abs (a - b)

  let area (x1, y1) (x2, y2) = 
    (dist x1 x2) * (dist y1 y2)

  let largest_rectangle t =
    let max_a = ref 0 in
    let len = Array.length t in
    for i = 0 to len - 1 do
      let c1 = t.(i) in
      for j = i+1 to len - 1 do
        let a = area c1 t.(j) in
        if a > !max_a then max_a := a;  
      done
    done;
    !max_a

  let solve_part1 () =
    let tiles = read_input () in
    let n = largest_rectangle tiles in
    Solution.printf "%d" n

  let inside_rectangle ((x1 : int), (y1 : int)) (x2, y2) (x, y) =
    let x1, x2 = if x1 > x2 then x2, x1 else x1, x2 in
    let y1, y2 = if y1 > y2 then y2, y1 else y1, y2 in
    x1 < x && x < x2 && y1 < y && y < y2

  (* Generate the array of all the points on the perimeter.
     It's important to use a shuffle it otherwise we will most of the time
     look for points far appart the rectangle we test. *)

  let all_points t =
    let h = ref [] in
    let len = Array.length t in
    for i = 0 to len - 1 do 
      let x1, y1 = t.(i) in
      let x2, y2 = t.((i+1) mod len) in
      if x1 == x2 then 
        for y = min y1 y2 to max y1 y2 do
          h := (x1, y) :: !h;
        done
      else 
        for x = min x1 x2 to max x1 x2  do
          h := (x, y1) :: !h
        done;
    done;
   let a =  !h |> Array.of_list in 
   a |> Array.shuffle ~rand:Random.int; a

  (* We iterate all the rectangles and test if a point of the polygon
    is *strictly inside* 
    Note, this wouldn't work for:
   
    O  Inside        Inside
    XOOOOOOOOOOOOOOO
                   O  Inside
      Outside      O
                   O
    OOOOOOOOOOOOOOOX

    The rectangle made by the two Xs is invalid, but our criteria would not find
    any point in the permitter that is inside. I was prepared to handle that case
    but my input does not need it it seems.
  *)
  let largest_rectangle_within t =
    let len = Array.length t in
    let all_points = all_points t in
    let max_a = ref 0 in
    for i = 0 to len - 1 do
      let (x1, y1) as c1 = t.(i) in
      for j = i+1 to len - 1 do
        let (x2, y2) as c2 = t.(j) in
        let a = area c1 c2 in
        if a > !max_a then
          if not (Array.exists (inside_rectangle c1 c2) all_points) then max_a := a; 
      done
    done;
    !max_a

  let solve_part2 () =
    let tiles = read_input () in
    let n = largest_rectangle_within tiles in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)