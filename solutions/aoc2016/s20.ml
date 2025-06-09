open Utils
module S =
struct
  let name = Name.mk "s20"

  let read_input () =
    Input.list_scan "%d-%d" (fun x y -> assert (x <= y); x, y)

  let add (x, y) l =
    let rec loop (x, y) l =
      match l with
        [] -> [ (x, y) ]
      | (a, b) :: ll ->
        if y < a - 1 then (x, y) :: l
        else if x > b + 1 then (a, b) :: loop (x, y) ll 
        else loop (min a x, max b y) ll
    in
    loop (x, y) l


  let min_outside l =
    let rec loop lim l =
      match l with
        [] -> lim
      | [ (x, y) ] ->
        if lim < x then lim else (y+1)
      | (x, y) :: ll ->
        if lim < x then lim else
          loop (y+1) ll
    in
    loop 0 l

  let merge l = List.fold_left (fun acc e -> add e acc) [] l
  let cardinal l = 
    List.fold_left (fun acc (x, y) -> acc + (y-x+1)) 0 l

  let solve_part1 () =
    let l = read_input () in
    let l = merge l in
    let n = min_outside l in
    Solution.printf "%d" n

  let solve_part2 () =
    let l = read_input () in
    let l = merge l in
    let n = (1 lsl 32) - cardinal l in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)