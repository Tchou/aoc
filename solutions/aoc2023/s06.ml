open Utils
module S =
struct
  let name = Name.mk "s06"

  let read_numbers () =
    Input.read_line ()
    |> String.split_on_char ' '
    |> List.filter_map int_of_string_opt

  let read_numbers2 () =
    Input.read_line ()
    |> String.split_on_char ' '
    |> List.filter_map int_of_string_opt
    |> List.map string_of_int
    |> String.concat ""
    |> int_of_string

  let load_input read_numbers =
    let times = read_numbers () in
    let records = read_numbers () in
    times, records

  (* total distance is x*(t-x), we want to solve
     x*(t-x) > r
     x^2 - tx + r < 0
     x : (t +- sqrt(t^2-4r))/2
     we need to be careful, solutions that are integers should
     be excluded since we want to win, not tie.
  *)
  let solve_equation t r =
    let tf = float t in
    let tr = float r in
    let delta = (sqrt (tf *.tf -. 4. *. tr)) in
    let root1 = floor (0.5 *. (tf -. delta) +. 1.0) in
    let root2 = ceil (0.5 *. (tf +. delta) -. 1.0) in
    1 + int_of_float (root2 -. root1)

  let solve_part1 () =
    let times, records = load_input read_numbers in
    List.fold_left2 (fun acc t r -> acc * solve_equation t r) 1 times records
    |> Solution.printf "%d"

  let solve_part2 () =
    let t, r = load_input read_numbers2 in
    solve_equation t r
    |> Solution.printf "%d"

end

let () = Solution.register_mod (module S)