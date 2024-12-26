open Utils

module S =
struct
  let name = Name.mk "s07"

  let fuel_cost array pos phi =
    let cost = ref 0 in
    for i = 0 to Array.length array - 1 do
      cost := !cost + array.(i) * phi (abs (pos - i));
    done;
    !cost

  let solve phi =
    let crabs = Input.read_line ()
                |> String.split_on_char ','
                |> List.map int_of_string
    in
    let top = List.fold_left max 0 crabs in
    let array = Array.make (top+1) 0 in
    List.iter (fun i -> array.(i) <- array.(i) + 1) crabs;
    let min_cost = ref max_int in
    let min_pos = ref 0 in
    for i = 0 to Array.length array - 1 do
      let c = fuel_cost array i phi in
      if c < !min_cost then begin
        min_cost := c;
        min_pos := i;
      end
    done;
    Solution.printf "%d" !min_cost
  let solve_part1 () = solve (fun x -> x)

  let solve_part2 () = solve (fun x -> x * (x + 1) / 2)
end

let () = Solution.register_mod (module S)