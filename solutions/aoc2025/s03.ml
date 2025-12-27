open Utils
module S =
struct
  let name = Name.mk "s03"

  let digit d = Char.(code d - code '0')
  let read_input () =
    Input.list_lines (String.explode_array)
    |> List.map (Array.map digit)

  let max_between tab i j =
    let max_d = ref 0 in
    let max_i = ref (-1) in
    for i = i to j do
      let d = tab.(i) in
      if d > !max_d then begin
        max_d := d;
        max_i := i;
      end;
    done;
    !max_d, !max_i

  let find_digits size tab =
    let last = Array.length tab - 1 in
    let rec loop i ci acc = 
      if ci >= size then acc
      else
        let d, ni = max_between tab i (last-(size-ci)+1) in
        loop (ni + 1) (ci + 1) (acc * 10 +  d)
    in
    loop 0 0 0
  let sum_of_max size l =
    Iter.(l
           |> list 
           |> map (find_digits size)
           |> sum int)
  let solve size () =
    let l = read_input () in
    let n = sum_of_max size l in
    Solution.printf "%d" n
  let solve_part1 = solve 2
  let solve_part2 = solve 12
end

let () = Solution.register_mod (module S)