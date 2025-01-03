open Utils
module S =
struct
  let name = Name.mk "s05"

  let read_input () = Input.read_line ()

  let is_opposite c1 c2 =
    abs (Char.code c1 - Char.code c2) = 32

  let react ?(remove='\128') l =
    let cr = Char.code remove in
    let rec loop lf lb =
      match lf, lb with
        [], _-> List.rev lb
      | c :: llf, _ when c = remove || Char.code c - 32 = cr -> loop llf lb
      | c1 :: llf, c2 :: llb when is_opposite c1 c2 -> loop llf llb
      | c:: llf , _ -> loop llf (c::lb)
    in
    loop l []

  let find_bad_unit l =
    let min_size = ref max_int in
    for i = Char.code 'A' to Char.code 'Z' do
      min_size := min !min_size (List.length (react ~remove:(Char.chr i) l));
    done;
    !min_size

  let solve_part1 () =
    let s = read_input () in
    let l = react (String.explode s) in
    let n = List.length l in
    Solution.printf "%d" n
  let solve_part2 () =
    let s = read_input () in
    let n = find_bad_unit (String.explode s) in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)