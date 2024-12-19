open Utils
module S =
struct
  let name = Name.mk "s04"
  let read_input () =
    Input.fold_scan "%d-%d" (fun _ a b -> (a, b)) (0, 0)

  let valid_number1 n =
    let rec loop double n =
      if n < 10 then double else
        let d1 = n mod 10 in
        let d2 = (n / 10) mod 10 in
        d2 <= d1 && loop (double || d1 = d2) (n/10)
    in
    loop false n

    (*
    XXXXXX
    111.111
    11.111
    1.111
    .111
    *)
  let valid_number2 n =
    valid_number1 n &&
    let d1 = n mod 10 in
    let d2 = (n / 10) mod 10 in
    let d3 = (n / 100) mod 10 in
    let d4 = (n / 1000) mod 10 in
    let d5 = (n / 10000) mod 10 in
    let d6 = (n / 100000) mod 10 in
    (d1 = d2 && d1 <> d3) ||
    (d2 = d3 && d2 <> d4 && d2 <> d1) ||
    (d3 = d4 && d3 <> d5 && d3 <> d2) ||
    (d4 = d5 && d4 <> d6 && d4 <> d3) ||
    (d5 = d6 && d6 <> d4)
  let count_valid_numbers valid_number i j =
    let count = ref 0 in
    for k = i to j do
      count := !count + int_of_bool (valid_number k);
    done;
    !count

  let solve f =
    let n1, n2 = read_input () in
    let n = count_valid_numbers f n1 n2 in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part1 () = solve valid_number1

  let solve_part2 () = solve valid_number2
end

let () = Solution.register_mod (module S)