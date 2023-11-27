open Utils
open Syntax

module S =
struct
  let name = Name.mk "s25"

  let load_input () =
    Input.fold_lines (fun acc s -> Bytes.of_string s :: acc) []
    |> List.rev |> Array.of_list

  let simulate grid =
    let moved = ref false in
    let height = Array.length grid in
    let width = Bytes.length grid.(0) in
    let step kind next =
      let todo = ref [] in
      for r = 0 to height - 1 do
        for c = 0 to width - 1 do
          let k = grid.(r).$[c] in
          if k = kind then
            let (r', c') as p' = next (r, c) in
            if grid.(r').$[c'] = '.' then
              todo := ((r, c),p') :: !todo;
        done;
      done;
      moved := !moved || !todo <> [];
      List.iter (fun ((r, c), (r', c')) ->
          grid.(r).$[c] <- '.';
          grid.(r').$[c'] <- kind;)
        !todo
    in
    let next_h (r, c) = (r, (c + 1) mod width) in
    let next_v (r, c) = ((r+1) mod height, c) in
    let rec loop n =
      moved := false;
      step '>' next_h;
      step 'v' next_v;
      if !moved then loop (n+1) else n
    in
    loop 1

  let solve_part1 () =
    load_input()
    |> simulate
    |> Ansi.printf "%d\n"
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)