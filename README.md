# Tchou's [advent of Code repository](https://adventofcode.com/)

## General usage
Invoke the code with:
```shell
$ dune exec -- bin/main.exe [prefix] <sol> [1|2] [variant] < input.txt
```
where:
    - `prefix` is an optional prefix of the form `aocXXXX`. If absent, it defaults to the current year, e.g. `aoc2024`
    - `sol` is the mandatory name of the solution, usually of the form `s17` (for problem 17)
    - `1|2` is either `1` for part 1 and `2` for part 2. If absent, it defaults to `1`.
    - `variant` is the name of a variant for some problems, e.g. `debug` which runs the solution in debug mode or `animate` which runs an animation in ascii art.

The program expects the problem input from `stdin`.

To list all prefix/solutions/variants, simply run:
```shell
$ dune exec -- bin/main.exe list
```

## Code structure

The repository is organized as follows:
- `aoc_private` : a private submodule that contains a script to locally sync 
my personal problem inputs (the problems/inputs are themselves not on github).
- `bin` the directory containing the main executable
- `lib` the directory containing utility libraries. Besides helpers to register
  solutions, it contains [various utility functions](lib/utils.mli) to easily
  parse input, perform common tasks, extend the OCaml standard library in a few
  places, provide convenient syntax extensions, and implement once and for all a
  few common math and graph algorithms.

- `solutions` contains the solutions to the problems.

## Adding a new year

 To add a new year, create
a directory `aoc20XX` in `solutions`. Inside it, symlink the file `template/dune`.
Copy the file `template/template.ml` as `sYY.ml` and inside change the name of
the solution:
```ocaml
open Utils
module S =
struct
  let name = Name.mk "sYY" (* change to e.g. s04 *)
  let solve_part1 () = ()  (* Code for part 1 *)
  let solve_part2 () = ()  (* Code for part 2 *)
end

let () = Solution.register_mod (module S)

(* optional, if one wants to register alternative versions with a name *)
module V = struct
   let solve_part1 () =
      S.solve ~display:true () (* call the main function with alternative parameters *)
    
   let solve_part2 () = ()
  end
let () = Solution.register_mod ~variant:"animate" (module V)
```

## License

See the file [LICENSE](LICENSE) for details.
