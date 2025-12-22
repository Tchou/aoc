open Utils
open Syntax

module S =
struct
  let name = Name.mk "s03"

  let read_input () = Input.read_line ()

  let count_houses  ?(pred=(fun _ -> true)) t dirs =
    let i = ref 0 in
    String.fold_left (fun pos c ->
        let res = 
          if pred !i then begin 
            let d = match c with
                '^' -> Grid.north
              |'>' -> Grid.east
              |'<' -> Grid.west
              |'v' -> Grid.south
              | _ -> assert false
            in
            let pos' = Grid.(pos +! d) in
            t.%{pos'} <- 1 + (t.%?{pos'} or 0);
            pos'
          end else pos in 
        incr i;
        res
      ) (0,0) dirs |> ignore;
    Hashtbl.length t

  let solve_part1 () =
    let dirs = read_input () in
    let n = count_houses ~%[(0,0), 1] dirs in
    Solution.printf "%d" n
  let solve_part2 () = 
    let dirs = read_input () in
    let t = ~%[(0,0), 1] in
    let _ = count_houses t ~pred:(fun i -> i mod 2 = 0) dirs in
    let n = count_houses t ~pred:(fun i -> i mod 2 = 1) dirs in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)