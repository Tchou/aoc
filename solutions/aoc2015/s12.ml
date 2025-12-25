open Utils
open Yojson
module S =
struct
  let name = Name.mk "s12"

  let read_input () = 
    Input.read_line ()
    |> Basic.from_string

  let rec add_integers skip (json : Basic.t) =
    match json with 
      `Int i -> i
    | `Float _ | `Bool _ | `Null | `String _ -> 0
    | `List l -> List.fold_left (fun acc j -> acc + add_integers skip j) 0 l
    | `Assoc l -> if skip l then 0
      else 
        List.fold_left (fun acc (_,j) -> acc + add_integers skip j) 0 l


  let solve skip () =
    let json = read_input () in
    let n = add_integers skip json in
    Solution.printf "%d" n


  let solve_part1 = solve (Fun.const false)
  let solve_part2 = solve (List.exists (fun (_, v) -> v = `String "red"))
end

let () = Solution.register_mod (module S)