open Utils
open Syntax

module S =
struct
  let name = Name.mk "s04"

  let load_input valid =
    Input.fold_lines (fun (accn, acch) s ->
        if s = "" then begin
          let nh = Hashtbl.create 16 in
          if valid acch then
            (accn+1, nh)
          else (accn, nh)
        end else begin
          String.split_on_char ' ' s
          |> List.iter (fun f ->
              match String.split_on_char ':' f with
                [k;v] -> acch.%{k} <-v
              | _ -> assert false );
          (accn, acch)
        end
      ) (0, Hashtbl.create 16)

  let valid1 h =
    let n = Hashtbl.length h in
    n = 8 || (n=7 && not (h %? "cid"))

  let valid2 h =
    let check_height s =
      if String.ends_with ~suffix:"cm" s then
        let n = int_of_string (Filename.chop_suffix s "cm") in
        n >= 150 && n <= 193
      else if String.ends_with ~suffix:"in" s then
        let n = int_of_string (Filename.chop_suffix s "in") in
        n >= 59 && n <= 76
      else false
    in

    try
      let byr = int_of_string h.%{"byr"} in
      byr >= 1920 && byr <= 2002 &&
      let iyr = int_of_string h.%{"iyr"} in
      iyr >= 2010 && iyr <= 2020 &&
      let eyr = int_of_string h.%{"eyr"} in
      eyr >= 2020 && eyr <= 2030 &&
      check_height h.%{"hgt"} &&
      Scanf.sscanf h.%{"hcl"} "#%[0-9a-f]" (fun s -> String.length s = 6) &&
      List.mem h.%{"ecl"} [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] &&
      Scanf.sscanf h.%{"pid"} "%[0-9]" (fun s -> String.length s = 9)
    with _ ->
      false
  let solve_part1 () =
    let n, _ = load_input valid1 in
    Ansi.printf "%d\n" n
  let solve_part2 () =
    let n, _ = load_input valid2 in
    Ansi.printf "%d\n" n

end

let () = Solution.register_mod (module S)