open Utils
open Syntax

module S =
struct
  let name = Name.mk "s06"

  let set tab c =
    tab.$[Char.code c - Char.code 'a'] <- '\x01'

  let incr_ tab c =
    let idx = Char.code c - Char.code 'a' in
    let n = 1 + Bytes.get_int8 tab idx in
    Bytes.set_int8 tab idx n

  let reset tab =
    for i = 0 to Bytes.length tab - 1 do
      tab.$[i] <- '\x00'
    done
  let sum =
    Bytes.fold_left (Agg.Left.sum Char.code) 0

  let count_distinct tab l =
    let count = ref 0 in
    for i = 0 to Bytes.length tab - 1 do
      let v = Bytes.get_int8 tab i in
      if v = l then incr count
    done;
    !count
  let solve_part1 () =
    let tab = Bytes.make (Char.code 'z' + 1) '\x00' in
    let acc = Input.fold_lines (fun acc s ->
        if s = "" then begin
          let acc = acc + sum tab in
          reset tab;
          acc
        end else begin
          String.iter (fun c -> set tab c) s;
          acc
        end
      ) 0
    in
    Solution.printf "%d" acc

  let solve_part2 () =
    let tab = Bytes.make (Char.code 'z' + 1) '\x00' in
    let acc,_ = Input.fold_lines (fun (acc,l) s ->
        if s = "" then begin
          let acc = acc + count_distinct tab l in
          reset tab;
          acc,0
        end else begin
          String.iter (fun c -> incr_ tab c) s;
          acc,l+1
        end
      ) (0,0)
    in
    Solution.printf "%d" acc

end

let () = Solution.register_mod (module S)