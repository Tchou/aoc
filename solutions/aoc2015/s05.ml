open Utils
module S =
struct
  let name = Name.mk "s05"
  let read_input () =
    Input.list_lines Fun.id

  let is_vowel c =
    c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'

  let is_invalid c1 c2 =
    (c1 = 'a' || c1 = 'c' || c1 = 'p' || c1 = 'x') && 
    (Char.code c1 + 1 = Char.code c2)

  let is_valid1 s =
    let rec loop i last num_vowels has_double =
      if i = last then 
        has_double && (num_vowels + int_of_bool (is_vowel s.[i])) >= 3
      else
        let c1 = s.[i] in
        let c2 = s.[i+1] in
        not (is_invalid c1 c2) &&
        loop (i+1) last
          (num_vowels + int_of_bool (is_vowel c1))
          (has_double || c1 = c2)
    in
    loop 0 (String.length s - 1) 0 false


  let int_of_letters c1 c2 =
    (Char.code c1 - 97 (*'a'*))*27+ (Char.code c2 - 97)


  let is_valid2 s =
    let pairs = Array.make (27*27) 0 in
    let s = s ^ "{" in
    let rec loop i len has_double has_split =
      if i >= len then false
      else
        let c1 = s.[i] in
        let c2 = s.[i+1] in
        let c3 = s.[i+2] in
        let has_double = 
          has_double ||
          let idx = int_of_letters c1 c2 in
          let n = 1 + pairs.(idx) in
          pairs.(idx) <- n; n >= 2
        in
        let has_split = has_split || c1 = c3 in
        (has_double && has_split) ||
        loop (i+1+(int_of_bool (c1 = c2 && c2 = c3))) len has_double has_split
    in
    loop 0 (String.length s - 2) false false

  let count_valid is_valid l =
    Iter.(list l |> count_if is_valid)

  let solve f () =
    let l = read_input () in
    let n = count_valid f l in
    Solution.printf "%d" n

  let solve_part1 = solve is_valid1
  let solve_part2 = solve is_valid2
end

let () = Solution.register_mod (module S)