open Utils
module S =
struct
  let name = Name.mk "s17"

  let gen_buffer step last =
    let rec loop n b =
      if n > last then b else
        let b = Dll.forward step b in
        let b = Dll.insert_after b n in
        loop (n+1) b
    in
    let b = loop 1 (Dll.singleton 0) in
    Dll.peek (Dll.next b)

  let solve_part1 () =
    let s = Input.read_line () |> int_of_string in
    let n = gen_buffer s 2017 in
    Solution.printf "%d" n

  (* If we anchor our Dll at position 0, we only need to keep track of the position
     where we would insert a number.
     When that position is 1 (= after 0), record the value.
  *)
  let after_zero step last =
    let pos = ref 0 in
    let v = ref 0 in
    for i = 1 to last do
      pos := ((!pos + step) mod i) + 1;
      if !pos = 1 then v := i;
    done;
    !v

  let solve_part2 () =
    let s = Input.read_line () |> int_of_string in
    let n = after_zero s 50_000_000 in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)