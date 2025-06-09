open Utils
module S =
struct
  let name = Name.mk "s19"
  (* Instance of the Josephus problem *)

  let read_input () = Input.read_line () |> int_of_string

  let log2 n = 
    n |> float
    |> Float.log2
    |> int_of_float

  let josephus n =
    let l = n - (1 lsl (log2 n)) in
    2 * l + 1

  let solve_part1 () =
    let n = read_input () in
    let r = josephus n in
    Solution.printf "%d" r

(*

Variation on the josephus problem with no closed formula, we simulate.
We keep two cursors, Current and Opposite

Round 1, 11 people, odd number:

                11        1C
                                  2
         10                             3


           9                            4
                  8                5
                        7      6O

Remove Opposite (6), Opposite moves to 7
Move C forward, move O forward
----
Round 2, 10 people, even number:

                11        1
                                  2C
         10                             3


           9                            4
                  8O             5
                        7    
Remove Opposite (8), Opposite moves to 9 
Move C forwawrd,
----
Round 3, 9 people, odd number:

                11        1
                                  2
         10                             3C


           9O                            4
                                5
                        7    
Remove Opposite (9), Opposite moves to 10
Move C forward, Move O forward
----
Round 4, 8 people, even number

              11O       1
                               2
                                  3
         10

              7                  4C
                         5

Remove Opposite (11), Opposite moves to 1
Move C forward
----
Round 5, 7 people, odd number

                    1O
                               2
                                  3
         10

              7                  4
                         5C
Remove Opposite (1), Opposite moves to 2
Move C forward, Move O forward
----
Round 6, 6 people, even number

                    10
                               2
           7C                     3O


                               4
                     5
Remove Opposite (3), Opposite moves to 4
Move C forward

So position Current and Opposite then :
- let n be the number of people at the begining of the round
- Delete Opposite (positions to the next cell)
- If n is odd, advance opposite pointer, otherwise leave it
- Advance current
- repeat until only 1 person remains.
*)

  let mk_dll n =
    let first = Dll.singleton 1 in
    let l = ref first in
    for i = 2 to n do
      l := Dll.insert_after !l i
    done;
    first

  let simulate dll size =
    let rec loop current opposite n =
      if n = 1 then Dll.peek current else
        let _, opposite = Dll.pop opposite in
        let opposite = if n mod 2 = 0 then opposite else Dll.next opposite in
        loop (Dll.next current) opposite (n-1)
    in
    loop dll (Dll.forward (size/2) dll) size



  let solve_part2 () =
    let size = read_input () in
    let dll = mk_dll size in
    let r = simulate dll size in
    Solution.printf "%d" r

end

let () = Solution.register_mod (module S)