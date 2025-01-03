open Utils
module S =
struct
  let name = Name.mk "s21"
  (*
#ip 4
00: seti 123 0 2               R2 <- 123
01: bani 2 456 2               R2 <- R2 & 456     72
02: eqri 2 72 2                R2 <- R2 = 72
03: addr 2 4 4                 True, GOTO 5, False, GOTO 4
04: seti 0 0 4                 GOTO 1
05: seti 0 0 2                 R2 <- 0
06: bori 2 65536 5             R5 <- R2 | 65536
07: seti 5234604 6 2           R2 <- 5234604
08: bani 5 255 3               R3 <- R5 & 255       0
09: addr 2 3 2                 R2 <- R2 + R3        65536
10: bani 2 16777215 2          R2 <- R2 & 16777215  65536
11: muli 2 65899 2             R2 <- R2 * 65899     4318756864
12: bani 2 16777215 2          R2 <- R2 & 16777215  7012352
13: gtir 256 5 3               R3 <- 256 > R5
14: addr 3 4 4                 True, GOTO 16, False Goto 15
15: addi 4 1 4                 GOTO 17
16: seti 27 2 4                GOTO 28
17: seti 0 0 3                 R3 <- 0
18: addi 3 1 1                 R1 <- R3 + 1
19: muli 1 256 1               R1 <- R1 * 256
20: gtrr 1 5 1                 R1 <- R1 > R5
21: addr 1 4 4                 True, GOTO 23, False, GOTO 22
22: addi 4 1 4                 GOTO 24
23: seti 25 6 4                GOTO 26
24: addi 3 1 3                 R3 <- R3 + 1
25: seti 17 7 4                GOTO 18
26: setr 3 4 5                 R5 <- R3
27: seti 7 8 4                 GOTO 8
28: eqrr 2 0 3                 R3 <- R2 = R0
29: addr 3 4 4                 TRUE, GOTO 31 (HALT), False GOTO 30
30: seti 5 6 4                 GOTO 6

The program computes a number (using tests and loops, starting from
a fixed value, and only tests at the end if it is equal to R0)
We can patch instruction 28 to halt and return the value stored in R2
the first time it is reached.

We also see that the program is a PRNG (that generates numbers of size
at most 24 bits). The program checks all numbers in the pseudo-random
sequence until it finds the one in R0. If R0 is not one of these numbers,
the program will not terminate.
So we patch instruction 28 to tabulate and find the second time we see the same number.
The number we seek is the one just before that.
*)

  let read_input () = S19.S.read_input ()
  exception Found of int
  let patch1 ip_reg prog =
    try
      prog.(28) <- (fun reg -> raise (Found reg.(2)));
      ignore (S19.S.eval 0 ip_reg prog);
      -1
    with Found n -> n

  let patch2 ip_reg prog =
    try
      let table = Array.make (1 lsl 24) false in
      let prev = ref (-1) in
      let old = prog.(28) in
      prog.(28) <- (fun reg ->
          let r2 = reg.(2) in
          if Array.unsafe_get table r2 then raise (Found (!prev)) else begin
            Array.unsafe_set table r2 true;
            prev := r2;
          end;
          old reg);
      ignore (S19.S.eval 0 ip_reg prog);
      -1
    with Found n -> n
  let solve patch =
    let ip_reg, prog = read_input () in
    let n = patch ip_reg prog in
    Solution.printf "%d" n
  let solve_part1 () = solve patch1
  let solve_part2 () = solve patch2
end

let () = Solution.register_mod (module S)