open Utils
module S =
struct
  let name = Name.mk "s19"

  let ops_by_name = List.map
      (fun (op,name) ->
         name,
         (fun a b c rsrc -> ignore @@ op rsrc rsrc a b c)) S16.S.ops

  let read_input () =
    let reg_line = Input.read_line () in
    let ip_reg = Scanf.sscanf reg_line "#ip %d" Fun.id in
    let prog =
      Input.fold_lines (fun acc line ->
          Scanf.sscanf line "%s %d %d %d"
            (fun name a b c ->
               ((List.assoc name ops_by_name) a b c)::acc)
        ) []
    in
    ip_reg, List.rev prog |> Array.of_list

  let eval init0 ip_reg prog =
    let regs = Array.make 6 0 in
    let len = Array.length prog in
    let rec loop ip =
      Array.(unsafe_set regs ip_reg ip);
      Array.(unsafe_get prog ip) regs;
      let ip' = 1 + Array.(unsafe_get regs ip_reg) in
      if ip' < len then loop ip'
    in
    regs.(0) <- init0;
    loop 0;
    regs

  let solve_part1 () =
    let ip_reg, prog = read_input () in
    let n = (eval 0 ip_reg prog).(0) in
    Solution.printf "%d" n

  (*
#ip 3
00: addi 3 16 3    #GOTO 17, INIT
                   #COMPUTE, R0 : 0, R1: 10550400, R2: 0, R4:10551378, R5:0
01: seti 1 6 5     #R5 <- 1       1
02: seti 1 8 2     #R2 <- 1       1
03: mulr 5 2 1     #R1 <- R5 * R2
04: eqrr 1 4 1     #R1 <- R1 = R4
05: addr 1 3 3     #R3 <- R1 + R3  FALSE, GOTO 6, TRUE GOTO 7
06: addi 3 1 3     #R3 <- R3 + 1   7 GOTO 8
07: addr 5 0 0     #R0 <- R5 + R0
08: addi 2 1 2     #R2 <- R2 + 1
09: gtrr 2 4 1     #R1 <- R2 > R4
10: addr 3 1 3     #R3 <- R3 + R1  FALSE, GOTO 11, TRUE GOTO 12
11: seti 2 3 3     #R3 <- 2        GOTO 3
12: addi 5 1 5     #R5 <- R5 + 1
13: gtrr 5 4 1     #R1 <- R5 > R4
14: addr 1 3 3     #R3 <- 1 + R3   FALSE, GOTO 5, TRUE, GOTO 16
15: seti 1 8 3     #R3 <- 1        GOTO 2
16: mulr 3 3 3     #EXIT

                   #INIT
17: addi 4 2 4     #R4 <- R4 + 2    2
18: mulr 4 4 4     #R4 <- R4 * R4   4
19: mulr 3 4 4     #R4 <- R3 * R4   76
20: muli 4 11 4    #R4 <- R4 * 11   836
21: addi 1 6 1     #R1 <- 6
22: mulr 1 3 1     #R1 <- R1 * R3   132
23: addi 1 10 1    #R1 <- R1 + 10   142
24: addr 4 1 4     #R4 <- R4 + R1   978
25: addr 3 0 3     #R3 <- R3 + 1    26 (GOTO 27)
26: seti 0 0 3
27: setr 3 9 1     #R1 <- R3        27
28: mulr 1 3 1     #R1 <- R1 * R3   756
29: addr 3 1 1     #R1 <- R3 + R1   785
30: mulr 3 1 1     #R1 <- R3 * R1   23550
31: muli 1 14 1    #R1 <- R1 * 14   329700
32: mulr 1 3 1     #R1 <- R1 * R3   10550400
33: addr 4 1 4     #R4 <- R1 + R4   10551378
34: seti 0 4 0     #R0 <- 4         4
35: seti 0 0 3     #R3 <- 0         0 (GOTO 1)

r0 = 4
r4 = 10551378
r5 = 1;
while (r5 <= r4) {
    r2 = 1;
    while (r2 <= r4) {
      r1 = r5 * r2;
      if (r1 == r4) {
        r0 = r0 + r5;
      }
      r2 = r2 + 1;
    }
    r5 = r5 + 1;
}
In other words, the program computes a magic number, stores it in r4
and computes in r0 the sum of all the divisors of r4.

To have a somewhat generic solution we patch the program to exit after
initializing r4, and compute the sum of all its divisors.

*)
  let patch ip_reg prog =
    prog.(1)<- (List.assoc "setr" ops_by_name) 4 0 3;
    (eval 1 ip_reg prog).(4)

  let sum_divisors n =
    let k = int_of_float (sqrt (float n)) in
    assert (k*k < n); (* don't bother with perfect squares *)
    let res = ref 0 in
    for i = 1 to k do
      if n mod i = 0 then res := !res + i + n/i;
    done;
    !res
  let solve_part2 () =
    let ip_reg, prog = read_input () in
    let n = patch ip_reg prog in
    Solution.printf "%d" (sum_divisors n)

end

let () = Solution.register_mod (module S)