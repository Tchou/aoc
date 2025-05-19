open Utils
module S =
struct
  let name = Name.mk "s23"
  open S18.S

  type instr += 
    | Sub of arg * arg
    | Jnz of arg * arg

  let read_input () =
    read_input ~others:(function 
        | ["sub"; a1; a2 ] -> Sub (arg a1,arg a2)
        | ["jnz"; a1; a2 ] -> Jnz (arg a1, arg a2)
        | l -> parse_others l) ()

  let eval instrs =
    let reg = Array.make 256 0 in
    let count_mul = ref 0 in
    let rec loop i =
      if i < 0 || i >= Array.length instrs then !count_mul
      else
        let i' =
          match instrs.(i) with
            Set (a1, a2) -> 
            set reg a1 (get reg a2); i + 1
          | Sub (a1, a2) -> 
            set reg a1 ((get reg a1) - (get reg a2)); i + 1
          | Mul (a1, a2) -> incr count_mul; set reg a1 ((get reg a1) * (get reg a2)); i + 1
          | Jnz (a1, a2) ->
            if (get reg a1) <> 0 then i + (get reg a2) else i + 1
          | _ -> failwith "Unsupported operation"
        in
        loop i'
    in
    loop 0


  let solve_part1 () =
    let instrs = read_input () in
    let n = eval instrs in
    Solution.printf "%d" n
(*
Analysing the code:
00    set b 65                # b := 65
01    set c b                 # c := 65
02    jnz a 2                 # goto 5
03    jnz 1 5                 #
04    mul b 100               # b := b * 100
05    sub b -100000           # b := b + 100000
06    set c b                 # c := b
07    sub c -17000            # c := c + 17000
# LABEL 8
08    set f 1                 # f := 1
09    set d 2                 # d := 2
# LABEL 10
10    set e 2                 # e := 2
# LABEL 11
11    set g d                 # g := d
12    mul g e                 # g := g * e
13    sub g b                 # g := g - b
14    jnz g 2                 # goto 16
15    set f 0                 #
16    sub e -1                # e := e + 1
17    set g e                 # g := e
18    sub g b                 # g := g - b
19    jnz g -8                # goto 11
20    sub d -1                # d := d + 1
21    set g d                 # g := d
22    sub g b                 # g := g - b
23    jnz g -13               # goto 10
24    jnz f 2                 # goto 26
25    sub h -1                # h := h + 1
26    set g b
27    sub g c
28    jnz g 2                 # goto 30
29    jnz 1 3                 # HALT
30    sub b -17
31    jnz 1 -23               # goto 8

Gives the following program:

void f() {
  int a,b,c,d,e,f,g,h;
  a = 1;
  b = 65
  c = b;
  b = b * 100;
  b = b + 100000;
  c = b;
  c = c + 17000;
  do { //label 8

    f = 1;
    d = 2;

    do { //label 10

      e = 2;

      do { //label 11

        g = d;
        g = g * e;
        g = g - b;
        if (g ==  0) {
          f = 0;
          break;//speedup
        }
        e = e + 1;
        g = e;
        g = g - b;
      } while (g != 0);
      if (f == 0) break;
      d = d + 1;
      g = d;
      g = g - b;
    } while (g != 0);
    if (f == 0) {
      h = h + 1;  
    }
    g = b;
    g = g - c;
    if (g == 0) {
      printf("%d\n", h);
      return;
    }
    b = b + 17;
  } while (1);

}
First, we can see that the program increments h whenever f is
set to 0. We can speed up a bit by breakin both loops when
this is the case.

Second, if we analyse the program, we see it examine
integers in the range (b=106500, to c=123500 with a step of 17).
For each integers, it enumerates d from 2 to b and e from 2 to b
and sets f to 0 whenever e*d = b. In otherwords,
The program counts the number of non prime numbers between
b and c
*)
  let simulate_prog () =
    let rec loop b c step total =
      let n = int_of_float (sqrt (float b)) in
      let found = ref false in
      let i = ref 2 in
      while not !found && !i <= n do
        if b mod !i = 0 then
          found := true;
        incr i;
      done;
      let total = if !found then total + 1 else total in
      if b == c then total
      else loop (b+step) c step total
    in
    loop 106500 123500 17 0


  let solve_part2 () =
    let instrs = read_input () in
    let n = simulate_prog () in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)