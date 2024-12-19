open Utils
module S =
struct
  let name = Name.mk "s19"

  let scan code =
    let total = ref 0 in
    for x = 0 to 49 do
      for y = 0 to 49 do
        let state = Intcode.make_state code in
        Queue.push x state.Intcode.stdin;
        Queue.push y state.Intcode.stdin;
        ignore (Intcode.eval state);
        total := !total + Queue.pop state.Intcode.stdout;
      done;
    done;
    !total
  let solve_part1 () =
    let code = Intcode.read () in
    let state = Intcode.make_state code in
    let n = scan code in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)


  let dove_tail_search square_size code =
    let test x y =
      let state = Intcode.make_state code in
      Queue.push x state.Intcode.stdin;
      Queue.push y state.Intcode.stdin;
      ignore (Intcode.eval state);
      Queue.pop state.stdout
    in
    let min_a = ref max_int in
    let max_a = ref min_int in
    let min_b = ref max_int in
    let max_b = ref min_int in
    let mark n m x y rmin rmax =
      if x = m then begin
        rmin := min y !rmin;
        rmax := max y !rmax;
      end
    in
    let rec loop n a b =
      if n <= b then begin
        for i = 0 to n - 1 do
          if test i n = 1 then (mark n a i n min_a max_a; mark n b i n min_b max_b);
          if test n i = 1 then (mark n a n i min_a max_a; mark n b n i min_b max_b);
        done;
        if test n n = 1 then (mark n a n n min_a max_a; mark n b n n min_b max_b);
        loop (n+1) a b
      end
    in
    let a = 10 and b = 60 in (* Found empirically *)
    loop 1 a b;
    let smin = (float !min_b -. float !min_a) /. (float b -. float a) in
    let smax = (float !max_b -. float !max_a) /. (float b -. float a) in
    let x0f = ((smin +. 1.0) *. (float square_size)) /. (smax -. smin) in
    let calc_y0 x = int_of_float (smax *. (float x)) in
    let x0 = int_of_float x0f in
    let rec find_limit x y t i =
      if test x y = t then find_limit x (y+i) t i
      else if test x y = 1 then y
      else if test x (y-1) = 1 then y
      else (y+1)
    in
    let rec fit_square n x0 =
      let y0 = calc_y0 x0 in
      let t0 = test x0 y0 in
      let y0 = find_limit x0 y0 t0 (if t0 = 1 then 1 else -1) in
      let x1 = x0 + n in
      let y1 = y0 - n in
      if test x1 y1 = 0 || test x0 y0 = 0 || test x1 y0 = 0 then fit_square n (x0+1)
      else x0, y0, x1, y1
    in
    let x0, y0, x1, y1 = fit_square square_size x0 in
    x0 * 10000 + y1

  let solve_part2 () =
    let code = Intcode.read () in
    let n = dove_tail_search 99 code in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)
end

let () = Solution.register_mod (module S)