open Utils
open Syntax

module S =
struct

  (*


        37  36   35   34   33   32   31
        38  17   16   15   14   13   30
        39  18   5    4    3    12   29
        40  19   6    1    2    11   28
        41  20   7    8    9    10   27
        42  21   22   23   24   25   26
        43  44   45   46   47   48   49


  Observer that the lower right corner is : 
  (2n+1)^2, at coordinates (n,-n).
  Given a number 's' on the spiral, we can start
  at the right of the corner where s is, and look for it.

  *)

  let name = Name.mk "s03"

  exception Found of int


  let coord_of_pos s =
    let n = int_of_float (sqrt (float s)) in
    let n = n - (1 - (n mod 2)) in
    let p = n*n in 
    let n = n / 2 in
    if p = s then (* exactly lower left corner *) (n, -n)
    else
      let rec loop (i, j) (xd, yd) ((x, y), p) =
        if p = s then Ok (x, y)
        else if (x, y) = (xd, yd) then Error ((x, y), p)
        else loop (i, j) (xd, yd) ((x+i, y+j), (p+1))
      in
      let (let**) o f =
        match o with
          Ok r -> r
        | Error s -> f s
      in
      let n1 = n + 1 in
      let x = n1 in
      let y = - n in
      let p = p + 1 in
      let** next = loop (0, 1) (n1, n1) ((x, y), p) in
      let** next = loop (-1, 0) (-n1, n1) next in
      let** next = loop (0, -1) (-n1, -n1) next in
      let** next = loop (1, 0) (n1, -n1) next in
      fst next

  let pos_of_coord (x, y) =
    if x = -y && x >= 0 then (2*x+1)*(2*x+1)
    else
      let n = (max (abs x) (abs y)) - 1 in
      let p = (2*n+1) * (2*n+1) in
      (* n, -n *)
      let rec loop (i, j) (xd, yd) (x0, y0) p =
        if (x0, y0) = (x, y) then Ok p
        else if (x0, y0) = (xd, yd) then Error p
        else loop (i, j) (xd, yd) (x0+i, y0+j) (p+1)
      in
      let ( let** ) o f = match o with Ok v -> v | Error p -> f p in
      let** next = loop (0,1) (n+1, n+1) (n+1,-n) (p+1) in
      let** next = loop (-1, 0) (-(n+1), n+1) (n+1, n+1) next in
      let** next = loop (0, -1) (-(n+1), -(n+1)) (-(n+1), n+1) next in
      let** next = loop (1, 0) (n+1, -(n+1)) (-(n+1), -(n+1)) next in
      next


  let memo = Hashtbl.create 16
  let rec sum_value (x, y) =
    if memo %? (x, y) then memo.%{x, y}
    else
      let res =
        if x = 0 && y = 0 then 1 else
          let s = pos_of_coord (x, y) in
          let c = ref 0 in
          for j = -1 to 1 do
            for i = - 1 to 1 do
              if i <> 0 || j <> 0 then
                let xi = x + i in
                let yj = y + j in
                let s0 = pos_of_coord (xi, yj) in
                if s0 <= s then c := !c + sum_value (xi, yj);
            done;
          done;
          !c
      in
      memo.%{x, y} <- res; res

  let find_larger_than n =
    let rec loop i =
      let v = sum_value (coord_of_pos i) in
      if v > n then v else loop (i+1)
    in
    loop 0 

  let solve_part1 () =
    let s = Input.read_line () |> String.trim |> int_of_string in
    let x, y = coord_of_pos s in
    Solution.printf "%d" (abs x + abs y)

  let solve_part2 () =
    let s = Input.read_line () |> String.trim |> int_of_string in
    let n = find_larger_than s in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)