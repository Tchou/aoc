open Utils
module S =
struct

  let name = Name.mk "s22"

  (* Helped by the keyword Linear Congruential Generators 

     (aX + b) mod n
     (cX + d) mod n
     They can be composed :
     (c ((aX + b) mod n) + d) mod n
     (caX + cb + d) mod n
  *)
  module Z =
  struct
    include Z
    let pp = pp_print
    let (mod) a b = ((a mod b)+b) mod b
  end
  type lcg = Z.t * Z.t

  let compose m (a, b) (c, d) = Z.(((a * c) mod m),((c * b + d) mod m))
  let id = Z.(one, zero)

  let compose_list m = List.fold_left (compose m) id

  let apply m (a, b) x = Z.((a * x + b) mod m)
  (*
            cut 4

      4  5  6  7  8  9  0  1  2  3
  *)


  let read_input () =
    Input.fold_fields ' ' (fun acc f ->
        match f with
        | [ "deal"; "into"; "new"; "stack" ] -> Z.(minus_one, minus_one)::acc
        | [ "cut"; n ] -> Z.(one, -of_string n)::acc
        | [ "deal"; "with"; "increment"; n ] -> Z.(of_string n, zero)::acc
        |  _ -> assert false
      ) []
    |> List.rev

  let power_comp m f k =
    let rec loop f g k =
      if k = 0 then g
      else
        let g = if k land 1 = 1 then compose m f g else g in
        loop (compose m f f) g (k lsr 1)
    in
    loop f id k

  let solve_part1 () =
    let ops = read_input () in
    let len = Z.of_int 10007 in
    let op = compose_list len ops in
    let n = apply len op (Z.of_int 2019) in
    Ansi.(printf "%a%a%a\n%!" fg green Z.pp n clear color)

  module ZMath = MathGen(Z)

  (* For the last part we need to compute the multiplicative inverse:
     op^k(X) =aX+b  mod len
     so
     X    = a op^-k(X) + b mod len
     op^-k(X) =  (X - b)a^-1    mod len
     op^-k(X) = a^-1 x - ba^-1  mod len

     we compute a^-1 using Bezout

  *)
  let solve_part2 () =
    let ops = read_input () in
    let len = Z.of_int 119315717514047 in
    let repeat = 101741582076661 in
    let op = compose_list len ops in
    let (a, b) = power_comp len op repeat in
    let ainv, _, _ = ZMath.egcd a len in
    let b = Z.((-b * ainv) mod len) in
    let n = apply len (ainv, b) Z.(of_int 2020) in
    Ansi.(printf "%a%a%a\n%!" fg green Z.pp n clear color)


end

let () = Solution.register_mod (module S)