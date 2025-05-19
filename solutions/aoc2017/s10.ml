open Utils
open Syntax
module S =
struct
  let name = Name.mk "s10"

  let read_input () =
    Input.read_line ()
    |> String.split_on_char ','
    |> List.map int_of_string

    (*
      [|0;1;2;3;4|]  3,4,1,5
      -> rev_range a 0 3
         -> 0,2
      [|2; 1; 0; 3; 4|]
      -> rev_range a 3 4
         -> 3,


    *)
  let rev_range a i n =
    let len = Array.length a in
    assert (n < len);
    assert (0 <= i && i < len);

    for j = 0 to n/2 - 1 do
      let x = a.((i + j) mod len) in
      let y = a.(((i+n-1)-j) mod len) in
      a.((i + j) mod len) <- y;
      a.(((i+n-1)-j) mod len) <- x;
    done

  let transform l a skip current =
    let len = Array.length a in
    List.iter (fun n -> rev_range a !current n;
                current := (!current + n + !skip) mod len;
                skip := !skip + 1
              ) l

  let transform1 l =
    let skip = ref 0 in
    let current = ref 0 in
    let a = Array.init 256 Fun.id in
    transform l a skip current;
    a.(0) * a.(1)

  let solve_part1 () =
    let l = read_input () in
    let n = transform1 l in
    Solution.printf "%d" n

  let hex_digits = Array.init 256 (Format.sprintf "%02x")
  let knot_hash s =
    let lc = String.explode s in
    let l = (List.map Char.code lc) @ [17; 31; 73; 47; 23] in
    let skip = ref 0 in
    let current = ref 0 in
    let a = Array.init 256 Fun.id in
    for _ = 1 to 64 do
      transform l a skip current;
    done;
    let r = Array.make 16 0 in
    for i = 0 to 15 do
      for j = 0 to 15 do
        r.(i) <- r.(i)  lxor a.(i*16+j);
      done;
    done;
    let b = Bytes.create 32 in
    for i = 0 to 15 do
      let s = hex_digits.(r.(i)) in
      b.$[i*2] <- s.[0];
      b.$[i*2+1] <- s.[1];
    done;
    b
      

  let solve_part2 () =
    let s = Input.read_line () in
    let h = knot_hash s in
    Solution.printf "%a" Format.pp_print_bytes h
end

let () = Solution.register_mod (module S)