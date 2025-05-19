open Utils
open Syntax
module S =
struct
  let name = Name.mk "s16"


  let tmp = Bytes.create 16
  let spin n s =
    Bytes.blit s (16-n) tmp 0 n;
    Bytes.blit s 0 s n (16-n);
    Bytes.blit tmp 0 s 0 n

  let exchange i j s =
    let c = s.$[i] in
    s.$[i] <- s.$[j];
    s.$[j] <- c

  let partner a b s =
    let i = Bytes.index s a in
    let j = Bytes.index s b in
    exchange i j s

  let sub1 s = String.sub s 1 (String.length s - 1)
  let code c = Char.code c - Char.code 'a'
  let read_input () =
    Input.read_line ()
    |> String.split_on_char ','
    |> List.map (fun s ->
        s,match s.[0] with
        's' -> spin (int_of_string (sub1 s))
        | 'x' -> begin match String.split_on_char '/' (sub1 s) with
              n::m::_ -> exchange (int_of_string n) (int_of_string m)
            | _ -> assert false
          end
        | 'p' -> partner s.[1] s.[3]
        | _ -> assert false
      )

  let exec l s =
    let b = Bytes.of_string s in
    List.iter (fun (_s,f) -> f b) l;
    Bytes.to_string b

  let solve_part1 () =
    let l = read_input () in
    let s = exec l "abcdefghijklmnop" in
    Solution.printf "%s" s

  let compute_cycle l s rep =
    let lam, mu, x = Misc.find_cycle String.equal (exec l) s in
    let x0 = ref x in
    for _ = 1 to rep mod lam do
      x0 := exec l !x0;
    done;
    !x0 

  let solve_part2 () =
    let l = read_input () in
    let s = compute_cycle l "abcdefghijklmnop" 1_000_000_000 in
    Solution.printf "%s" s

end

let () = Solution.register_mod (module S)