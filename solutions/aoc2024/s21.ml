open Utils
open Syntax
module S =
struct
  let name = Name.mk "s21"
  (**

                   +---+---+---+
                   | 7 | 8 | 9 |               +---+---+
                   +---+---+---+               | ^ | A |
                   | 4 | 5 | 6 |           +---+---+---+
                   +---+---+---+           | < | v | > |
                   | 1 | 2 | 3 |           +---+---+---+
                   +---+---+---+
                       | 0 | A |
                       +---+---+

     level 3: <vA<AA>>^AvAA<^A>A        <v<A>>^AvA^A      <vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
     level 2: v<<A>>^A                  <A>A              vA<^AA>A<vAAA>^A
     level 1: <A                        ^A *              >^^AvvvA
     level 0: A0                         2                9      A

     A good strategy is to go down recursively and then iterate rather than
     computing the whole level before going down. One observation is that, when
     going down, e.g. from level 1, with "^A" (marked * above) to expand it to
     "<A>A" we always start with the robot arm on the 'A' key (since it's either
     in the initial position or it came back to press 'A' to validate the
     previous input) This allows us to just split the first input into sequences
     of pairs of symbols: from A to 0, from 0 to 2, from 2 to 9, from 9 to A.
     For each one, we transform, recursively the destination symbol, knowing
     that the previous button puched was the source symbol.

     One caveat (found while testing the last code of the example) is that even
     though all the sequences that don't go above a hole have the same length
     (the Manatthan distance form src to dst), some of them generate shorter
     sequences at the following level. Rather than trying to guess the general
     rule (if there is one) we can just test while backtracking all the
     permutations of the initial move sequence.

     This solves part 1 pretty quickly (< 3 ms).

     For part 2, we add some memoization. But we also want to avoid representing the
     *huge* sequence of moves that is computed as a string or a list of chars.
     Instead we use a minimal implementation of ropes (with no rebalancing, which
     does not hurt too much since at worse, the depth is 26). Ropes + memoization
     allow us to cache the intermediary result and produce huge sequences of moves
     where the common parts are shared in memory.

     This solves part 2 pretty quickly also (< 10 ms)
  *)

  (* Coordinates on the numeric keypad *)
  let num_coords = [| (1,3); (0,2); (1,2); (2,2); (0,1); (1,1); (2,1); (0,0); (1,0); (2,0);(2,3)|]

  (* from a key symbol to the index in the keypad array *)
  let num_idx = function
    | '0'..'9' as c -> Char.code c - Char.code '0'
    | 'A' -> 10
    | _ -> assert false

  (* coordinates on the joystick *)
  let dir_coords = [|(1,0); (2,0); (0,1); (1,1); (2,1) |]

  (* from direction symbols to  the index in the direction array *)
  let dir_idx = function
      '^' -> 0
    | 'A' -> 1
    | '<' -> 2
    | 'v' -> 3
    | '>' -> 4
    | _ -> assert false

  (* generate all moves given by dx (horizontally) and
     dy (vertically), e.g. (-1, 2) = ['<';'v';'v']*)
  let gen_moves (dx, dy) =
    let hsym = if dx < 0 then '<' else '>' in
    let vsym = if dy < 0 then '^' else 'v' in
    let res = ref [] in
    for _i = 0 to abs dx - 1 do
      res := hsym :: !res
    done;
    for _i = 0 to abs dy - 1 do
      res := vsym :: !res;
    done;
    !res

  let ( -!) (a, b) (c, d) = (a - c, b - d)

  (* test whether a path is valid w.r.t a given hole *)
  let dirs = ['<', (-1,0); '>', (1,0); '^', (0, -1); 'v', (0, 1) ]
  let valid hole src path =
    let rec loop p path =
      if p = hole then false
      else match path with
          [] -> true
        | m :: path' ->
          let d = List.assoc m dirs in
          let p' = Grid.(p +! d) in
          loop p' path'
    in
    loop src path

  module Rope = struct
    type rope =
        Leaf of int * char list
      | Node of int * int * rope * rope
    let empty = Leaf (0,[])
    let height = function
        Leaf _ -> 0
      | Node(h, _, _, _) -> h

    let length = function
        Leaf (n, _ )
      | Node (_, n, _, _) -> n

    let (@) r1 r2 =
      let l1 = length r1 in
      let l2 = length r2 in
      if l1 = 0 then r2
      else if l2 = 0 then r1
      else
        let l = l1 + l2 in
        match r1, r2 with
          Leaf (_, l1), Leaf(_, l2) when l <= 64 -> Leaf (l, l1 @ l2)
        | _->
          let h = 1+ max (height r1) (height r2) in
          Node (h, l, r1, r2)

    let (@:) e r = (Leaf (1, [e])) @ r
    let is_empty r = length r = 0

    let rec view r =
      match r with
        Leaf (_, []) -> None
      | Leaf (n, e :: l) -> Some (e, Leaf (n-1, l))
      | Node (_, l, r1, r2) ->
        match view r1 with
          None -> assert false
        | Some (e, r') -> Some (e, r' @ r2)


    let of_list l = Leaf (List.length l, l)

    (* The functions below are only usefull for displaying the ropes *)
    let sub_list l i len =
      let rec loop l i len =
        if i = 0 then loop_keep l len
        else match l with
            [] -> assert false
          | e :: ll -> loop ll (i-1) len
      and loop_keep l len =
        if len = 0 then []
        else match l with
            [] -> assert false
          | e :: ll -> e :: loop_keep ll (len - 1)
      in loop l i len
    let rec sub r i len =
      match r with
        Leaf (_, l) -> Leaf (len, sub_list l i len)
      | Node (h, l, r1, r2) ->
        let len1 = length r1 in
        if i + len <= len1 then sub r1 i len
        else if i >= len1 then sub r2 (i-len1) len
        else
          let n = len1 - i in
          let res1 = sub r1 i n in
          let res2 = sub r2 0 (len - n) in
          res1 @ res2

    let rec iter_char f r =
      match r with
        Leaf(_, l) -> List.iter f l
      | Node (_, _, r1, r2) ->
        iter_char f r1;
        iter_char f r2

    let implode r =
      let b = Bytes.create (length r) in
      let i = ref 0 in
      iter_char (fun c -> b.$[!i] <- c; incr i) r;
      Bytes.unsafe_to_string b

  end

  let enumerate lmap symbols =
    let memo = ~%[] in
    let rec loop level lmap seq =
      match lmap with
        []  -> seq
      | conf :: llmap ->
        loop_apply level conf llmap Rope.('A' @: seq)
    and loop_apply level ((idx, hole, coords) as conf) lmap seq =
      match Rope.view seq with
        None -> Rope.empty
      | Some (src, lseq) ->
        match Rope.view lseq with
          None -> Rope.empty
        | Some (dst, _) ->
          let key = level, src, dst in
          let res = try memo.%{key} with
              Not_found ->
              let perm = (* Generate all permutations *)
                gen_moves (coords.(idx dst) -! coords.(idx src))
                |> Comb.perm
                |> List.of_seq
                |> List.sort_uniq compare
                |> List.filter (valid hole coords.(idx src))
                |> List.map Rope.of_list
              in
              let res =
                perm |> List.fold_left (fun acc_res sm ->
                    let res = loop (level+1) lmap Rope.(sm @ of_list ['A']) in
                    if Rope.(is_empty acc_res || length res < length acc_res) then res else acc_res
                  ) Rope.empty
              in
              memo.%{key} <- res; res
          in
          Rope.(res @ loop_apply level conf lmap lseq)
    in
    loop 0 lmap (Rope.of_list symbols)

  let read_input () =
    Input.fold_lines (fun acc s -> s :: acc) []
    |> List.rev

  let score verbose lmap l =
    List.fold_left (Agg.Left.sum (fun s ->
        let r = enumerate lmap (String.explode s) in
        let n = Rope.length r in
        let () =
          if verbose then
            let r = if n > 100 then
                Rope.((sub r 0 40) @ of_list [ ' '; '.'; '.'; '.' ; ' ']
                      @ (sub r (n - 40) 40))
              else r
            in
            Format.printf "%s: %s (%d)\n%!" s (Rope.implode r) n
        in
        let c = String.sub s 0 3 |> int_of_string in
        n * c)) 0 l

  let dir_conf = (dir_idx, (0,0), dir_coords)
  let num_conf = (num_idx, (0,3), num_coords)
  let lmap_part1 = [ num_conf; dir_conf; dir_conf ]
  let lmap_part2 = (* 26 in total *)
    [ num_conf;
      dir_conf; dir_conf; dir_conf; dir_conf; dir_conf;
      dir_conf; dir_conf; dir_conf; dir_conf; dir_conf;
      dir_conf; dir_conf; dir_conf; dir_conf; dir_conf;
      dir_conf; dir_conf; dir_conf; dir_conf; dir_conf;
      dir_conf; dir_conf; dir_conf; dir_conf; dir_conf;
    ]

  let solve verbose lmap =
    let l = read_input () in
    let n = score verbose lmap l in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

  let solve_part1 () = solve false lmap_part1
  let solve_part2 () = solve false lmap_part2

end

let () = Solution.register_mod (module S)
module SV =
struct
  let name = S.name
  let solve_part1 () = S.solve true S.lmap_part1
  let solve_part2 () = S.solve true S.lmap_part2
end
let () = Solution.register_mod ~variant:"verbose" (module SV)
