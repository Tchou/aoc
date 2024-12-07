open Utils
open Syntax
module S =
struct
  let name = Name.mk "s22"

  module Pqueue =
  struct
    type 'a t = { front : 'a list; back : 'a list }

    let empty = {front = []; back = [] }
    let is_empty = function { front = []; back = [];_ } -> true
                          | _ -> false
    let push v q = { q with back = v::q.back}
    let pop q = match q.front, q.back with
        [], [] -> failwith "pop"
      | v :: f, _ -> v, { q with front = f }
      | [], back ->
        match List.rev back with
          v :: f -> v, { front = f; back = [] }
        | _ -> assert false

    let length q = List.length q.front + List.length q.back

    let take n q =
      let rec loop front l n =
        if n = 0 then []
        else match l with
            [] -> if front then loop false (List.rev q.back) n
            else failwith "take"
          | v :: ll -> v::loop front ll (n-1)
      in
      { back = []; front = loop true q.front n }

    let fold f acc q =
      let rec loop fr bk acc =
        match fr, bk with
          [], [] -> acc
        | [], _ -> loop (List.rev bk) [] acc
        | v::ffr, _ -> loop ffr bk (f acc v)
      in
      loop q.front q.back acc
  end

  let read_player () =
    let s = read_line () in
    Scanf.sscanf s "Player %[12]:"
      (fun _ ->
         InputUntil.fold_lines (fun acc l ->
             if l = "" then (false, acc)
             else (true, Pqueue.push (int_of_string l) acc)) Pqueue.empty
      )


  let read_input () =
    let q1 = read_player () in
    let q2 = read_player () in
    q1, q2

  let play recursive q1 q2 =
    let exception Cycle of int Pqueue.t in
    let rec loop seen q1 len1 q2 len2 =
      let key = q1, q2 in
      if recursive && seen %? key then raise (Cycle q1) else
        let () = if recursive then seen.%{key} <- () in
        if Pqueue.is_empty q1 then (2, q2)
        else if Pqueue.is_empty q2 then (1, q1)
        else
          let c1, qq1 = Pqueue.pop q1 in
          let c2, qq2 = Pqueue.pop q2 in
          let winner =
            if recursive && c1 < len1 && c2 < len2 then
              let new_q1 = Pqueue.take c1 qq1 in
              let new_q2 = Pqueue.take c2 qq2 in
              try
                fst (loop (Hashtbl.create 16) new_q1 c1 new_q2 c2)
              with Cycle _ -> 1
            else if c1 > c2 then 1 else 2
          in
          if winner = 1 then
            loop seen Pqueue.(push c2 (push c1 qq1)) (len1+1) qq2 (len2-1)
          else
            loop seen qq1 (len1-1) Pqueue.(push c1 (push c2 qq2)) (len2+1)
    in
    try
      loop (Hashtbl.create 16) q1 (Pqueue.length q1) q2 (Pqueue.length q2)
    with Cycle q1 -> (1, q1)

  let solve recursive =
    let q1, q2 = read_input () in
    let _, q = play recursive q1 q2 in
    let l = Pqueue.length q in
    let _, n = Pqueue.fold
        (fun (i, n) c -> (i-1, n+i*c)) (l, 0) q
    in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part1 () = solve false

  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)