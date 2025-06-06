open Utils
open Syntax

module S =
struct
  let name = Name.mk "s13"


  let magic = ref 0
  let read_input () =
    let n = Input.read_line () |> int_of_string in
    magic := n; n

  let is_open x y =
    let n = x * x + 3 * x + 2 * x * y + y + y*y in
    let n = n + !magic in
    let n = S11.S.IDevSet.pop_count n in
    n land 1 == 0


  let do_call f x y =
    if is_open x y then f (x, y)
  let iter_next f (x, y) =
    if x >= 1 then do_call f (x-1) y;
    do_call f (x+1) y;
    if y >= 1 then do_call f x (y-1);
    do_call f x (y+1)

  (* Custom BFS :
     If the final destination is reached, return the length of the path (part 1)
     Otherwise, whenever the queue is empty, return the number of positions visited.
     Don't expolore rooms that are further than limit moves from the start.
  *)
  let bfs limit init final =
    let visited = ~%[init, ()] in
    let queue = Queue.create () in
    let () = Queue.add (0, init) queue in
    let count = ref 0 in
    let rec loop () =
      if Queue.is_empty queue then !count else
        let n, pos = Queue.pop queue in
        let () = incr count in
        if pos = final then n else begin
          iter_next (fun npos ->
              if not (visited %? npos) && n < limit then begin
                visited.%{npos} <- (); 
                Queue.add (n+1, npos) queue
              end
            ) pos;
          loop ()
        end
    in loop ()

  let solve limit final = 
    let _ = read_input () in
    let n = bfs limit (1,1) final in
    Solution.printf "%d" n

  let solve_part1 () = solve max_int (31,39)
  let solve_part2 () = solve 50 (-1, -1)
end

let () = Solution.register_mod (module S)