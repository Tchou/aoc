open Utils
open Syntax
module S =
struct
  let name = Name.mk "s15"

  let all_dirs = [|(0,-1);(0,1);(-1,0);(1,0)|]
  let dir i = all_dirs.(i-1)
  let move (x, y) d =
    let (i, j) = dir d in (i+x, j+y)

  exception OxygenFull
  exception OxygenFound of Intcode.state * (int * int) * int
  let bfs fill_mode start state =
    let todo = Queue.create () in
    let max_dist = ref 0 in
    let () = Queue.push (start, 0, state) todo in
    let visited = ~%[start, ()] in
    let rec loop () =
      if not (Queue.is_empty todo) then
        let p, dist, state = Queue.pop todo in
        begin
          for dir = 1 to 4 do
            let np = move p dir in
            if not (visited %? np) then begin
              visited.%{np} <- ();
              let nstate = Intcode.copy_state state in
              Queue.push dir nstate.stdin;
              let _ = Intcode.eval ~out_count:1 nstate in
              let out = Queue.pop nstate.stdout in
              if out = 2 && not (fill_mode) then raise (OxygenFound (nstate, np, dist+1))
              else if out = 1 then begin
                max_dist := max !max_dist (dist+1);
                Queue.push (np, dist+1, nstate) todo
              end
            end
          done;
          loop ()
        end
      else raise OxygenFull
    in
    try
      loop ()
    with OxygenFound (state, np, d) -> (state, np, d)
    | OxygenFull -> state, start, !max_dist


  let solve_part1 () =
    let code = Intcode.read () in
    let _, _, dist = bfs false (0,0) (Intcode.make_state code) in
    Ansi.(printf "%a%d%a\n%!" fg green dist clear color)
  let solve_part2 () =
  let code = Intcode.read () in
  let state, tank, _ = bfs false (0,0) (Intcode.make_state code) in
  let _, _, time = bfs true tank state in
  Ansi.(printf "%a%d%a\n%!" fg green time clear color)

end

let () = Solution.register_mod (module S)