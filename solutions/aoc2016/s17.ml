open Utils
open Syntax

module S =
struct
  let name = Name.mk "s17"

  let read_input () = Input.read_line ()


  type config = Grid.position * String.t

  let is_open n = n > 0xa

  let get_hex d i = 
    let n = Char.code d.[i lsr 1] in
    let o = (lnot i) land 1 in
    (n lsr (o lsl 2)) land 0xf

  let dirs = Grid.[ north, "U"; south, "D"; west, "L"; east, "R"]

  let iter_next f (pos, path) =
    let md5 = Digest.string path in
    dirs |> List.iteri (fun i (d, l) ->
        if is_open (get_hex md5 i) then
          let (x, y) as pos' = Grid.(pos +! d) in
          if x >= 0 && y >= 0 && x < 4 && y < 4 then
            f (pos', path ^ l)
      )

  let bfs init final prefix =
    let start = init, prefix in
    let visited = ~%[start, () ] in
    let queue = Queue.create () in
    let () = Queue.add start queue in
    let rec loop () =
      if Queue.is_empty queue then "NO PATH" else
        let (pos, cur_path) as conf = Queue.take queue in
        if pos = final then String.remove_prefix ~prefix cur_path else
          let () = conf |> iter_next (fun conf' ->
              if not (visited %? conf') then begin
                visited.%{conf'} <- ();
                Queue.add conf' queue
              end )
          in
          loop ()
    in loop ()

  let dfs init final prefix =
    let max_len = ref min_int in
    let plen = String.length prefix in
    let visited = ~%[] in
    let rec loop ((pos, path) as conf) =
      if pos = final then max_len := max !max_len (String.length path - plen)
      else if not (visited %? conf) then begin
        visited.%{conf} <- ();
        iter_next loop conf;
        visited %- conf;
      end
    in
    loop (init, prefix);
    !max_len

  let solve_part1 () =
    let prefix = read_input () in
    let s = bfs (0,0) (3,3) prefix in
    Solution.printf "%s" s

  let solve_part2 () =
    let prefix = read_input () in
    let n = dfs (0,0) (3,3) prefix in
    Solution.printf "%d" n

  end

let () = Solution.register_mod (module S)