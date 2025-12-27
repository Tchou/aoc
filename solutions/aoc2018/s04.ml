open Utils
open Syntax

module S =
struct
  let name = Name.mk "s04"


  let read_lines () =
    Input.list_scan "[%s %d:%d] %[0-9A-Za-z# ]"
      (fun date h m txt -> (date, (h,m), txt))

  let read_input () =
    let map = ~%[] in
    let rec loop_guards l =
      match l with
        [] -> map
      | (date, (h,m), txt)::ll ->
        let id = Scanf.sscanf txt "Guard #%d begins shift" Fun.id in
        let events, ll' = loop_events ll [] in
        map.%{id} <- events @ (map.%?{id} or []);
        loop_guards ll'
    and loop_events l acc =
      match l with
        [] -> acc, []
      | (_, _, txt)::_ when String.starts_with ~prefix:"Guard" txt -> acc, l
      | (date1, t1, "falls asleep")::(date2, t2, "wakes up") :: ll ->
        assert (date1 = date2);
        loop_events ll ((date1, (t1, t2))::acc)
      | _ -> assert false
    in
    loop_guards (List.sort compare (read_lines()))

  let max_asleep map =
    Hashtbl.fold (fun id events (aid, an) ->
        let n =
          Iter.(
            events
            |> list
            |> map (fun (_, ((_, m1), (_, m2))) -> m2 - m1) 
            |> sum int)
        in
        if n > an then (id, n) else (aid, an)
      ) map (-1,-1)
    |> fst

  let max_minute id map =
    let counts = Array.make 60 0 in
    map.%{id}
    |> List.iter (fun (_, ((_, m1), (_, m2))) ->
        for i = m1 to m2 - 1 do
          counts.(i) <- 1 + counts.(i)
        done);
    let m, v, _ = Array.fold_left (fun (acci, accv, i) v ->
        if v > accv then (i, v, i+1)
        else (acci, accv, i+1)
      ) (-1, -1, 0) counts
    in m, v

  let max_asleep_on_minute map =
    let id, m, _ =
      Hashtbl.fold (fun id _ ((_, _, accv) as acc) ->
          let m, v = max_minute id map in
          if v > accv then (id, m, v) else acc
        ) map (-1,-1,-1)
    in id, m

  let solve_part1 () =
    let map = read_input () in
    let id = max_asleep map in
    let minute, _ = max_minute id map in
    let n = id * minute in
    Solution.printf "%d" n
  let solve_part2 () =
    let map = read_input () in
    let id, minute = max_asleep_on_minute map in
    let n = id * minute in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)