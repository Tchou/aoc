open Utils
open Syntax
module S =
struct
  let name = Name.mk "s14"

  let read_input () =
    Input.list_scan "%s can fly %d km/s for %d seconds, but then must rest for %d seconds."
      (fun n s t1 t2 -> (n,s,t1,t2))


  let compute_dist duration (_name, speed, t_run, t_rest) =
    let t = t_run + t_rest in
    let n = duration / t in
    let dist = speed * n * t_run in 
    dist + ((min t_run (duration mod t)) * speed)

  let max_dist duration l =
    l 
    |> List.map (compute_dist duration)
    |> Iter.(max list)
  let solve_part1 () =
    let reindeers = read_input () in
    let n = max_dist 2503 reindeers in
    Solution.printf "%d" n

  let run_race duration reindeers =
    let tbl = ~%(List.map (fun (n,a,b,c) -> n, ((a,b,c), 0, 0)) reindeers) in
    let max_in_round = ref [] in
    let max_dist = ref 0 in
    for second = 1 to duration do
      Hashtbl.filter_map_inplace (fun n ((speed, t_run, t_rest) as d,dist,points) ->
          let ndist =             
            let smod = second mod (t_run + t_rest) in
            if 0 < smod && smod  <= t_run then
              dist + speed else dist
          in 
          let () =
            if ndist > !max_dist then begin
              max_in_round := [ n ];
              max_dist := ndist
            end
            else if ndist = !max_dist then 
              max_in_round := n :: !max_in_round
          in
          Some (d, ndist, points)
        ) tbl;
      List.iter (fun name -> 
          let d, dist, points = tbl.%{name} in 
          tbl.%{name} <- d, dist, 1+ points) !max_in_round;
      max_dist := 0;
      max_in_round := [];
    done;
    tbl 
    |> Hashtbl.to_seq_values
    |> Seq.map (fun (_, _, p) -> p)
    |> Iter.(max Fun.id)

  let solve_part2 () =
    let reindeers = read_input () in
    let n = run_race 2503 reindeers in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)