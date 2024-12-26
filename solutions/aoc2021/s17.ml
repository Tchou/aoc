open Utils
open Syntax

module S =
struct
  let name = Name.mk "s17"

  type probe = { x : int; y : int ; vx : int; vy : int }
  type area = { x_min : int; x_max : int; y_min : int; y_max : int}
  let step {x; y; vx; vy} =
    let x = x + vx in
    let y = y + vy in
    let vx =
      if vx < 0 then vx + 1 else if vx > 0 then vx - 1 else vx
    in
    let vy = vy - 1 in
    {x; y; vx; vy}

  let on_target probe area =
    area.x_min <= probe.x &&
    probe.x <= area.x_max &&
    area.y_min <= probe.y &&
    probe.y <= area.y_max

  let approaching probe area =
    (probe.x <= area.x_max && probe.vx > 0) ||
    (probe.y >= area.y_min)

  let simulate_until probe area =
    let y_max = ref min_int in
    let x_max = ref min_int in
    let rec loop probe n =
      y_max := max !y_max probe.y;
      x_max := max !x_max probe.x;
      if on_target probe area then (true, !x_max, !y_max)
      else
        let nprobe = step probe in
        if approaching probe area then loop nprobe (n+1)
        else (false, !x_max, !y_max)
    in
    loop probe 0

  let read_input () =
    Scanf.sscanf (Input.read_line ()) "target area: x=%d..%d, y=%d..%d"
      (fun x_min x_max y_min y_max -> {x_min;x_max;y_min;y_max})

  let debug_simu probe area y_max valid =
    let t, _, y = simulate_until probe area in
    if t then begin
      y_max := max !y_max y;
      valid.%{probe.vx, probe.vy} <- ();
    end

  let brute_force area limit =
    let probe = {x=0; y = 0; vx=0; vy=0 } in
    let y_max = ref 0 in
    let valid = ~%[] in
    for n = 0 to limit do
      for vx = 0 to n do
        debug_simu { probe with vx; vy=n} area y_max valid;
      done;
      for vy = n - 1 downto -n do
        debug_simu {probe with vx = n; vy} area y_max valid;
      done;
    done;
    !y_max, Hashtbl.length valid

  let solve_part1 () =
    let area = read_input () in
    let y_max,_ = brute_force area area.x_max in
    Solution.printf "%d" y_max

  let solve_part2 () =
    let area = read_input () in
    let _, distinct = brute_force area area.x_max in
    Solution.printf "%d" distinct

end

let () = Solution.register_mod (module S)