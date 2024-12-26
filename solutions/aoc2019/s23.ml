open Utils
module S =
struct
  let name = Name.mk "s23"

  type network = { machines : Intcode.state array;
                   codes : [ `need_input | `full_output | `halt] array}
  let init_network n code =
    { machines = Array.init n (fun i ->
          let state = Intcode.make_state (Array.copy code) in
          Queue.push i state.stdin;
          state );
      codes = Array.make n `need_input }

  let run_network with_nat {machines; codes } =
    let exception Found of int in
    let x_nat = ref min_int in
    let y_nat = ref min_int in
    let last_y = ref min_int in
    let num_machines = Array.length machines in
    let idle_count = ref 0 in
    try
      while true do
        for i = 0 to  Array.length machines - 1 do
          match codes.(i) with
          | `need_input ->
            if Queue.is_empty machines.(i).stdin then begin
              incr idle_count;
              Queue.push (-1) machines.(i).stdin;
            end;
            codes.(i) <- Intcode.eval ~out_count:3 machines.(i)
          | `full_output ->
            let addr = Queue.pop machines.(i).stdout in
            let x = Queue.pop machines.(i).stdout in
            let y = Queue.pop machines.(i).stdout in
            if addr = 255 then
              if with_nat then begin
                x_nat := x;
                y_nat := y;
              end else raise (Found y)
            else begin
              Queue.push x machines.(addr).stdin;
              Queue.push y machines.(addr).stdin;
              decr idle_count;
            end;
            codes.(i) <- Intcode.eval ~out_count:3 machines.(i)
          | `halt  -> ()
        done;
        if with_nat && !x_nat != min_int && !y_nat != min_int &&
           !idle_count = num_machines then begin
          if !last_y = !y_nat then raise (Found !last_y);
          last_y := !y_nat;
          Queue.push !x_nat machines.(0).stdin;
          Queue.push !y_nat machines.(0).stdin;
        end;
        idle_count := 0;
      done;
      -1
    with Found y -> y


  let solve with_nat =
    let code = Intcode.read () in
    let network = init_network 50 code in
    let y = run_network with_nat  network in
    Solution.printf "%d" y
  let solve_part1 () = solve false

  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)