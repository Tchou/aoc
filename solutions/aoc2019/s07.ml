open Utils
module S =
struct
  let name = Name.mk "s07"

  let max_thruster code =
    let max_signal = ref 0 in
    let orig_state = Intcode.make_state code in
    Comb.perm [ 0; 1; 2; 3; 4 ]
    |> Seq.iter (fun l ->
        let signal =
          l
          |> List.fold_left (fun acc psetting ->
              let state = Intcode.copy_state orig_state in
              Queue.push psetting state.stdin;
              Queue.push acc state.stdin;
              ignore (Intcode.eval state);
              Queue.take state.stdout
            ) 0
        in
        if signal > !max_signal then max_signal := signal
      );
    !max_signal
  let solve_part1 () =
    let code = Intcode.read () in
    let n = max_thruster code in
    Ansi.(printf "%a%d%a\n" fg green n clear color)


  let feedback_loop code settings =
    let state = Intcode.make_state code in
    let all_states = Array.make 5 state in
    for i = 1 to 4 do
      all_states.(i) <- Intcode.{ state with
                                  code = Array.copy code;
                                  stdin = all_states.(i-1).stdout;
                                  stdout = if i < 4 then Queue.create() else state.stdin;
                                };
    done;
    for i = 0 to 4 do
      Queue.push (List.nth settings i) all_states.(i).stdin
    done;
    Queue.push 0 all_states.(0).stdin;
    let need_input = Array.make 5 `need_input in
    let rec loop i =
      if need_input.(i)<>`need_input then if i = 4 then Queue.take all_states.(4).stdout
        else loop (i+1)
      else begin
        need_input.(i) <- Intcode.eval all_states.(i);
        loop ((i+1) mod 5)
      end
    in loop 0

  let find_max_output code =
    let max_output = ref 0 in
    Comb.perm [ 5;6;7;8;9 ]
    |> Seq.iter (fun settings ->
        let r = feedback_loop code settings in
        if r > !max_output then max_output := r);
    !max_output

  let solve_part2 () =
    let code = Intcode.read () in
    let n = find_max_output code in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

end

let () = Solution.register_mod (module S)