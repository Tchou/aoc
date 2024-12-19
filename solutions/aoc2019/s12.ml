open Utils

module S =
struct
  let name = Name.mk "s12"

  type t = int array
  let _X = 0
  let _Y = 1
  let _Z = 2
  let pp fmt v = Format.fprintf fmt "<x=%d, y=%d, z=%d>" v.(_X) v.(_Y) v.(_Z)

  let ( +! ) a b =
    let r = Array.copy a in
    for i = 0 to Array.length r - 1 do
      r.(i) <- r.(i) + b.(i)
    done;
    r
  let sum = Array.fold_left (fun acc x -> acc + abs x) 0
  let ( ~-! ) = Array.map (~-)
  let zero = Array.make 3 0
  let mk i v =
    let r = Array.copy zero in
    r.(i) <- v; r
  let plus_one = Array.mapi (fun i _ -> mk i 1) zero
  let minus_one = Array.map (~-!) plus_one
  let compare_vect f m1 m2 =
    if m1.(f) < m2.(f) then plus_one.(f)
    else if m1.(f) = m2.(f) then zero
    else minus_one.(f)

  let read_input () =
    Input.fold_scan "<x=%d, y=%d, z=%d>" (fun acc x y z ->
        [|x;y;z|]::acc) []
    |> List.rev
    |> Array.of_list

  let make_key f moons = Array.map (fun m -> m.(f)) moons

  let equal_key f key moons velocities =
    Array.for_all (fun v -> v.(f) = 0) velocities &&
    Array.for_all2 (fun m mm -> m = mm.(f)) key moons

  exception Found of int array
  let simulate round moons =
    let dims = Array.length moons.(0) in
    let keys = Array.mapi (fun i _ -> make_key i moons) moons.(0) in
    let found = Array.map (fun _ -> -1) moons.(0) in
    let finished = ref 0 in
    let velocities = Array.map (fun _ -> zero) moons in
    let new_velocity = Array.copy velocities in
    let len = Array.length velocities in
    let rec loop r =
      if r != round then begin
        Array.blit velocities 0 new_velocity 0 len;
        for i = 0 to len - 1 do
          let m1 = moons.(i) in
          for j = i+1 to len - 1 do
            let m2 = moons.(j) in
            let v1 = ref (compare_vect 0 m1 m2) in
            for i = 1 to dims - 1 do
              v1 := !v1 +! compare_vect i m1 m2;
            done;
            new_velocity.(i) <- new_velocity.(i) +! !v1;
            new_velocity.(j) <- new_velocity.(j) +! ~-! !v1;
          done;
        done;
        for i = 0 to len - 1 do
          let v = new_velocity.(i) in
          velocities.(i) <- v;
          moons.(i) <- moons.(i) +! v;
        done;
        let nr = r + 1 in
        if round < 0 then begin
          for i = 0 to dims - 1 do
            if found.(i) < 0 && equal_key i keys.(i) moons velocities then begin
              found.(i) <- nr;
              incr finished;
            end
          done;
          if !finished = dims then raise (Found found);
        end;
        loop nr
      end
    in
    loop 0;velocities

  let energy moons velocities =
    let acc = ref 0 in
    for i = 0 to Array.length moons - 1 do
      acc := !acc + sum moons.(i) * sum velocities.(i);
    done;
    !acc
  let solve_part1 () =
    let moons = read_input () in
    let velocities = simulate 1000 moons in
    let n = energy moons velocities in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part2 () =
    let moons = read_input () in
    let n =
      try
        ignore (simulate (-1) moons); -1
      with Found tab -> Array.fold_left (Math.lcm) 1 tab
    in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

end

let () = Solution.register_mod (module S)