open Utils
module S =
struct
  let name = Name.mk "s17"


  let read_map queue =
    queue
    |> Queue.to_seq
    |> Seq.map Char.chr
    |> String.of_seq
    |> String.split_on_char '\n'
    |> List.filter ((<>) "")
    |> Array.of_list

  let get_map code =
    let state = Intcode.make_state code in
    let res = Intcode.eval state in
    assert (res =`halt);
    read_map state.stdout

  let all_dirs = [ 'v',(0, 1); '^',(0, -1); '>',(1, 0); '<',(-1, 0)]

  let (+!) (x, y) (i, j) = x + i, y + j

  let count_inter map =
    let acc = ref 0 in
    let h = Array.length map in
    let w = String.length map.(0) in
    let all_dirs = (0, 0) :: List.map snd all_dirs in
    for y = 1 to h - 2 do
      for x = 1 to w - 2 do
        if List.for_all (fun (i, j) ->
            let xi, yj = (x, y) +! (i, j) in
            map.(yj).[xi] <> '.'
          ) all_dirs then
          acc := !acc + y * x;
      done
    done;
    !acc

  let solve_part1 () =
    let code = Intcode.read () in
    let map = get_map code in
    let n = count_inter map in
    Array.iter (Format.printf "%s\n%!") map;
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let right (i, j) = (-j, i)
  let left (i, j)  = (j, -i)

  type move = Left | Right | Forward of int | Fun of string

  let all_subsequences l =
    let a = Array.of_list l in
    let acc = ref [] in
    for i = 0 to Array.length a - 1 do
      for j = i+1 to Array.length a  do
        acc := (Array.sub a i (j-i) |> Array.to_list, i)::!acc
      done
    done;
    List.sort (fun (l1,i) (l2,j) ->
        let c = compare (List.length l2) (List.length l1) in
        if c = 0 then compare (l1,i) (l2,j) else c
      ) !acc

  let pp fmt l =
    let s = l |> List.map (function Left -> "L"
                                  | Right -> "R"
                                  | Forward n -> string_of_int n
                                  | Fun s -> s)
            |> String.concat ","
    in
    Format.fprintf fmt "%s" s

  let subsequence_candidates l =
    let rec loop l acc =
      match l, acc with
        [], _  -> acc

      | (l1, i1):: ll, ((l2, i2)::al)::accl ->
        if l1 = l2 && i1 <> i2 then
          loop ll (((l1, i1)::(l2, i2)::al)::accl)
        else
          loop ll ([(l1, i1)]::acc)
      | e :: ll, [] -> loop ll [[e]]
      | e :: ll, []::_ -> loop ll ([e]::acc)
    in
    loop l [[]] 
    |> List.filter (fun l ->
        List.length l > 1 &&
        List.for_all (fun (ll,_) ->
            let s = Format.asprintf "%a" pp ll in
            String.length s <= 20 &&
            String.length s >= 2
          ) l
      ) |> List.rev

  let list_to_string l = Format.asprintf "%a" pp l

  let tab_to_string tab =
    let l = Array.to_list tab in
    let l = List.filter (function Fun ("A"|"B"|"C") -> true | _ -> false) l in
    list_to_string l

  let debug_tab fmt tab =
    Format.fprintf fmt "%a" pp (Array.to_list tab)

  let find_mapping l candidates =
    let source = Array.of_list l in
    let num_candidates = Array.length candidates in
    let forall_range check tab pos len =
      let rec loop i =
        if i >= len then true
        else check tab.(pos+i) && loop (i+1)
      in
      loop 0
    in
    let valid_insert =
      forall_range (function Fun _ -> false | _ -> true)
    in
    let valid_tab tab =
      forall_range (function Fun _ -> true | _ -> false) tab 0 (Array.length tab)
      && let s = tab_to_string tab in String.length s <= 20
    in
    let replace tab i s len =
      for k = i+1 to i+len - 1 do
        tab.(k) <- Fun "_"
      done;
      tab.(i) <- Fun s
    in
    let valid_candidate cand tab s =
      match List.filter (fun (l, i) -> valid_insert tab i (List.length l)) cand with
        [] -> false
      | cand -> List.iter (fun (l, i) -> replace tab i s (List.length l)) cand ; true
    in
    let exception Found of (string * string * string * string) in
    try
      for i = 0 to num_candidates - 1 do
        let tab_a = Array.copy source in
        if valid_candidate candidates.(i) tab_a "A" then begin
          for j = 0 to num_candidates - 1 do
            if i <> j then
              let tab_b = Array.copy tab_a in
              if valid_candidate candidates.(j) tab_b "B" then begin
                for k = 0 to num_candidates - 1 do
                  if i <> k && j <> k then
                    let tab_c = Array.copy tab_b in
                    if valid_candidate candidates.(k) tab_c "C" then begin
                      if valid_tab tab_c then
                        let a = fst (List.hd candidates.(i)) in
                        let b = fst (List.hd candidates.(j)) in
                        let c = fst (List.hd candidates.(k)) in
                        raise (Found (tab_to_string tab_c, list_to_string a, list_to_string b, list_to_string c))
                    end
                done;
              end;
          done;
        end;
      done;
      ("FAILED","","","")
    with Found r-> r




  let explore map =
    let exception Found of (int * int * (int * int)) in
    (* Find the initial position and orientation *)
    let x0, y0, dir0 = try
        map
        |> Array.iteri (fun y s ->
            s
            |> String.iteri (fun x c ->
                if c <> '#' && c <> '.' then
                  raise (Found (x, y, List.assoc c all_dirs))));
        (-1,-1, (0,0))
      with
        Found r -> r
    in
    let all_dirs = List.map snd all_dirs in
    let h = Array.length map in
    let w = String.length map.(0) in
    let valid x y = x >= 0 && y >= 0 && x < w && y < h in
    (* We should be on the extremity of a path, look how much we should turn initially *)
    let dir = all_dirs |> List.find (fun dir ->
        let (x, y) = (x0, y0) +! dir in
        valid x y && map.(y).[x] = '#')
    in
    let first = if dir = dir0 then [Forward 1]
      else if dir = right dir0 then [Forward 1; Right]
      else if dir = left dir0 then [Forward 1; Left]
      else [Forward 1; Right; Right]
    in
    let forward l = match l with
      | Forward n :: ll -> (Forward (n+1))::ll
      | _ -> (Forward 1)::l
    in
    let rec loop (x, y) dir acc =
      let xn, yn = (x, y) +! dir in
      if valid xn yn && map.(yn).[xn] = '#' then
        loop (xn, yn) dir (forward acc)
      else let xn, yn = (x, y) +! (right dir) in
        if valid xn yn && map.(yn).[xn] = '#' then
          loop (xn, yn) (right dir) (forward (Right::acc))
        else let xn, yn = (x, y) +! (left dir) in
          if valid xn yn && map.(yn).[xn] = '#' then
            loop (xn, yn) (left dir) (forward (Left::acc))
          else acc
    in
    loop ((x0, y0)+! dir) dir first |> List.rev

  let solve_part2 () =
    let code = Intcode.read () in
    let map = get_map (Array.copy code) in
    let moves = explore map in
    Array.iter (Format.printf "%s\n%!") map;
    Format.printf "%a\n%!" pp moves;
    let smoves = all_subsequences moves in
    let cmoves = subsequence_candidates smoves |> Array.of_list in
    let (prog, fa, fb, fc) = find_mapping moves cmoves in
    let commands = [prog; fa; fb; fc; "n"] in
    let () = code.(0) <- 2; in
    let state = Intcode.make_state code in
    let () = List.iter (fun s ->
        String.iter (fun c ->
            Queue.push (Char.code c) state.stdin;
          ) s;
        Queue.push 10 state.stdin;
      ) commands
    in
    let res = Intcode.eval state in
    let n =  state.stdout |> Queue.to_seq |> List.of_seq |> List.rev |> List.hd in
    Ansi.(printf "%a%d%a\n%!" fg green n clear color)

end

let () = Solution.register_mod (module S)