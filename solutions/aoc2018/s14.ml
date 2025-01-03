open Utils
module S =
struct
  let name = Name.mk "s14"
  let read_input () =
    Input.read_line () |> int_of_string

  let simulate part1 n =
    let recipes = Dynarray.create 1024 in
    let () =
      Dynarray.push recipes 3;
      Dynarray.push recipes 7
    in
    let push1, stop1  =
      Dynarray.push,
      (fun () ->
         if (Dynarray.length recipes) >= n + 10 then begin
           let acc = ref  [] in
           for i = n + 9 downto n do
             acc := (Char.chr (Dynarray.get recipes i + Char.code '0'))::!acc;
           done;
           Some (String.implode !acc)
         end else None)
    in
    let push2, stop2 =
      let orig =
        String.explode (string_of_int n) |> List.map (fun c -> Char.code c - Char.code '0')
      in
      let pattern = ref orig in
      let res = ref "" in
      (fun a v ->
         begin match !pattern with
             [] -> ()
           | c :: l -> if c = v then begin
               pattern := l;
               if l = [] then res := string_of_int (1 + Dynarray.length a - List.length orig);
             end else pattern := orig
         end;
         Dynarray.push a v),
      (fun () -> if !res = "" then None else Some !res)
    in
    let rec loop push stop i1 i2 =
      match stop () with
        Some s -> s
      | None ->
        let n1 = Dynarray.get recipes i1 in
        let n2 = Dynarray.get recipes i2 in
        let r = n1 + n2 in
        let r = if r >= 10 then (push recipes 1; r-10) else r in
        let () = push recipes r in
        let len = Dynarray.length recipes in
        let i1 = (i1 + 1 + n1) mod len in
        let i2 = (i2 + 1 + n2) mod len in
        loop push stop i1 i2
    in
    if part1 then loop push1 stop1 0 1
    else loop push2 stop2 0 1

  let solve part1 =
    let n = read_input () in
    let s = simulate part1 n in
    Solution.printf "%s" s
  let solve_part1 () = solve true
  let solve_part2 () = solve false
end

let () = Solution.register_mod (module S)