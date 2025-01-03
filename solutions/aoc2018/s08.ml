open Utils
module S =
struct
  let name = Name.mk "s08"
  type tree =
      Node of tree array * int array

  let read_input () =
    Input.read_line ()
    |> String.split_on_char ' '
    |> List.map int_of_string

  let rec repeat n f l =
    let rec loop n l acc =
      if n = 0 then List.rev acc, l else
        match l with
          [] -> assert false
        | _ -> let e, l' = f l in
          loop (n-1) l' (e::acc)
    in
    loop n l []

  let build_tree l =
    let rec node l =
      match l with
        children::metadata::ll ->
        let children, ll' = repeat children node ll in
        let metadata, ll'' = repeat metadata meta ll' in
        Node(Array.of_list children, Array.of_list metadata), ll''
      | _ -> assert false
    and meta l =
      match l with
        [] -> assert false
      | m :: ll -> m, ll
    in
    node l |> fst

  let sum t =
    let rec loop acc (Node (children, metadata)) =
      let acc = Array.fold_left (+) acc metadata in
      Array.fold_left loop acc children
    in
    loop 0 t


  let solve f =
    let l = read_input () in
    let t = build_tree l in
    let n = f t in
    Solution.printf "%d" n

  let sum2 t =
    let rec loop t =
      match t with
        Node([| |], metadata) -> Array.fold_left (+) 0 metadata
      | Node (children, metadata) ->
        Array.fold_left (fun acc i ->
            acc + (if i >= 1 && i <= Array.length children
                   then loop children.(i-1) 
                   else 0)
          ) 0 metadata
    in
    loop t

  let solve_part1 () = solve sum
  let solve_part2 () = solve sum2
end

let () = Solution.register_mod (module S)