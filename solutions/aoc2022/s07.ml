open Utils
open Syntax

type entry = Dir of (string, entry) Hashtbl.t | File of int

let rec pp_entry fmt t =
  match t with
  | Dir entries ->
      entries |> Hashtbl.to_seq |> List.of_seq |> List.sort compare
      |> pp_tree fmt
  | File s -> Format.fprintf fmt "(size=%d)" s

and pp_tree fmt l =
  List.iter
    (fun (name, e) -> Format.fprintf fmt "@[- %s @[%a@]@\n" name pp_entry e)
    l

let iter_on_dir_size f t =
  let rec loop t =
    match t with
    | File s -> s
    | Dir entries ->
        let size = Hashtbl.fold (fun _ e acc -> acc + loop e) entries 0 in
        f size; size
  in
  ignore (loop t)

let sum_size limit t =
  let acc = ref 0 in
  let f s = if s <= limit then acc := !acc + s in
  iter_on_dir_size f t; !acc

let find_dir total needed t =
  let acc = ref [] in
  let f size = acc := size :: !acc in
  iter_on_dir_size f t;
  match !acc with
  | root_size :: rest ->
      let available = total - root_size in
      assert (available < needed);
      let l = List.sort compare rest in
      List.find (fun s -> s + available >= needed) l
  | _ -> assert false

let solve compute () =
  let root_dir = ~%[] in
  let root_path = [ "/", root_dir ] in
  let _path =
    Input.fold_fields ' '
      (fun path cmd ->
        match path, cmd with
        | _, [ "$"; "cd"; "/" ] -> root_path
        | _ :: parent_path, [ "$"; "cd"; ".." ] -> parent_path
        | (_, cwd) :: _, [ "$"; "cd"; name ] -> begin
            if not (Hashtbl.mem cwd name) then cwd.%{name} <- Dir ~%[];
            match cwd.%{name} with
            | File _ -> failwith "Cannot cd into a file"
            | Dir d -> (name, d) :: path
          end
        | _, [ "$"; "ls" ] -> path
        | _, [ "dir"; _ ] -> path
        | (_, cwd) :: _, [ ssize; name ] ->
            cwd.%{name} <- File (int_of_string ssize);
            path
        | _ -> assert false)
      root_path
  in
  let t = Dir ~%[ "/", Dir root_dir ] in
  Solution.printf "%d" (compute t)

module Sol = struct
  let name = Name.mk "s07"
  let solve_part1 = solve (sum_size 100000)
  let solve_part2 = solve (find_dir 70000000 30000000)
end

let () = Solution.register_mod (module Sol)