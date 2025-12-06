open Utils
open Grid
module S =
struct
  let name = Name.mk "s06"

  let rec read_strings acc l =
    match l with
    | [ s::_ as l0 ] when s = "+" || s = "*" ->
      List.rev acc |> Array.of_list, l0
    | (s :: _ as l0) :: ll when s.[0] >= '0' && s.[0] <= '9' ->
      read_strings ((l0
                     |> List.map int_of_string
                     |> Array.of_list)::acc) ll
    | _ -> assert false

  let read_input () =
    Input.list_lines (fun s -> String.split_on_char ' ' s 
                               |> List.map String.trim
                               |> List.filter ((<>) ""))
    |> read_strings []

  let compute data l =
    let data = IntGrid.of_matrix data in
    let data = IntGrid.rotate_right data in
    l
    |> List.mapi (fun i op ->
        let args = IntGrid.get_line data i in
        Iter.((if op = "+" then sum else prod)
                array (module Int) args))
    |> Iter.(sum list (module Int))

  let solve_part1 () =
    let data, l = read_input () in
    let n = compute data l in 
    Solution.printf "%d" n


  let rec split_last l =
    match l with
      [] -> assert false
    | x::[] -> [], x |> String.split_on_char ' ' |> List.filter ((<>)"")
    | x::ll -> let ll, y = split_last ll in x::ll, y


  let read_input () =
    Input.list_lines Fun.id 
    |> split_last 

  let compute2 data l =
    let data = Array.of_list data in
    let data = StringGrid.of_array data in
    let data = StringGrid.rotate_left data in
    let () = List.iter (Format.printf "%s\n%!") l in
    let subtotal = ref [] in
    let total = ref [] in
    StringGrid.iter_lines (fun s -> 
        let s = String.trim s in
        if s = "" then begin
          total := !subtotal :: !total;
          subtotal := [];
        end else 
          subtotal := int_of_string s :: !subtotal
      ) data;
    let data =  !subtotal::!total in
    List.fold_left2 (fun acc ints op ->
        Iter.((if op = "+" then sum else prod)
                list (module Int) ints
             ) + acc) 0 data l

  let solve_part2 () = 
    let data, l = read_input () in 
    let n = compute2 data l in 
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)