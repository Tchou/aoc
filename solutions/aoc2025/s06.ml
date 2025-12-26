open Utils
open Grid
module S =
struct
  let name = Name.mk "s06"

  let separate s =
    String.split_on_char ' ' s 
    |> List.map String.trim
    |> List.filter ((<>) "")

  let rec split_last l =
    match l with
      [] -> assert false
    | x::[] -> [], x |> separate
    | x::ll -> let ll, y = split_last ll in x::ll, y

  let read_input () =
    Input.list_lines Fun.id 
    |> split_last


  let prepare1 num_lines =
    num_lines
    |> List.map (fun l -> 
        List.map int_of_string (separate l)
        |> Array.of_list)
    |> Array.of_list

  let compute1 num_lines l =
    let data = prepare1 num_lines in
    let data = IntGrid.of_matrix data in
    let data = IntGrid.rotate_right data in
    Iter2.(
      l
      |> ilist
      |> map (fun (i, op) ->
          IntGrid.get_line data i 
          |> array
          |> (if op = "+" then sum else prod)
            int)
      |> sum int)

  let solve compute () =
    let data, l = read_input () in
    let n = compute data l in 
    Solution.printf "%d" n

  let compute2 data l =
    let data = Array.of_list data in
    let data = StringGrid.of_array data in
    let data = StringGrid.rotate_left data in
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
        Iter2.((if op = "+" then sum else prod)
                 int (list ints)
              ) + acc) 0 data l
  let solve_part1 = solve compute1
  let solve_part2 = solve compute2
end

let () = Solution.register_mod (module S)