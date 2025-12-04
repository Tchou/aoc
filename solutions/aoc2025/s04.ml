open Utils
open Grid

module S =
struct
  let name = Name.mk "s04"

  let read_input () = BytesGrid.read ()

  let less_than4 g pos =
    let empty = ref 0 in
    BytesGrid.iter8
      (fun _ d _ -> if d = '@' then incr empty) g pos;
    !empty < 4

  let count_rolls g =
    let total = ref 0 in
    BytesGrid.iter
      (fun pos c ->
         if c = '@' && less_than4 g pos then
           incr total)
      g;
    !total

  let copy_into g_from g_to =
    let h = BytesGrid.height g_from in
    let w = BytesGrid.width g_from in
    for i = 0 to h - 1 do
      let lf = BytesGrid.get_line g_from i in
      let lt = BytesGrid.get_line g_to i in
      Bytes.blit lf 0 lt 0 w
    done

  let remove_less4 g_from g_to =
    copy_into g_from g_to;
    let total = ref 0 in
    BytesGrid.iter
      (fun pos c ->
         if c = '@' && less_than4 g_from pos then begin
           incr total;
           BytesGrid.( g_to.!(pos) <- '.')
         end)
      g_from;
    !total

  let remove_all_possible g_from =
    let g_to = BytesGrid.copy g_from in
    let rec loop g_from g_to acc =
      let n = remove_less4 g_from g_to in
      if n = 0 then acc else
        loop g_to g_from (acc + n)
    in
    loop g_from g_to 0

  let solve_part1 () =
    let g = read_input () in
    let n = count_rolls g in 
    Solution.printf "%d" n

  let solve_part2 () =
    let g = read_input () in
    let n = remove_all_possible g in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)