open Utils
open Syntax
module S =
struct
  let name = Name.mk "s17"

  let read_input ?(part2=false) () =
    let y = ref 0 in
    let map = ~%[] in
    let len = if part2 then 4 else 3 in
    Input.fold_lines (fun _ line ->
        String.iteri (fun x c ->
            if c = '#' then
              let cube = Array.make len 0 in
              cube.(0) <- x;
              cube.(1) <- !y;
              map.%{cube} <- ()
          ) line;
        incr y
      ) ();
    map

  let iter_neighbours f cube =
    let dcube = Array.copy cube in
    let rec loop all_zero i =
      if i >= Array.length cube then begin
        if not all_zero then f dcube
      end else begin
        dcube.(i) <- cube.(i) -1;
        loop false (i+1);
        dcube.(i) <- cube.(i);
        loop all_zero (i+1);
        dcube.(i) <- cube.(i) + 1;
        loop false (i+1)
      end
    in
    loop true 0

  let count_and_mark inactives map cube =
    let count = ref 0 in
    iter_neighbours (fun n -> if map %? n then incr count
                      else
                        inactives.%{Array.copy n} <- 
                          1 + (inactives.%?{n} or 0)) cube;
    !count
  let count map cube =
    let count = ref 0 in
    iter_neighbours (fun n -> if map %? n then incr count) cube;
    !count

  let cycle map =
    let new_map = ~%[] in
    let inactives = ~%[] in
    Hashtbl.iter (fun cube () ->
        if map %? cube then
          let c = count_and_mark inactives map cube in
          if c == 2 || c == 3 then new_map.%{cube} <- ()
      ) map;
    Hashtbl.iter (fun cube n ->
        if n = 3 then new_map.%{cube} <- ()
      ) inactives;
    new_map
  let solve part2 =
    let map = ref (read_input ~part2 ()) in
    for _ = 1 to 6 do
      map := cycle !map;
    done;
    let n = Hashtbl.length !map in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

  let solve_part1 () = solve false
  let solve_part2 () = solve true
end

let () = Solution.register_mod (module S)