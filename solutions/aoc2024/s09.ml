open Utils
module S =
struct
  let name = Name.mk "s09"
  let read_input () = Input.read_line ()
  let total_length =
    String.fold_left (Agg.Left.sum (fun c -> Char.code c - Char.code '0')) 0

  let rle s =
    let blocks = ref [] in
    let idx = ref 0 in
    String.iteri (fun i c ->
        let id = if i mod 2 = 0 then i / 2 else -1 in
        let len = Char.code c - Char.code '0' in
        blocks:= (!idx, len, id) :: !blocks;
        idx := !idx + len;
      ) s;
    List.rev !blocks

  let rec insert_split spaces ((fidx, flen, fid) as file) acc acc_spaces =
    match spaces with
      [] -> assert false (* no space left on device ;) *)
    | (sidx, slen, _) :: nspaces ->
      if sidx > fidx then spaces, file::acc
      else if slen > flen then
        (sidx+flen, slen - flen, -1):: nspaces,
        (sidx, flen, fid)::acc
      else if slen = flen then
        nspaces, (sidx, flen, fid)::acc
      else (* slen < flen, consume what we can *)
        insert_split nspaces (fidx, flen-slen, fid)  ((sidx, slen, fid)::acc) acc_spaces

  let rec insert_fit spaces ((fidx, flen, fid) as file) acc_files acc_spaces =
    match spaces with
      [] -> List.rev acc_spaces, acc_files
    | ((sidx, slen, _) as space) :: nspaces ->
      if sidx > fidx then List.rev_append acc_spaces spaces, (file::acc_files)
      else if slen > flen then
        List.rev_append acc_spaces ((sidx+flen, slen - flen, -1):: nspaces),
        ((sidx, flen, fid)::acc_files)
      else if slen = flen then
        List.rev_append acc_spaces nspaces,
        ((sidx, flen, fid)::acc_files)
      else (* slen < flen, consume what we can *)
        insert_fit nspaces file acc_files (space::acc_spaces)
  
  let compact insert blocks =
    let rec loop spaces files acc_files =
      match files with
        [] -> acc_files
      | ((idx, len, id) as file) :: nfiles ->
        let nspaces, nacc = insert spaces file acc_files [] in
        loop nspaces nfiles nacc
    in
    let spaces, files =
      List.partition (fun (_,_, id) -> id < 0) blocks
    in
    loop spaces (List.rev files) []

  let checksum l =
    let acc = ref 0 in
    l |> List.iter (fun (idx, len, id) ->
        for i = idx to idx + len - 1 do
          acc := !acc + id * i;
        done
      );
    ! acc
  let solve insert =
    let s = read_input () in
    let blocks = rle s in
    let files = compact insert blocks in
    let n = checksum files in
    Solution.printf "%d" n

  let solve_part1 () = solve insert_split
  let solve_part2 () = solve insert_fit
end

let () = Solution.register_mod (module S)