open Utils
module S =
struct
  let name = Name.mk "s03"
  (*    BBBBBBB
        BBBBBBB
        AAAAAA
        AAAAAA
  *)
  module Rect =
  struct
    type t = { width : Interval.t; height : Interval.t }

    let pp fmt r = Format.fprintf fmt "%d,%d: %dx%d"
        r.width.Interval.inf
        r.height.Interval.inf
        Interval.(length r.width)
        Interval.(length r.height)

    let make x y w h =
      { width = Interval.{inf = x; sup = x + w };
        height = Interval.{inf = y; sup = y + h} }
    let cap r1 r2 =
      match Interval.cap r1.width r2.width,
            Interval.cap r1.height r2.height with
        Some width, Some height -> Some {width; height}
      | _ -> None
(*
    We use the identity
    (X1,Y1)\(X2,Y2) = (X1\X2, Y1) U (X1, Y1\Y2),
    but modified so that the unions are disjoint:
    (X1\X2, Y1&Y2) U (X1&X2, Y1\Y2) U (X1\X2, Y1\Y2)
*)
    let diff r1 r2 =
      let dw = Interval.diff r1.width r2.width in
      let dh = Interval.diff r1.height r2.height in
      let iw = Interval.cap r1.width r2.width |> Option.to_list in
      let ih = Interval.cap r1.height r2.height |> Option.to_list in
      let s1 = Comb.product dw ih in
      let s2 = Comb.product iw dh in
      let s3 = Comb.product dw dh in
      Seq.append s1 (Seq.append s2 s3)
      |> Seq.map (fun (width, height) -> {width; height})
      |> List.of_seq


    let add r l =
      let rec loop r l =
        match l with
          [] -> [ r ]
        |  r0 :: ll ->
          match cap r r0 with
            None -> r0 :: loop r ll
          | Some r_int -> (diff r0 r) @ [ r_int ] @
                          List.fold_left (fun acc rx -> loop rx acc) ll (diff r r0)
      in
      loop r l

    let cup l1 l2 = List.fold_left (fun acc r -> add r acc) l2 l1

  end


  let read_input () =
    Input.list_scan "#%d @ %d,%d: %dx%d" 
      (fun id x y w h ->
         (id, Rect.make x y w h))

  let count_inter l =
    Iter.(
      l
      |> list
      |> pairs ~sym:false ~refl:false
      |> fold (fun acc ((id1, r1), (id2,r2)) ->
          match Rect.cap r1 r2 with
            None -> acc
          | Some r -> Rect.add r acc
        ) []
      |> list
      |> map (fun Rect.{width;height} ->
          Interval.length width * Interval.length height)
      |> sum int)
  let solve_part1 () =
    let l = read_input () in
    let n = count_inter l in
    Solution.printf "%d" n

  let find_single rects =
    List.find (fun (_, r) ->
        List.for_all (fun (_,r') -> r == r' || Rect.cap r r' = None) rects)
      rects
    |> fst
  let solve_part2 () =
    let l = read_input () in
    let n = find_single l in
    Solution.printf "%d" n

end

let () = Solution.register_mod (module S)