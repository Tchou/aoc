open Utils
module S =
struct
  let name = Name.mk "s03"

  (* Why bother with regular expressions ? :D *)
  let read_input () =
    let b = Buffer.create 16 in
    Input.fold_chars (fun acc c -> Buffer.add_char acc c; acc) b
    |> Buffer.contents


  let read_muls txt =
    let s = String.to_seq txt in
    let rec read_m s acc =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons('m', ss) -> read_u ss acc
      | Seq.Cons(_ , ss) -> read_m ss acc
    and read_u s acc =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons('u', ss) -> read_l ss acc
      | Seq.Cons('m', ss) -> read_u ss acc
      | Seq.Cons(_ , ss) -> read_m ss acc
    and read_l s acc =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons('l', ss) -> read_lp ss acc
      | Seq.Cons('m', ss) -> read_u ss acc
      | Seq.Cons(_ , ss) -> read_m ss acc
    and read_lp s acc =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons('(', ss) -> read_num1 ss acc
      | Seq.Cons('m', ss) -> read_u ss acc
      | Seq.Cons(_ , ss) -> read_m ss acc
    and read_num1 s acc =
      match s () with
      | Seq.Nil -> acc
      | Seq.Cons('0'..'9' as d, ss) -> read_num1n ss (([d],[])::acc)
      | Seq.Cons('m', ss) -> read_u ss acc
      | Seq.Cons(_ , ss) -> read_m ss acc
    and read_num1n s acc =
      match s (), acc with
      | Seq.Nil,_ -> List.tl acc
      | Seq.Cons('0'..'9' as d, ss),(l1,[])::acc -> read_num1nn ss ((d::l1,[])::acc)
      | Seq.Cons(',' , ss),_ -> read_num2 ss acc
      | Seq.Cons('m', ss),_ -> read_u ss (List.tl acc)
      | Seq.Cons(_ , ss),_ -> read_m ss (List.tl acc)
    and read_num1nn s acc =
      match s (), acc with
      | Seq.Nil,_ -> List.tl acc
      | Seq.Cons('0'..'9' as d, ss),(l1,[])::acc -> read_comm ss ((d::l1,[])::acc)
      | Seq.Cons(',' , ss),_ -> read_num2 ss acc
      | Seq.Cons('m', ss),_ -> read_u ss (List.tl acc)
      | Seq.Cons(_ , ss),_ -> read_m ss (List.tl acc)
    and read_comm s acc =
      match s () with
      | Seq.Nil -> List.tl acc
      | Seq.Cons(',', ss) -> read_num2 ss acc
      | Seq.Cons('m', ss) -> read_u ss (List.tl acc)
      | Seq.Cons(_ , ss) -> read_m ss (List.tl acc)
    and read_num2 s acc =
      match s (), acc with
      | Seq.Nil, _ -> List.tl acc
      | Seq.Cons('0'..'9' as d, ss), (l1,[])::acc -> read_num2n ss ((l1,[d])::acc)
      | Seq.Cons('m', ss),_ -> read_u ss (List.tl acc)
      | Seq.Cons(_ , ss),_ -> read_m ss (List.tl acc)
    and read_num2n s acc =
      match s (), acc with
      | Seq.Nil,_ -> List.tl acc
      | Seq.Cons('0'..'9' as d, ss),(l1,l2)::acc -> read_num2nn ss ((l1,d::l2)::acc)
      | Seq.Cons(')' , ss),_ -> read_m ss acc
      | Seq.Cons('m', ss),_ -> read_u ss (List.tl acc)
      | Seq.Cons(_ , ss),_ -> read_m ss (List.tl acc)
    and read_num2nn s acc =
      match s (), acc with
      | Seq.Nil,_ -> List.tl acc
      | Seq.Cons('0'..'9' as d, ss),(l1,l2)::acc -> read_rp ss ((l1,d::l2)::acc)
      | Seq.Cons(')' , ss),_ -> read_m ss acc
      | Seq.Cons('m', ss),_ -> read_u ss (List.tl acc)
      | Seq.Cons(_ , ss),_ -> read_m ss (List.tl acc)
    and read_rp s acc =
      match s () with
      | Seq.Nil -> List.tl acc
      | Seq.Cons(')', ss) -> read_m ss acc
      | Seq.Cons('m', ss) -> read_u ss (List.tl acc)
      | Seq.Cons(_ , ss) -> read_m ss (List.tl acc)
    in
    read_m s []

  let l_to_int l =
    List.fold_left (fun acc c ->
        acc * 10 + (Char.code c - Char.code '0')) 0 (List.rev l)
  let compute l =
    List.fold_left (fun acc (l1, l2) ->
        let i1 = l_to_int l1 in
        let i2 = l_to_int l2 in
        acc + (i1 * i2)
      ) 0 l

  let compute l =
    List.fold_left (Agg.Left.sum (fun (i1, i2) -> l_to_int i1 * l_to_int i2 )) 0 l

  let simplify_text txt =
    let s = String.to_seq txt in
    let b = Buffer.create 16 in
    let rec read_d_dont s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('d', ss) -> read_o_dont ss
      | Seq.Cons(c, ss) -> Buffer.add_char b c; read_d_dont ss
    and read_o_dont s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('o', ss) -> read_n_dont ss
      | Seq.Cons('d', ss) -> read_o_dont ss
      | Seq.Cons(c, ss) -> Buffer.add_char b c; read_d_dont ss
    and read_n_dont s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('n', ss) -> read_'_dont ss
      | Seq.Cons('d', ss) -> read_o_dont ss
      | Seq.Cons(c, ss) -> Buffer.add_char b c; read_d_dont ss
    and read_'_dont s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('\'', ss) -> read_t_dont ss
      | Seq.Cons('d', ss) -> read_o_dont ss
      | Seq.Cons(c, ss) -> Buffer.add_char b c; read_d_dont ss
    and read_t_dont s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('t', ss) -> read_lp_dont ss
      | Seq.Cons('d', ss) -> read_o_dont ss
      | Seq.Cons(c, ss) -> Buffer.add_char b c; read_d_dont ss
    and read_lp_dont s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('(', ss) -> read_rp_dont ss
      | Seq.Cons('d', ss) -> read_o_dont ss
      | Seq.Cons(c, ss) -> Buffer.add_char b c; read_d_dont ss
    and read_rp_dont s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons(')', ss) -> read_d_do ss
      | Seq.Cons('d', ss) -> read_o_dont ss
      | Seq.Cons(c, ss) -> Buffer.add_char b c; read_d_dont ss
    and read_d_do s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('d', ss) -> read_o_do ss
      | Seq.Cons(_, ss) -> read_d_do ss
    and read_o_do s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('o', ss) -> read_lp_do ss
      | Seq.Cons(_, ss) -> read_d_do ss
    and read_lp_do s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons('(', ss) -> read_rp_do ss
      | Seq.Cons(_, ss) -> read_d_do ss
    and read_rp_do s =
      match s () with
        Seq.Nil -> ()
      | Seq.Cons(')', ss) -> Buffer.add_char b '#'; read_d_dont ss
      | Seq.Cons(_, ss) -> read_d_do ss
    in
    read_d_dont s;
    Buffer.contents b


  let solve part2 =
    let s = read_input () in
    let s = if part2 then simplify_text s else s in
    let l = read_muls s in
    let n = compute l in
    Ansi.(printf "%a%d%a\n" fg green n clear color)


  let solve_part1 () = solve false
  let solve_part2 () = solve true

end

let () = Solution.register_mod (module S)