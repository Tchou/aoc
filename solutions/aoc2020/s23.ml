open Utils

module S =
struct
  let name = Name.mk "s23"

  module Dll = struct
    type t = { value : int; mutable next : t; mutable prev : t}

    let make n =
      let rec t = { value = n; next = t; prev = t } in t

    let push_before t v =
      let t2 = { value = v ; next = t; prev = t.prev } in
      t.prev <- t2;
      t2.prev.next <- t2
    let push_after t v =
      let t2 = {value = v; next = t.next; prev = t} in
      t.next <- t2;
      t2.next.prev <- t2

    let pop_after t =
      if t.next == t then failwith "pop"
      else
        let v = t.next.value in
        (* t t1 t2 *)
        t.next <- t.next.next;
        t.next.prev <- t;
        v

    let iter f t =
      let rec loop t' =
        if t != t' then begin
          f t';
          loop t'.next;
        end
      in
      f t;
      loop t.next

    let pp fmt t =
      iter (fun t -> Format.fprintf fmt "%d" t.value) t
  end

  let read_input () =
    let s = read_line () in
    let t = Dll.make (Char.code s.[0] - Char.code '0') in
    for i = 1 to String.length s - 1 do
      Dll.push_before t (Char.code s.[i] - Char.code '0')
    done;
    t

  let play max_val rounds t =
    let dummy = Dll.make (-1) in
    let map = Array.make (max_val + 1) dummy in
    let remove_from_map c = map.(c) <- dummy in
    let is_in_map c = map.(c) != dummy in
    Dll.iter (fun t -> map.(t.value) <- t) t;
    let decr_wrap n =
      if n = 1 then max_val else n - 1
    in
    let rec step1 n current =
      let c1 = Dll.pop_after current in
      let c2 = Dll.pop_after current in
      let c3 = Dll.pop_after current in
      remove_from_map c1;
      remove_from_map c2;
      remove_from_map c3;
      step2 n current (decr_wrap (current.value)) c1 c2 c3
    and step2 n current dest_num c1 c2 c3 =
      if not (is_in_map dest_num) then step2 n current (decr_wrap dest_num) c1 c2 c3
      else
        step3 n current (map.(dest_num)) c1 c2 c3
    and step3 n current dest c1 c2 c3 =
      Dll.push_after dest c3;
      map.(c3) <- dest.next;
      Dll.push_after dest c2;
      map.(c2) <- dest.next;
      Dll.push_after dest c1;
      map.(c1) <- dest.next;
      if n = 1 then ()
      else step1 (n-1) current.next
    in
    step1 rounds t;
    map.(1)

  let collect t =
    let acc = ref "" in
    Dll.iter (fun t -> if t.value <> 1 then
                 acc := !acc ^ string_of_int t.value) t;
    !acc

  let fill_million current =
    for i = 10 to 1000000 do
      Dll.push_before current i;
    done;
    current


  let solve_part1 () =
    let t = read_input () in
    let t1 = play 9 100 t in
    let s = collect t1 in
    Ansi.(printf "%a%s%a\n" fg green s clear color)

  let solve_part2 () =
    let t = read_input () in
    let t = fill_million t in
    let t1 = play 1_000_000 10_000_000 t in
    let n = t1.next.value * t1.next.next.value in
    Ansi.(printf "%a%d%a\n" fg green n clear color)

end

let () = Solution.register_mod (module S)