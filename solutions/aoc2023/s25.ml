open Utils
open Syntax

module S =
struct
  let name = Name.mk "s25"

  module Graph =
  struct
    type t = {
      e : (string , int * string list) Hashtbl.t;
      v : (string * int) array;
      mutable size : int
    }

    let copy g = { e = Hashtbl.copy g.e; v = Array.copy g.v; size = g.size }

    let rec merge_edges u v u_edges v_edges acc =
      match u_edges, v_edges with
        [], [] -> acc
      | [], v'::v_edges' ->
        merge_edges u v [] v_edges' (if v'= u then acc else v'::acc)
      | u'::u_edges', _ ->
        merge_edges u v u_edges' v_edges (if u' = v then acc else u'::acc)

    let rec contract g u v =
      let iv, v_edges = g.e.%{v} in
      let iu, u_edges = g.e.%{u} in
      if iv < iu then contract g v u else
        let () = List.iter (fun w ->
            if w <> u then
              let iw, w_edges = g.e.%{w} in
              g.e.%{w} <- (iw, List.map (fun x -> if x=v then u else x) w_edges)
          ) v_edges
        in
        let uv_edges = merge_edges u v u_edges v_edges [] in
        g.e.%{u} <- iu, uv_edges;
        g.v.(iu) <- u, snd g.v.(iv) + snd g.v.(iu);
        g.e %- v;
        g.size <- g.size - 1;
        if iv < g.size then begin
          g.v.(iv) <- g.v.(g.size);
          let _, l = g.e.%{fst g.v.(iv)} in
          g.e.%{fst g.v.(iv)} <- (iv, l)
        end

    let build lst =
      let e = ~%[] in
      let size = ref ~-1 in
      let get_size () = incr size;!size in
      let () = List.iter (fun (u, ul) ->
          let iu, old_ul =
            match e.%?{u} with
              Some (i,l) -> i, l
            | None -> get_size (),  []
          in
          let ul = List.sort compare ul in
          let ul = List.map (fun v ->
              begin match e.%?{v} with
                  Some (i, l) -> e.%{v} <- i, List.merge compare [u] l
                | None -> e.%{v} <- get_size (),[u]
              end;
              v) ul
          in
          e.%{u} <- iu, List.merge compare old_ul ul;
        ) lst
      in
      let size = get_size () in
      let v = Array.make size ("",0) in
      Hashtbl.iter (fun u (i,l) -> v.(i) <- u,1) e;
      {v;e;size}
  end

  let load_input () =
    Input.fold_scan "%[a-z]: %[a-z ]" (fun acc u s ->
        (u, String.split_on_char ' ' s)::acc) []
    |> Graph.build

  let karger ?(limit=2) g =
    let g = Graph.copy g in
    while g.Graph.size > limit do
      let iu = Random.int g.Graph.size in
      let u,_ = g.v.(iu) in
      let _, ul = g.Graph.e.%{u} in
      let n = List.length ul in
      let v = List.nth  ul (Random.int n) in
      Graph.contract g u v;
    done;
    g, List.length (snd g.Graph.e.%{fst g.Graph.v.(0)})

  let rec solve i g =
    let (g', n), f = Time.time karger g in
    if n > 3 && i < g.Graph.size then solve (i+1) g
    else if n <= 3  then
      (snd g'.Graph.v.(0)) * (snd g'.Graph.v.(1))
    else min_int
  let solve_part1 () =
    Random.self_init ();
    load_input  ()
    |> solve 1
    |> Solution.printf "%d"
  let solve_part2 () = ()
end

let () = Solution.register_mod (module S)