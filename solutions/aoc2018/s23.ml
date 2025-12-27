open Utils
module S =
struct
  let name = Name.mk "s23"

  type bot = { x : int; y : int; z : int; r : int }
  let read_input () =
    let l = Input.fold_scan "pos=<%d,%d,%d>, r=%d"
        (fun acc x y z r -> {x;y;z;r}::acc) []
    in
    l, Iter.(l |> list |> max_ ~compare:(fun b1 b2 -> Int.compare b1.r b2.r))

  let dist b1 b2 =
    abs (b1.x - b2.x) + abs (b1.y - b2.y) + abs (b1.z - b2.z)

  let count_in_radius l bot =
    Iter.(l |> list |> count_if (fun b -> dist b bot <= bot.r))

  let solve_part1 () =
    let bots, mbot = read_input () in
    let n = count_in_radius bots mbot in
    Solution.printf "%d" n

  let pp fmt bot =
    Format.printf "{%d, %d, %d},%d" bot.x bot.y bot.z bot.r

  let in_range bot1 bot2 =
    dist bot1 bot2 <= bot1.r + bot2.r

  let middle l =
    let mi = Iter.(list l |> min_) in
    let ma = Iter.(list l |> max_) in
    mi + (ma - mi) / 2

  let origin = { x = 0; y = 0;z = 0; r = 0}
  let search_space bots =
    let init =
      let x = List.map (fun {x;_} -> x) bots |> middle in
      let y = List.map (fun {y;_} -> y) bots |> middle in
      let z = List.map (fun {z;_} -> z) bots |> middle in
      let b0 = {x; y; z; r = 0 } in
      let ml = List.map (dist b0) bots in
      { b0 with r = Iter.(ml |> list |> max_) }
    in
    let count_in_range box =
      Iter.(bots |> list |> count_if (in_range box))
    in
    let compare box1 box2 =
      let n1 = count_in_range box1 in
      let n2 = count_in_range box2 in
      let c = Int.compare n2 n1 in
      if c <> 0 then c else
        let r1 = dist box1 origin in
        let r2 = dist box2 origin in
        Int.compare r1 r2
    in
    let module PQ = Pqueue(struct type t = bot let compare = compare end) in
    let queue = PQ.create 16 in
    let () = PQ.add queue init in
    let rec loop () =
      let box = PQ.remove_min queue in
      if box.r = 0 then dist box origin else begin
        let r = (box.r + 1) / 3 in
        Iter.(
          let-> i = range int ~start:~-1 2 in
          let-> j = range int ~start:~-1 2 in
          let-> k = range int ~start:~-1 2 in
          PQ.add queue
            { x = box.x + i * r; y = box.y + j * r; z = box.z + k * r; r});
        loop ()
      end
    in
    loop ()

  let solve_part2 () =
    let bots, _ = read_input () in
    let n = search_space bots in
    Solution.printf "%d" n
end

let () = Solution.register_mod (module S)