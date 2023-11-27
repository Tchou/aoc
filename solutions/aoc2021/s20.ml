open Utils

module S =
struct

  module Cache =
  struct
    type t = {
      data : Bytes.t;
      width : int;
      height : int;
      level : int;
      level_size : int;
    }
    let dummy = { data = Bytes.create 0; width = 0;
                  height = 0; level = 0; level_size = 0}
    let absent = 2
    let create w h l =
      let width = w + 4 * l in
      let height = h + 4 * l in
      let level_size = width * height in
      { data = Bytes.make (level_size*(l+1)) (Char.chr absent);
        width;height;level = l; level_size;}


    let index cache r c l =
      let dl = cache.level lsl 1 in
      let r = r + dl in
      let c = c + dl in
      let loffset = cache.level_size * l in
      loffset + r * cache.width + c

    let get cache r c l =
      Bytes.get_uint8 cache.data (index cache r c l)

    let set cache r c l v =
      Bytes.set_uint8 cache.data (index cache r c l) v
  end
  type image = {
    width : int;
    height : int;
    data : Bytes.t;
    bitmap : bytes;
    applied : int;
    mutable cache : Cache.t;
  }
  let out_of_bound img r c =
    r < 0 || c < 0 || r >= img.height || c >= img.width

  let get img r c =
    let rec get_aux n r c =
      if n = 0 then
        if out_of_bound img r c then 0
        else
          Bytes.get_uint8 img.data (r*img.width + c)
      else
        let b = Cache.get img.cache r c n in
        if b != Cache.absent then b else begin
          let idx = ref 0 in
          for i = -1 to 1 do
            for j = -1 to 1 do
              idx := (!idx lsl 1) lor (get_aux (n-1) (r+i) (c+j) land 1);
            done;
          done;
          let b = Bytes.get_uint8 img.bitmap !idx in
          Cache.set img.cache r c n b; b
        end
    in
    get_aux img.applied r c


  let image_from_strings s l =
    let bitmap =
      Bytes.init (String.length s)
        (fun i ->if s.[i] = '#' then '\x01' else '\x00')
    in
    let arr = Array.of_list l in
    let height = Array.length arr in
    let width = String.length arr.(0) in
    let data = Bytes.make (height * width) '\x00' in
    let idx = ref 0 in
    for i = 0 to height - 1 do
      let line = arr.(i) in
      for j = 0 to width - 1 do
        if line.[j] = '#' then
          Bytes.set_uint8 data !idx 1;
        incr idx;
      done;
    done;
    {data; width; height;bitmap;applied=0;cache = Cache.dummy}

  let enhance img n =  { img with applied = img.applied + n}


  let pp_img r0 c0 r1 c1 fmt img =
    for r = r0 to r1 do
      for c = c0 to c1 do
        Ansi.fprintf fmt "%c" (if get img r c = 1 then '#' else '.')
      done;
      Ansi.fprintf fmt "@\n";
    done

  let count_lit_pixels img =
    let n = img.applied in
    let cache = Cache.create img.width img.height n in
    img.cache <- cache;
    let total = ref 0 in
    for r = -n to img.height + n - 1 do
      for c = -n to img.width + n - 1 do
        let b = get img r c in
        assert (b <=1);
        total := !total + b;
      done;
    done;
    !total

  let load_input () =
    let bitmap = read_line () in
    let _ = read_line () in
    let lines = Input.fold_lines (fun acc l -> l::acc) []
                |> List.rev
    in
    image_from_strings bitmap lines



  let name = Name.mk "s20"

  let solve applied =
    let img = { (load_input ()) with applied  } in
    let n, t = Time.time count_lit_pixels img in
    Ansi.printf "%d (%fms)@\n" n t

  let solve_part1 () = solve 2

  let solve_part2 () = solve 50
end

let () = Solution.register_mod (module S)