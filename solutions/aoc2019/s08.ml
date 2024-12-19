open Utils
open Syntax
module S =
struct
  let name = Name.mk "s08"
  let _H = 6
  let _W = 25

  let read_input () =
    let line = read_line () in
    let num_layers = String.length line / (_W * _H) in
    let i = ref 0 in
    let layers = Array.make num_layers [| |] in
    for l = 0 to num_layers - 1 do
      let tab = Array.init _H (fun _ -> Bytes.make _W ' ') in
      layers.(l) <- tab;
      for y = 0 to _H - 1 do
        for x = 0 to _W - 1 do
          tab.(y).$[x] <- line.[!i];
          i := !i + 1
        done;
      done;
    done;
    layers

  let count_12 layers =
    let num0 = ref max_int in
    let prod12 = ref 0 in
    Array.iter (fun layer ->
        let n0, n1, n2 =
          Array.fold_left (fun acc line ->
              Bytes.fold_left (fun (n0, n1, n2) c ->
                  if c = '0' then (n0+1, n1, n2)
                  else if c = '1' then (n0, n1+1, n2)
                  else (n0, n1, n2 + 1)
                ) acc line
            ) (0,0,0) layer
        in
        if n0 < !num0 then begin 
          num0 := n0;
          prod12 := n1 * n2;
        end
      ) layers;
    !prod12

  let flatten layers =
    let num_layers = Array.length layers in
    let base = layers.(num_layers - 1) in
    for l = num_layers - 2 downto 0 do
      for y = 0 to _H - 1 do
        for x = 0 to _W -1 do
          let c = layers.(l).(y).$[x] in
          if c <> '2' then
            base.(y).$[x] <- c;
        done;
      done;
    done;
    Array.iter (fun b ->
        Bytes.iter (fun c ->
            if c = '0' then Ansi.printf " "
            else Ansi.(printf "%a %a" bg white clear color)
          ) b;
        Ansi.printf "\n%!"
      ) base


  let solve_part1 () =
    let layers = read_input () in
    let n = count_12 layers in
    Ansi.(printf "%a%d%a\n" fg green n clear color)
  let solve_part2 () =
    let layers = read_input () in
    flatten layers
end

let () = Solution.register_mod (module S)