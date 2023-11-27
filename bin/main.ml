open Utils

let () =
  Sys.catch_break true;
  Printexc.record_backtrace true;
  let open Format in
  pp_set_margin err_formatter 2000;
  pp_set_max_indent err_formatter 2000;
  pp_set_margin std_formatter 2000;
  pp_set_max_indent std_formatter 2000

let usage ppf =
  Format.fprintf ppf "Usage: %s [prefix] sol [1|2] [variant]"

let main () =
  try
    let (), t = Utils.Time.time Solution.run Sys.argv in
    Utils.Ansi.(printf "%a%f ms%a\n" bfg yellow t clear color)
  with
    Failure msg ->
    Format.eprintf "%s\n" msg;
    usage Format.err_formatter Sys.argv.(0);
    Format.eprintf "\n%!";
    exit 1


let () = main ()