open Utils

let () =
  Sys.catch_break true;
  Printexc.record_backtrace true;
  let open Format in
  pp_set_margin err_formatter 2000;
  pp_set_max_indent err_formatter 2000;
  pp_set_margin std_formatter 2000;
  pp_set_max_indent std_formatter 2000

let usage ppf s =
  let open Format in
  fprintf ppf "Usage:\n";
  fprintf ppf "%s [prefix] sol [1|2] [variant]: execute a given solution.\n\n"
    s;
  fprintf ppf "%s list: list all registered solutions.\n\n"
    s;
  fprintf ppf "%s all prefix file1, ..., filen: execute all solutions of the input files given on the command line.\n\n"
    s


let main () =
  try
    Fun.protect ~finally:(fun () ->
        Ansi.(printf "%a%a%!"
                clear color show_cursor()))
      (fun () -> Solution.run Sys.argv)
  with
  | Solution.Error msg ->
    Format.eprintf "%s\n" msg;
    usage Format.err_formatter Sys.argv.(0);
    Format.eprintf "\n%!";
    exit 1
  | e ->
    Format.eprintf "%s\n%!" (Printexc.to_string e);
    Printexc.print_backtrace stderr; exit 2

let () = main ()