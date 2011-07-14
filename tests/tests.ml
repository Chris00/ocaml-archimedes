let print_help () =
  print_string "usage: tests [option1] [option2] ...\n\n";
  print_string "where an option is a test name or one of the following:\n";
  print_string "--ps\t\tactivate ps (cairo) output (default: false)\n";
  print_string "--pdf\t\tactivate pdf (cairo) output (false)\n";
  print_string "--png\t\tactivate png (cairo) output (false)\n";
  print_string "--tex\t\tactivate tex (tikz) output (false)\n";
  print_string "--graphics\t\tactivate graphics output (true)\n";
  print_string "Available tests are:\n";
  List.iter (fun (name, _) -> Printf.printf "%s  " name) map;
  print_string "\n\nExample: ";
  print_string "tests demo_zoom vector_field --nographics --pdf --png\n"

let backends title =
  let p = Printf.sprintf in
  let ret = Array.fold_left (fun acc -> function
    | "--ps" -> ret := (p "cairo PS %s.ps" title) :: acc
    | "--pdf" -> ret := (p "cairo PDF %s.pdf" title) :: acc
    | "--png" -> ret := (p "cairo PNG %s.png" title) :: acc
    | "--tex" -> ret := (p "tikz %s.tex" title) :: acc
    | "--graphics" -> ret := (p "graphics hold") :: acc) [] Sys.argv
  in
  if ret = [] then [p "graphics hold"] else ret

let map = [
  (* AUTOGEN_TESTS *)
]

let rec exec_test = function
  | "all" -> List.iter exec_test (List.map fst map)
  | name -> try
      let test = List.assoc name map in
      Printf.printf "%s\n" name;
      List.iter test (backends name)
    with Not_found -> Printf.printf "Test not found: %s" name

let () =
  if Array.length Sys.argv = 1 then print_help ()
  else Array.iteri (fun i x ->
                      if i <> 0 && String.sub x 0 2 <> "--"
                      then exec_test x) Sys.argv
