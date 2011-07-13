let rec all bk =
  List.iter (fun (title, test) ->
               print_newline ();
               print_string title;
               print_newline ();
               test bk) (List.tl map)
and map = [
  ("all", all); (* To be skipped by the "all" function just above *)
  (* AUTOGEN_TESTS *)
]

let exec_test backends t =
  match List.filter (fun x -> fst x = t) map with
  | [] -> Printf.printf "Test not found: %s\n" t
  | (_, test) :: [] -> List.iter test backends
  | _ -> Printf.printf "Duplicated test: %s\n" t

let print_help () =
  print_string "--[no]ps\t\t[de]activate ps (cairo) output (default: false)\n";
  print_string "--[no]pdf\t\t[de]activate pdf (cairo) output (false)\n";
  print_string "--[no]png\t\t[de]activate png (cairo) output (false)\n";
  print_string "--[no]tex\t\t[de]activate tex (tikz) output (false)\n";
  print_string "--[no]graphics\t\t[de]activate graphics output (true)\n"

let backends ps pdf png tex graphics title =
  let ret = ref [] in
  if ps then ret := ("cairo PS " ^ title ^ ".ps") :: !ret;
  if pdf then ret := ("cairo PDF " ^ title ^ ".pdf") :: !ret;
  if png then ret := ("cairo PNG " ^ title ^ ".png") :: !ret;
  if tex then ret := ("tikz " ^ title ^ ".tex") :: !ret;
  if graphics then ret := "graphics hold" :: !ret;
  print_string "backends:\n";
  List.iter (fun x -> print_string x; print_newline ()) !ret;
  !ret

let () =
  let ps = ref false
  and pdf = ref false
  and png = ref false
  and tex = ref false
  and graphics = ref true in
  let options x =
    if x = "--ps" then ps := true;
    if x = "--pdf" then pdf := true;
    if x = "--png" then png := true;
    if x = "--tex" then tex := true;
    if x = "--graphics" then graphics := true;
    if x = "--nops" then ps := false;
    if x = "--nopdf" then pdf := false;
    if x = "--nopng" then png := false;
    if x = "--notex" then tex := false;
    if x = "--nographics" then graphics := false;
    if x = "--help" then print_help ()
  in
  Array.iter options Sys.argv;
  let backends = backends !ps !pdf !png !tex !graphics in
  Array.iteri (fun i x ->
                 if i <> 0 && String.sub x 0 2 <> "--"
                 then exec_test (backends x) x) Sys.argv
