(** Update the tests.ml file *)

open Printf

let skip = [
  (* TODO drop those tests or fix them *)
  "arrays.ml";
  "test_layer.ml";
  "cairo_backend.ml";
  "functions.ml";
  "simple_plot.ml";
  "test_handle.ml";
  (* do NOT edit the four following *)
  "tests.ml";
  "realtests.ml";
  "testing.ml";
  "testsmaker.ml"
]

let starting = "  (* BEGIN AUTOGEN_TESTS *)"
let ending = "  (* END AUTOGEN_TESTS *)"

let is_test name =
  List.for_all (( <> ) name) skip &&
    String.length name > 3 &&
    String.sub name (String.length name - 3) 3 = ".ml"

let print_test name =
  let name = String.sub name 0 (String.length name - 3) in
  printf "  (\"%s\", %s.draw);\n" name (String.capitalize name)

let print_tests () =
  let files = Array.to_list (Sys.readdir "tests") in
  let files = List.filter is_test files in
  let files = List.sort compare files in
  List.iter print_test files

let () =
  let ftests = open_in "tests/tests.ml" in
  let copylines = ref true in
  try
    while true do
      let l = input_line ftests in
      if !copylines && l <> starting then
        printf "%s\n" l
      else if !copylines then begin
        printf "%s\n" starting;
        printf "  (* Do NOT edit until END AUTOGEN_TESTS *)\n";
        print_tests ();
        copylines := false
      end
      else if not !copylines && l = ending then begin
        printf "%s\n" ending;
        copylines := true
      end
    done
  with End_of_file ->
    printf "\n";
    close_in ftests
