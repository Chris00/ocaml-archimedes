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
  "test_boxes.ml";
  "test_custom_labels.ml";
  "test_stack.ml";
  (* do NOT edit the four following *)
  "tests.ml";
  "realtests.ml";
  "testing.ml";
  "testsmaker.ml"
]

let tag = "  (* AUTOGEN_TESTS *)"

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
      if l = tag then
        print_tests ()
      else
        printf "%s\n" l
    done
  with End_of_file ->
    printf "\n";
    close_in ftests
