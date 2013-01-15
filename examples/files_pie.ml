module A = Archimedes
module PC = A.Piechart

let rec dig directory =
  let all = Array.to_list (Sys.readdir directory) in
  (* Privacy ... please :p *)
  let all = List.filter (fun x -> String.get x 0 <> '.') all in
  let all = List.map (fun x -> directory ^ "/" ^ x) all in
  let directories, files = List.partition Sys.is_directory all in
  let directories = List.map dig directories in
  let count_files = float (List.length files) in
  let total = List.fold_left (fun acc x -> acc +. x.PC.value) count_files directories in
  { PC.name = directory; PC.value = total; PC.children = directories }

let () =
  let data = dig "." in
  let vp = A.init(try A.backend_of_filename Sys.argv.(1) with _ -> []) in
  PC.multilevel vp data.PC.children;
  A.close vp
