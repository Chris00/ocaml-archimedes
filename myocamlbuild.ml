(* OASIS_START *)
(* OASIS_STOP *)

Ocamlbuild_plugin.dispatch
  (MyOCamlbuildBase.dispatch_combine [
    dispatch_default;
    begin function
    | After_rules ->

      dep ["file:src/plot.ml"] ["src" / "plot_arr.ml"];
      dep ["file:src/path.ml"] ["src" / "path_arr.ml"];

      (* ocamlbuild does not support cmxs before 3.12.0.  Quick hack. *)
      let version_major, version_minor =
        Scanf.sscanf Sys.ocaml_version "%i.%i" (fun mj mi -> mj, mi) in
      if version_major <= 3 && version_minor <= 11 then (
        rule "cmxa -> cmxs" ~dep:"%.cmxa" ~prod:"%.cmxs"
          begin fun env build ->
            let cmxa = env "%.cmxa" and cmxs = env "%.cmxs" in
            Cmd(S [!Options.ocamlopt; A "-shared"; A "-linkall";
                   T (tags_of_pathname cmxs ++ "library" ++ "native" ++ "ocaml"
                      ++ "link" ++ "shared");
                   P cmxa; A "-o"; Px cmxs])
          end;
      )

    | _ -> ()
    end;
  ]);;
