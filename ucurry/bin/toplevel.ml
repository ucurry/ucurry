module Se = Semant

type action = Ast | CAST | LAST | Default | LLVMIR | TRY

let () =
  let action = ref Default in
  let set_action a () = action := a in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-l", Arg.Unit (set_action LAST), "Print the LAST");
      ("-c", Arg.Unit (set_action CAST), "Print the CAST");
      ("-s", Arg.Unit (set_action LLVMIR), "Print the LLVM");
      ("-t", Arg.Unit (set_action TRY), "Print the LLVM from SAST");
    ]
  in
  let usage_msg = "usage: ./ucurry [-a|-s|-l|-c] [file.uc]\n" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  let sast, _ = Semant.semant_check ast in
  (* let sast, _ = Semantic.typecheck ast in *)
  match !action with
  | Ast ->
      let _ = print_string (Ast.string_of_program ast) in
      print_newline ()
  | LAST ->
      let _ = List.map Lazyconvert.lazyDef sast in
      print_newline ()
  | CAST ->
      (* let last = List.map Lazyconvert.lazyDef sast in *)
      let cast = Clconvert.closeProgram sast in
      let renamed_cast = Rename.rename cast in
      let _ = print_string (Cast.string_of_program renamed_cast) in
      print_newline ()
  | LLVMIR ->
      (* let last = List.map Lazyconvert.lazyDef sast in *)
      let cast = Clconvert.closeProgram sast in
      let llvmir = Codegen.build_main_body cast in
      let _ = print_string (Llvm.string_of_llmodule llvmir) in
      print_newline ()
  | TRY ->
      (* let llvmir = CodegenFromSast.build_main_body sast in *)
      (* let _ = print_string (Llvm.string_of_llmodule llvmir) in *)
      print_newline ()
  | _ -> print_string usage_msg
