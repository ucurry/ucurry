type action = Ast | CAST | LAST | Default | LLVMIR

let () =
  let action = ref Default in
  let set_action a () = action := a in
  let speclist = [ 
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-c", Arg.Unit (set_action CAST), "Print the CAST");
    ("-l", Arg.Unit (set_action LAST), "Print the LAST");
    ("-s", Arg.Unit (set_action LLVMIR), "Print the LAST");
  ] in
  let usage_msg = "usage: ./ucurry [-a|-s|-l|-c] [file.uc]\n" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  let _ = Semantic.typecheck ast in 
  match !action with
  | Ast ->
      let _ = print_string (Ast.string_of_program ast) in
      print_newline ()
  | LAST ->
      let last = List.map Lazyconvert.lazyDef ast in
    print_newline ()
  | CAST ->
      let last = List.map Lazyconvert.lazyDef ast in
      let cast = Clconvert.closeProgram last in
      let renamed_cast = Rename.rename cast in
      let _ = print_string (Cast.string_of_program renamed_cast) in
      print_newline ()
  | LLVMIR -> 
    print_newline ()
    
      (* let last = List.map Lazyconvert.lazyDef ast in
      let cast = Clconvert.closeProgram last in
      let renamed_cast = Rename.rename cast in
      let llvmir = Llvmconvert.llvmProgram renamed_cast in
      let _ = print_string (Llvm.string_of_llmodule llvmir) in *)
    
  | _ -> print_string usage_msg
