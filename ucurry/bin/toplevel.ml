type action = Ast | PAST | CAST | LAST | Default | LAZY | BARE

let () =
  let action = ref Default in
  let set_action a () = action := a in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-p", Arg.Unit (set_action PAST), "Print the PAST");
      ("-l", Arg.Unit (set_action LAST), "Print the LAST");
      ("-c", Arg.Unit (set_action CAST), "Print the CAST");
      ("-s", Arg.Unit (set_action LAZY), "Compile with all features");
      ("-b", Arg.Unit (set_action BARE), "Compile without lazy");
    ]
  in
  let usage_msg = "usage: ./ucurry [-a|-s|-l|-c] [file.uc]\n" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  (* commented out path for lazy  *)
  match !action with
  | Ast ->
      let lexbuf = Lexing.from_channel !channel in
      let ast = Parser.program Scanner.token lexbuf in
      let _ = Desugar.desugar ast in
      let _ = print_string (Ast.string_of_program ast) in
      print_newline ()
  | PAST ->
      let lexbuf = Lexing.from_channel !channel in
      let ast = Parser.program Scanner.token lexbuf in
      let desugared = Desugar.desugar ast in
      let _ = print_string (Ast.string_of_program desugared) in
      print_newline ()
  | LAST ->
      let lexbuf = Lexing.from_channel !channel in
      let ast = Parser.program Scanner.token lexbuf in
      let curried = Curry.curry ast in
      let desugared = Desugar.desugar curried in
      let lazied = Lazy.lazy_convert desugared in
      let _ = Semant.semant_check lazied in
      let _ = print_string (Ast.string_of_program lazied) in
      print_newline ()
  | CAST ->
      let lexbuf = Lexing.from_channel !channel in
      let ast = Parser.program Scanner.token lexbuf in
      let desugared = Desugar.desugar ast in
      let sast, _ = Semant.semant_check desugared in
      let cast = Clconvert.close_program sast in
      let _ = print_string (Cast.string_of_program cast) in
      print_newline ()
  | LAZY ->
      let lexbuf = Lexing.from_channel !channel in
      let ast = Parser.program Scanner.token lexbuf in
      let curried = Curry.curry ast in
      let desugared = Desugar.desugar curried in
      let lazied = Lazy.lazy_convert desugared in
      let sast, _ = Semant.semant_check lazied in
      let cast = Clconvert.close_program sast in
      let llvmir = Codegen.build_main_body cast in
      let _ = print_string (Llvm.string_of_llmodule llvmir) in
      print_newline ()
  | BARE ->
      let lexbuf = Lexing.from_channel !channel in
      let ast = Parser.program Scanner.token lexbuf in
      let curried = Curry.curry ast in
      let desugared = Desugar.desugar curried in
      let sast, _ = Semant.semant_check desugared in
      let cast = Clconvert.close_program sast in
      let llvmir = Codegen.build_main_body cast in
      let _ = print_string (Llvm.string_of_llmodule llvmir) in
      print_newline ()
  | Default -> print_newline ()
(* | _ -> print_newline () *)
