(* module Se = Semant *)

type action = Ast | CAST | LAST | Default | LLVMIR

let () =
  let action = ref Default in
  let set_action a () = action := a in
  let speclist =
    [
      ("-a", Arg.Unit (set_action Ast), "Print the AST");
      ("-l", Arg.Unit (set_action LAST), "Print the LAST");
      ("-c", Arg.Unit (set_action CAST), "Print the CAST");
      ("-s", Arg.Unit (set_action LLVMIR), "Print the LLVM");
    ]
  in
  let usage_msg = "usage: ./ucurry [-a|-s|-l|-c] [file.uc]\n" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  let curried = Curry.curry ast in
  let last = Lazy.lazy_convert curried in
  let sast, _ = Semant.semant_check last in
  (* commented out path for lazy  *)
  match !action with
  | Ast ->
      let _ = print_string (Ast.string_of_program curried) in
      print_newline ()
  (* | LAST ->
      let _ = print_string (Ast.string_of_program last) in
      print_newline () *)
  (* | CAST ->
      let cast = Clconvert.close_program sast in
      let _ = print_string (Cast.string_of_program cast) in
      print_newline () *)
  | LLVMIR ->
      let cast = Clconvert.close_program sast in
      let llvmir = Codegen.build_main_body cast in
      let _ = print_string (Llvm.string_of_llmodule llvmir) in
      print_newline ()
  | _ -> print_string usage_msg
