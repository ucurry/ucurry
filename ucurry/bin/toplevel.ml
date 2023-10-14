type action = Ast | Default

let () =
  let action = ref Default in
  let set_action a () = action := a in
  let speclist = [ ("-a", Arg.Unit (set_action Ast), "Print the AST") ] in
  let usage_msg = "usage: ./ucurry [-a|-s|-l|-c] [file.uc]\n" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
  | Ast ->
      let _ = print_string (Ast.string_of_program ast) in
      print_newline ()
  | _ -> print_string usage_msg
