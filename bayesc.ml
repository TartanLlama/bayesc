let dumpAst = ref false;;
let dumpSimple = ref false;;
let printBytecode = ref false;;
let file = ref "";;

let main () =
  begin
    Arg.parse [("--dump-ast", Arg.Set dumpAst,
                "Dump the abstract syntax tree of the program");
           ("--dump-simple", Arg.Set dumpSimple,
            "Dump the simplified tree for the program");
           ("--print-bytecode", Arg.Set printBytecode,
            "Print out the bytecode which represents the program")]

      (fun x -> file := x)
      "Compiles a bayesian program";
    let cin =
      if !file != ""
      then open_in !file
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    let ast = BayesParse.main BayesLex.bayes_lang lexbuf in
    if !dumpAst then Ast.dumpprogram ast;
    let (nVars, tree) = BayesSimplify.transformp ast in
    if !dumpSimple then Tree.dumptree tree;
    let bytecode = BayesEmit.compile nVars tree in
    if !printBytecode then Printf.printf "%s\n" bytecode;
    let f = open_out "out.j" in
    begin
      close_in cin;
      output_string f bytecode;
      close_out f;
      Unix.system "java -jar jasmin.jar out.j";
      (*If you want to read the bytecode, comment out this line*)
      Sys.remove "out.j"
    end
  end

let _ = Printexc.print main ()
