open Tree.Tree
open ExtList
module T = Tree.Tree

exception Invalid_Lvalue
exception Only_Use_This_Function_For_Args

let className = ref "";;
let numberOfVariables = ref 0;;
let varCount = ref 0;;

(*From http://rosettacode.org/wiki/Repeat_a_string*)
let string_repeat s n =
  let len = String.length s in
  let res = String.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len;
  done;
  (res)
;;

let rec getNumArgs' count args =
  match args with
    | T.Name(n) -> 1 + count
    | T.Seq (n, es) -> getNumArgs' (count+1) es
    | _ -> raise Only_Use_This_Function_For_Args

let getNumArgs args = getNumArgs' 0 args

let rec getArgs' fv count args =
  match args with
    | T.Name(n) -> (n,count) :: fv
    | T.Seq (T.Name(n), es) -> (n,count) :: (getArgs' fv (count+1) es)
    | _ -> raise Only_Use_This_Function_For_Args

(*Gets an associative list of the variable names to stack numbers*)
let getArgs fv args =
  getArgs' fv 1 args

(*Prints out function headers*)
let rec printHeaders h =
  match h with
    | [] -> ()
    | (l,s) :: hs -> Printf.printf "%s -> %s\n" l s; printHeaders hs

let rec printVars v =
  match v with
    | [] -> ()
    | (l,s) :: hs -> Printf.printf "%s -> %d" l s; printVars hs

let mapRelOp o =
  match o with
    | T.Eq -> "ifeq"
    | T.Ne -> "ifne"
    | T.Lt -> "iflt"
    | T.Gt -> "ifgt"
    | T.Le -> "ifle"
    | T.Ge -> "ifge"

let rec emitHelper' emits nv nfv nh its ifn =
  match emits with
    | [] -> (nv, nfv, nh, its, ifn)
    | e :: es -> match emitTree nv nfv nh its ifn e with
        | (v,fv,h,ts,fn) ->
          (*Emit the next tree in the new context*)
          emitHelper' es v fv h ts fn

(*Emit all prerequisites, then prepend the new stuff*)
and emitHelper v fv h ts fn emits nts nfn =
  match emitHelper' emits v fv h ts fn with
    | (fv,ffv,fh,fts,ffn) -> (fv,ffv,fh,(List.rev nts)@fts,nfn@ffn)

(*v is global variable mappings to sample array indicies
  fv is function arg mappings to stack indicies
  h is function name mappings to signatures
  ts is the all the bytecode currently emitted apart from functions
  fn is all the bytecode emitted for functions

  Recursively builds a back-to-front list of bytecodes*)
and emitTree v fv h ts fn t =
  match t with
    | T.Seq (e,es) ->
      (*Emit e, then es*)
      emitHelper v fv h ts fn [e; es] [] []

    (*Label syntax is <label>:*)
    | T.Label (l) -> (v, fv, h, ((makeLabel l) ^ ":") :: ts, fn)

    (*Jump maps to goto*)
    | T.Jump (l) -> (v, fv, h, ("goto " ^ (makeLabel l)) :: ts, fn)

    | T.Cjump (o,l,r,lt) ->
      emitHelper v fv h ts fn [l;r]
        (*Do a float comparison before the if instruction*)
        ["fcmpl"; (mapRelOp o) ^ " " ^ (makeLabel lt)] []

    | T.Store (T.Name(n),e) ->
      (*Just increment the variable count and carry on*)
      begin
        incr varCount;
        emitTree ((n,!varCount-1)::v) fv h ts fn e
      end

    | T.Store (T.Field(l,t),e) ->
      (*Load "this" first*)
      emitHelper v fv h ("aload_0" :: ts) fn [e]
        (*Put the top of the stack into that field*)
        ["putfield runtime/BayesProgram/" ^ l ^ " " ^ t] []

    (*Can't store into anything else*)
    | T.Store (_,e) -> raise Invalid_Lvalue

    | T.Binop (T.Plus,e1,e2) ->
      emitHelper v fv h ts fn [e1; e2] ["fadd"] []
    | T.Binop (T.IPlus,e1,e2) ->
      emitHelper v fv h ts fn [e1; e2] ["iadd"] []
    | T.Binop (T.Minus,e1,e2) ->
      emitHelper v fv h ts fn [e1; e2] ["fsub"] []
    | T.Binop (T.Mul,e1,e2) ->
      emitHelper v fv h ts fn [e1; e2] ["fmul"] []
    | T.Binop (T.Div,e1,e2) ->
      emitHelper v fv h ts fn [e1; e2] ["fdiv"] []

    | T.Name (l) ->
      (*Look it up in global variables first*)
      (try
        let varNum = List.assoc l v in
        emitTree v fv h ts fn
          (*If it's a global variable, retrieve whether it is true or false*)
          (T.ArrayElt(T.Field("samples","[F"), (T.IConst(varNum))))
      with Not_found ->
        (*It might be a function arg, try to load it*)
        (v, fv, h, ("fload_" ^ (string_of_int (List.assoc l fv))) :: ts, fn))

    (*Use the optimised instructions*)
    | T.IConst (0) -> (v, fv, h, "iconst_0" :: ts, fn)
    | T.IConst (1) -> (v, fv, h, "iconst_1" :: ts, fn)
    | T.IConst (2) -> (v, fv, h, "iconst_2" :: ts, fn)
    | T.IConst (3) -> (v, fv, h, "iconst_3" :: ts, fn)
    | T.IConst (4) -> (v, fv, h, "iconst_4" :: ts, fn)
    | T.IConst (5) -> (v, fv, h, "iconst_5" :: ts, fn)

    (*Otherwise, just use ldc*)
    | T.IConst (i) -> (v, fv, h, ("ldc " ^ (string_of_int i)) :: ts, fn)

    (*Use the optimised instructions*)
    | T.FConst (0.0) -> (v, fv, h, "fconst_0" :: ts, fn)
    | T.FConst (1.0) -> (v, fv, h, "fconst_1" :: ts, fn)
    | T.FConst (2.0) -> (v, fv, h, "fconst_2" :: ts, fn)

    (*Otherwise, just use ldc*)
    | T.FConst (f) -> (v, fv, h, ("ldc " ^ (string_of_float f)) :: ts, fn)

    | T.Call (l,es) ->
      emitHelper v fv h ("aload_0"::ts) fn
        [es]
        (*Invoke the class by retrieving its signature*)
        ["invokevirtual " ^ !className ^ "/" ^ (List.assoc l h)] []

    | T.Field (l,t) -> (v, fv, h,
                       ("getfield runtime/BayesProgram/" ^ l ^ " " ^ t) ::
                          (*aload first*)
                          "aload_0" ::
                          ts, fn)
    | T.ArrayElt (x,i) ->
      emitHelper v fv h ts fn [x; i] ["faload"] []

    | T.FunDef (l,e,es) ->
      let nArgs = getNumArgs e in
      let fArgs = string_repeat "F" nArgs in
      let signature = l ^ "(" ^ fArgs ^ ")F" in
      let nfv = getArgs fv e in
      let nfn = (".limit stack 100"
                 :: (".limit locals " ^ (string_of_int (nArgs + 1)))
                 :: (".method private " ^ signature)
                 :: fn) in
      (match emitTree v nfv ((l, signature) :: h) nfn fn es with
        | (nv, nfv, nh, nts, nfn)
          -> (nv,
             List.drop nArgs nfv, (*get rid of argument names from storage*)
             nh,
             ts,
             ".end method" :: "freturn" :: nts))

    | T.Class (l,e) ->
      begin
        className := l;
        let preamble = List.rev [".class public " ^ l;
                         ".super runtime/BayesProgram";
                         ".method public <init>()V";
                         ".limit stack 2";
                         "aload_0";
                         "ldc " ^ (string_of_int !numberOfVariables);
                         "invokespecial runtime/BayesProgram/<init>(I)V";
                         "return";
                         ".end method";
                         ".method public static main([Ljava/lang/String;)V";
                         ".limit stack 3";
                         "new runtime/Runtime";
                         "dup";
                         "invokespecial runtime/Runtime/<init>()V";
                         "new " ^ !className;
                         "dup";
                         "invokespecial " ^ !className ^ "/<init>()V";
                         "invokevirtual runtime/Runtime/run(Lruntime/BayesProgram;)V";
                         "return";
                         ".end method";
                         ".method public run()F";
                         ".limit stack 100"; (*dirty hack*)
                         ".limit locals "^(string_of_int(!numberOfVariables+1));
                         "aload_0";
                         "invokevirtual runtime/BayesProgram/initRun()V"] in
        (match emitTree v fv h preamble fn e with
          | (nv,nfv,nh,nts,nfn) ->
            (nv,
             nfv,
             nh,
             ".end method" :: "freturn" :: nts,
             nfn))
      end

and makeLabel l =
  (*Labels in jvm cannot be numbers, so stick #_ onto the front of them*)
  "#_" ^ l

let compile nVars t =
  begin
    numberOfVariables := nVars;
    match (emitTree [] [] [] [] [] t) with
      | (v, fv, h, ts, fn) ->
        (*Reverse both lists whilst adding newlines and creating a string*)
        (List.fold_left (fun x y -> y^"\n"^x) "" ts)
        (*Add the function definitions on to the end*)
        ^ (List.fold_left (fun x y -> y^"\n"^x) "" fn)
  end
