module Ast =
struct
  type pos = int;;
  type symbol = string;;

  (*Corresponds to the <bayes> rule*)
  type program = Bayesian of id * expr * pos (*Represents the entire program*)

  (*A helper which doesn't appear in the concrete syntax*)
  and id = Id of symbol * pos (*A variable or function identifier*)

  (*Correponds to the <expr> rule*)
  and expr =
      (*The <number> rule*)
      FloatExp of float * pos (*No integers, only floats, for simplicity*)

    (*<boolean>*)
    | BoolExp of bool * pos (*A boolean*)

    (*for saying what constitutes a "true" run*)
    (*<id>*)
    | IdExp of id * pos (*An id on its own*)

    (*An else clause is needed*)
    (*Corresponds to conditional rule*)
    (*Note that "expr" can be a sequence*)
    | IfElseExp of expr * expr * expr * pos (*Conditional statement*)

    (*Local definition rule for variables*)
    | LetExp of id * expr * pos

    (*Local definition rule for functions*)
    | FunDefExp of id * expr * expr * pos

    (*Statistical sample rule*)
    | SampleExp of dist * pos

    (*Observation rule*)
    | ObserveExp of expr * pos

    (*Function call rule*)
    | FunCallExp of id * expr * pos

    (*Binary operator rule*)
    | OpExp of expr * op * expr * pos

    (*Negation rule*)
    | NotExp of expr * pos

    (*For representing "exprs"*)
    (*This is not a separate type for sake of simplicity*)
    (*Whether or not a seq is valid is dealt with in the parser*)
    | Seq of expr * expr * pos

    (*For arguments when calling a function*)
    | CallArgs of expr * expr * pos

    (*For arguments when defining a function*)
    | Args of id * expr * pos

    (*For ending arg lists*)
    | Nil

  (*The <distribution> rule*)
  and dist = Bernoulli of expr * pos

  (*The <op> rule*)
  and op = PlusOp | MinusOp | MultOp | DivOp
           | OrOp | AndOp
           | LtOp | GtOp | EqOp
  ;;
end

(*These functions are for printing an ast*)

let rec dumpprogram p =
  match p with
    | Ast.Bayesian (i,e,p) ->
      Printf.printf "(Bayesian %s %s)\n" (dumpid i) (dumpexp e)

and dumpid e =
  match e with
    | Ast.Id (s,p) -> Printf.sprintf "(Id %s)" s

and dumpexp e =
  match e with
    | Ast.FloatExp (f,p) -> Printf.sprintf "(FloatExp %f)" f
    | Ast.BoolExp (b,p) -> Printf.sprintf "(BoolExp %B)" b
    | Ast.IdExp (i,p) -> Printf.sprintf "(IdExp %s)" (dumpid i)
    | Ast.IfElseExp (t,i,e,p) -> Printf.sprintf "(IfElseExp %s %s %s)"
      (dumpexp t) (dumpexp i) (dumpexp e)
    | Ast.LetExp (i,e,p) -> Printf.sprintf "(LetExp %s %s)"
      (dumpid i) (dumpexp e)
    | Ast.FunDefExp (i,a,e,p) -> Printf.sprintf "(FunDefExp %s %s %s)"
      (dumpid i) (dumpexp a) (dumpexp e)
    | Ast.SampleExp (d,p) -> Printf.sprintf "(SampleExp %s)" (dumpdist d)
    | Ast.ObserveExp (e,p) -> Printf.sprintf "(ObserveExp %s)" (dumpexp e)
    | Ast.FunCallExp (i,c,p) -> Printf.sprintf "(FunCallExp %s %s)"
      (dumpid i) (dumpexp c)
    | Ast.OpExp (l,o,r,p) -> Printf.sprintf "(OpExp %s %s %s)"
      (dumpexp l) (dumpop o) (dumpexp r)
    | Ast.NotExp (e,p) -> Printf.sprintf "(NotExp %s)" (dumpexp e)
    | Ast.Seq (e,es,p) -> Printf.sprintf "(Seq %s %s)" (dumpexp e) (dumpexp es)
    | Ast.CallArgs (i,a,p) -> Printf.sprintf "(CallArgs %s %s)"
      (dumpexp i) (dumpexp a)
    | Ast.Args (i,a,p) -> Printf.sprintf "(Args %s %s)" (dumpid i) (dumpexp a)
    | Ast.Nil -> "Nil"

and dumpdist e =
  match e with
    | Ast.Bernoulli (e,p) -> Printf.sprintf "(Bernoulli %s)" (dumpexp e)

and dumpop o =
  match o with
    | Ast.PlusOp -> "PlusOp"
    | Ast.MinusOp -> "MinusOp"
    | Ast.MultOp -> "MultOp"
    | Ast.DivOp -> "DivOp"
    | Ast.LtOp -> "LtOp"
    | Ast.GtOp -> "GtOp"
    | Ast.EqOp -> "EqOp"
    | Ast.OrOp -> "OrOp"
    | Ast.AndOp -> "AndOp"
