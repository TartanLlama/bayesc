module Ast :
  sig
    type pos = int
    type symbol = string
    type program = Bayesian of id * expr * pos
    and id = Id of symbol * pos
    and expr =
        FloatExp of float * pos
      | BoolExp of bool * pos
      | IdExp of id * pos
      | IfElseExp of expr * expr * expr * pos
      | LetExp of id * expr * pos
      | FunDefExp of id * expr * expr * pos
      | SampleExp of dist * pos
      | ObserveExp of expr * pos
      | FunCallExp of id * expr * pos
      | OpExp of expr * op * expr * pos
      | NotExp of expr * pos
      | Seq of expr * expr * pos
      | CallArgs of expr * expr * pos
      | Args of id * expr * pos
      | Nil
    and dist = Bernoulli of expr * pos
    and op =
        PlusOp
      | MinusOp
      | MultOp
      | DivOp
      | OrOp
      | AndOp
      | LtOp
      | GtOp
      | EqOp
  end
val dumpprogram : Ast.program -> unit
val dumpid : Ast.id -> string
val dumpexp : Ast.expr -> string
val dumpdist : Ast.dist -> string
val dumpop : Ast.op -> string
