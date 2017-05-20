open Tree.Tree
open Ast.Ast
module T = Tree.Tree
module A = Ast.Ast

exception Invalid_Operator;;
exception Invalid_Test;;
exception This_Should_Not_Happen;;

let definingFunction = ref false;;

let genLabel i =
  string_of_int i;;

let incrementVarCount =
  T.Store
    (T.Field("varCount","I"),
     T.Binop
       (T.IPlus, T.Field("varCount","I"), T.IConst(1)))

let rec transformp p =
  match p with
      A.Bayesian (i,es,p) ->
        match transforme 0 0 es with
          | (cl,nv,te) -> (nv, T.Class (transformid i, te))

and transformid id =
  match id with
      A.Id (s,p) -> s

and transforme cl nv e =
  match e with
    | A.FloatExp (f,p) -> (cl,nv, T.FConst (f))

    (*Bools represented as floats internally*)
    | A.BoolExp (b,p) -> (cl,nv, T.FConst (if b then 1. else 0.))

    | A.IdExp (s,p)
      -> (cl,nv, T.Name (transformid s))

    | A.OpExp (l,A.PlusOp,r,p)
      -> opExpHelper cl nv l r T.Plus

    | A.OpExp (l,A.MinusOp,r,p)
      -> opExpHelper cl nv l r T.Minus

    | A.OpExp (l,A.MultOp,r,p)
      -> opExpHelper cl nv l r T.Mul

    | A.OpExp (l,A.DivOp,r,p)
      -> opExpHelper cl nv l r T.Div

    | A.OpExp (l,A.LtOp,r,p)
      -> simulateRelop cl nv T.Lt l r

    | A.OpExp (l,A.GtOp,r,p)
      -> simulateRelop cl nv T.Gt l r

    | A.OpExp (l,A.EqOp,r,p)
      -> simulateRelop cl nv T.Eq l r

    | A.OpExp (l,A.AndOp,r,p)
      -> simulateLogAnd cl nv l r

    | A.OpExp (l,A.OrOp,r,p)
      -> simulateLogOr cl nv l r

    | A.LetExp (i,e,p) ->
      (match (transforme cl (nv+1) e) with
        | (cl,nv,e) ->
          (cl, nv,
           T.Seq(incrementVarCount,
                 T.Store (T.Name(transformid i),
                    e))))

    | A.IfElseExp (A.OpExp (l,A.AndOp,r,pr),t,f,pe)
      -> logIfElse cl nv l simulateLogAnd r t f

    | A.IfElseExp (A.OpExp (l,A.OrOp,r,pr),t,f,pe)
      -> logIfElse cl nv l simulateLogOr r t f

    | A.IfElseExp (A.OpExp (l,o,r,pr),t,f,pe)
      -> let (lt,ld) = (genLabel (cl+1), genLabel (cl+2)) in
        (*This string of matches ensures that forms are processed in
          the correct order*)
        (match (transforme (cl+2) nv l) with
          | (cl,nv,l) -> match (transforme cl nv r) with
              | (cl,nv,r) -> match (transforme cl nv t) with
                  | (cl,nv,t) -> match (transforme cl nv f) with
                      | (cl,nv,f) ->
                        (cl,nv,
                         T.Seq
                           (T.Seq
                              (T.Seq
                                 (T.Seq
                                    (T.Seq
                                       (T.Cjump ((map_relop o), l, r, lt),
                                        f),
                                     T.Jump (ld)),
                                  T.Label(lt)),
                               t),
                            T.Label(ld))))

    (*"if <id> ..." is syntactic sugar for "if <id> = 1.0 ..."*)
    | A.IfElseExp (A.IdExp(i,pi),t,f,pe)
      -> transforme cl nv
      (A.IfElseExp
         (A.OpExp
            (A.IdExp(i,pi), A.EqOp, A.FloatExp(1.0,-1), pi) ,t,f,pe))

    (*Other exps don't make sense here*)
    | A.IfElseExp (_,t,f,pe) -> raise Invalid_Test

    | A.FunDefExp (i,a,e,p) ->
      definingFunction := true;
      let result = (match (transforme cl nv a) with
        | (cl,nv,a) -> match (transforme cl nv e) with
            | (cl,nv,e) ->
              (cl,nv, T.FunDef ((transformid i), a,
                                T.Seq(e,
                                      T.ArrayElt(T.Field("samples","[F"),
                                                 T.Field("varCount","I")))))) in
      definingFunction := false;
      result

    | A.FunCallExp (i,a,p) ->
      (match (transforme cl (nv+1) a) with
        | (cl,nv,a) -> (cl,nv,
                       T.Seq
                         (incrementVarCount,
                          T.Call (transformid i, a))))

    | A.SampleExp (di,p)
      -> let (t,d) = (genLabel (cl+1), genLabel (cl+2)) in
        let sampleIndex = if !definingFunction
          then T.Field("varCount","I")
          else T.IConst(nv - 1) in
        (match transformd (cl+2) nv di with
          | (cl,nv,s) ->
            (cl, nv,
                 (*Store the new probability*)
             T.Store
               (T.Field("probability", "F"),
                    (*Multiply the probability by the probability of this step*)
                T.Binop (T.Mul, T.Field("probability", "F"),
                         T.Seq
                           (T.Seq
                              (T.Seq
                                 (T.Seq
                                    (T.Seq
                                           (*Check if this variable is false*)
                                       (T.Cjump (T.Eq,
                                                 T.ArrayElt
                                                   (T.Field ("samples", "[F"),
                                                    sampleIndex),
                                                 T.FConst(0.0), t),
                                            (*If true, probability is as stated*)
                                        s),
                                     T.Jump(d)),
                                  T.Label(t)),
                                   (*If false, probability is 1-p*)
                               T.Binop (T.Minus, T.FConst(1.0), s)),
                            T.Label(d))))))

    | A.ObserveExp (e,p)
      -> let (t,d) = (genLabel (cl+1), genLabel (cl+2)) in
        (match (transforme (cl+2) nv e) with
          | (cl,nv,e) ->
            (cl,nv,
             T.Seq
               (T.Seq
                      (*If the observation is false,
                        set the valid bit to false (0.0)*)
                  (T.Cjump (T.Eq, e, T.FConst(1.0), t),
                   T.Store (T.Field("valid", "F"), T.FConst(0.0))),
                T.Label(t))))

    | A.NotExp (e,p)
      -> let (t,d) = (genLabel (cl+1), genLabel (cl+2)) in
        (match (transforme (cl+2) nv e) with
          | (cl,nv,e) ->
            (cl,nv,
             T.Seq
               (T.Seq
                  (T.Seq
                     (T.Seq
                        (T.Seq
                               (*If true, then false;
                                 if false, then true*)
                           (T.Cjump (T.Ne, e, T.FConst(0.0), t),
                            T.FConst (1.)),
                         T.Jump (d)),
                      T.Label (t)),
                   T.FConst (0.0)),
                T.Label (d))))

    | A.Seq (e,es,p) ->
      (match transforme cl nv e with
        | (cl,nv,e) -> match transforme cl nv es with
            | (cl,nv,es) ->
              (cl,nv,
               T.Seq (e, es)))

    | A.CallArgs (e,es,p) ->
      (match transforme cl nv e with
        | (cl,nv,e) -> match transforme cl nv es with
            | (cl,nv,es) ->
              (cl,nv,
               T.Seq (e, es)))

    | A.Args (l,A.Nil,p) -> (cl,nv, T.Name (transformid l))

    | A.Args (l,es,p) ->
      (match transforme cl nv es with
        | (cl,nv,es) -> (cl,nv,T.Seq(T.Name(transformid l),es)))

        (*Shouldn't appear outside of args*)
    | A.Nil -> raise This_Should_Not_Happen

and transformd cl nv d =
  match d with
      A.Bernoulli (e,p) -> transforme cl nv e

and map_binop op =
  match op with
    | A.PlusOp -> T.Plus
    | A.MinusOp -> T.Minus
    | A.MultOp -> T.Mul
    | A.DivOp -> T.Div
    | _ -> raise Invalid_Operator


and map_relop op =
  match op with
    | A.LtOp -> T.Lt
    | A.GtOp -> T.Gt
    | A.EqOp -> T.Eq
    | _ -> raise Invalid_Operator

and map_logop op =
  match op with
    | A.AndOp -> T.LAnd
    | A.OrOp -> T.LOr
    | _ -> raise Invalid_Operator

and simulateRelop cl nv op l r =
  let (t,d) = (genLabel (cl+1), genLabel (cl+2)) in
  match (transforme (cl+2) nv r) with
    | (cl,nv,r) -> match (transforme cl nv l) with
        | (cl,nv,l) ->
          (cl,nv,
           T.Seq
             (T.Seq
                (T.Seq
                   (T.Seq
                      (T.Seq
                         (T.Cjump (op, l, r, t),
                          T.FConst (0.0)),
                       T.Jump (d)),
                    T.Label(t)),
                 T.FConst(1.)),
              T.Label(d)))

(*Doesn't short-circuit so that all parameterised variables are executed*)
and simulateLogAnd cl nv l r =
  match (transforme (cl+2) nv l) with
    | (cl,nv,l) -> match (transforme cl nv r) with
        | (cl,nv,r) ->
          (cl,nv, T.Binop (T.Mul, l, r))


and simulateLogOr cl nv l r =
  let (t,d) = (genLabel (cl+1), genLabel (cl+2)) in
  match (transforme (cl+2) nv l) with
    | (cl,nv,l) -> match (transforme cl nv r) with
        | (cl,nv,r) ->
          (cl,nv,
           T.Seq
             (T.Seq
                (T.Seq
                   (T.Seq
                      (T.Seq
                         (*If either is true, then true, else false*)
                         (T.Cjump (T.Ge, T.Binop (T.Plus,r,l), T.FConst(1.0), t),
                          T.FConst (0.0)),
                       T.Jump (d)),
                    T.Label(t)),
                 T.FConst(1.0)),
              T.Label(d)))

and logIfElse cl nv l opfun r t f =
  let (lt,ld) = (genLabel (cl+1), genLabel (cl+2)) in
          (*Simplify the logical operation first*)
  (match opfun (cl+2) nv l r with
    | (cl,nv,l) -> match (transforme cl nv t) with
        | (cl,nv,t) -> match (transforme cl nv f) with
            | (cl,nv,f) ->
              (cl,nv,
               T.Seq
                 (T.Seq
                    (T.Seq
                       (T.Seq
                          (T.Seq
                             (T.Cjump (T.Ne, l, T.FConst(0.0), lt),
                              f),
                           T.Jump (ld)),
                        T.Label(lt)),
                     t),
                  T.Label(ld))))

and opExpHelper cl nv l r op =
  match (transforme cl nv r) with
    | (cl,nv,r) -> match (transforme cl nv l) with
        | (cl,nv,l) -> (cl,nv,T.Binop (op, r, l))
