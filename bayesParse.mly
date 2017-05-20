%{
  open Ast.Ast;;
  module A = Ast.Ast;;
%}

%token <float> FLOAT
%token <string> ID
%token <bool> BOOLEAN
%token PLUS MINUS ASTR SLASH
%token LPAR RPAR
%token BAYESIAN WHERE LET
%token SAMPLE OBSERVE BERNOULLI
%token IF THEN ELSE
%token AMP BAR COMMA
%token LT GT EQ NOT
%token EOF
%left SEQ
%nonassoc SAMPLE
%nonassoc BERNOULLI
%left BAR
%left AMP
%left LT GT EQ
%nonassoc NOT
%left PLUS MINUS
%left ASTR SLASH
%start main
%type <Ast.Ast.program> main
%%

main:
  BAYESIAN id WHERE exprs EOF         { A.Bayesian ($2,$4,1) }

  expr:
      FLOAT                           { A.FloatExp ($1,1) }
    | BOOLEAN                         { A.BoolExp ($1,1) }
    | id callargs                     { A.FunCallExp ($1,$2,1) }
    | id                              { A.IdExp ($1,1) }
    | IF expr THEN expr ELSE expr     { A.IfElseExp ($2,$4,$6,1) }
    | LET id EQ expr                  { A.LetExp ($2,$4,1) }
    | LET id args EQ expr             { A.FunDefExp ($2,$3,$5,1) }
    | SAMPLE distribution             { A.SampleExp ($2,1) }
    | OBSERVE expr                    { A.ObserveExp ($2,1) }
/* Repetition is required for associativity */
    | expr PLUS expr                  { A.OpExp($1,A.PlusOp,$3,1) }
    | expr ASTR expr                  { A.OpExp($1,A.MultOp,$3,1) }
    | expr MINUS expr                 { A.OpExp($1,A.MinusOp,$3,1) }
    | expr SLASH expr                 { A.OpExp($1,A.DivOp,$3,1) }
    | expr BAR expr                   { A.OpExp($1,A.OrOp,$3,1) }
    | expr AMP expr                   { A.OpExp($1,A.AndOp,$3,1) }
    | expr LT expr                    { A.OpExp($1,A.LtOp,$3,1) }
    | expr GT expr                    { A.OpExp($1,A.GtOp,$3,1) }
    | expr EQ expr                    { A.OpExp($1,A.EqOp,$3,1) }
    | NOT expr                        { A.NotExp ($2,1) }
    | LPAR expr RPAR                  { $2 }

  exprs:
      expr                            { $1 }
    | expr exprs                      { A.Seq ($1,$2,1) }

  id:
      ID                              { A.Id ($1,1) }

  callargs:
      /* empty */
    | LPAR callargsp RPAR             { $2 }

  callargsp:
      expr COMMA callargsp            { A.CallArgs ($1,$3,1) }
    | expr COMMA                      { $1 }

  args:
      /* empty */
    | LPAR argsp RPAR                 { $2 }

  argsp:
      id COMMA argsp                  { A.Args ($1,$3,1) }
    | id COMMA                        { A.Args ($1,Nil,1) }

  distribution:
      BERNOULLI expr                  { A.Bernoulli($2,1) }
      ;
