{
  open Printf
  open Lexing
  open BayesParse

  exception Did_Not_Close_Comment
  (*let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
  ;;*)
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule bayes_lang = parse
    | "<[" {comments 0 lexbuf}
    | "bayesian" {BAYESIAN}
    | "where" {WHERE}
    | "let" {LET}
    | "=" {EQ}
    | "sample" {SAMPLE}
    | "observe" {OBSERVE}
    | "Bernoulli" {BERNOULLI}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "not" {NOT}
    | "(" {LPAR}
    | ")" {RPAR}
    | "+" {PLUS}
    | "*" {ASTR}
    | "-" {MINUS}
    | "/" {SLASH}
    | "|" {BAR}
    | "&" {AMP}
    | "<" {LT}
    | ">" {GT}
    | "," {COMMA}
    | "true" {BOOLEAN(true)}
    | "false" {BOOLEAN(false)}
    | digit+ "%" as per
	{
	  let len = String.length per -1 in
	  let nop = String.sub per 0 len in
	  FLOAT((float_of_string nop)/.100.0)
	}
    | digit+ ("." digit+)? as f { FLOAT(float_of_string f) }

    | id as iden {ID(iden)}
    | ";" [^'\n']* {bayes_lang lexbuf}
    | '\n' {bayes_lang lexbuf}
    | [' ' '\t'] {bayes_lang lexbuf}
    | _ as c
  	{ printf "Unrecognized character: %d\n" (Char.code c);
	  bayes_lang lexbuf
	}
    | eof {EOF}

and comments level = parse
    | "]>"      {
                  if level = 0 then bayes_lang lexbuf
                  else comments (level-1) lexbuf
                }
    | "<["      {comments (level+1) lexbuf}
    | _	        {comments level lexbuf}
    | eof       {raise Did_Not_Close_Comment}
