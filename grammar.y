/*
  This Bison file exists only to ensure that the grammar is unambigous. The resulting Bison-
  generated parser isn't used. Instead, we use the hand-written packrat parser in `src/parser.rs`
  for better control over error messages.
*/

/*
  Bison's default parsing algorithm is LALR(1), but more grammars are possible if we configure it
  to use an LR(1) algorithm instead. Since we aren't going to use the generated parser, the
  computational complexity of the algorithm doesn't matter. We choose IELR(1) over the canonical
  LR(1) algorithm because it results in fewer conflicts when the grammar is ambiguous, which makes
  grammar development easier.
*/

%define lr.type ielr

/* [tag:bison_tokens] [ref:tokens] */

%token COLON
%token EQUALS
%token SEMICOLON
%token LEFT_PAREN
%token RIGHT_PAREN
%token THICK_ARROW
%token THIN_ARROW
%token TYPE
%token IDENTIFIER

%%

/* [tag:bison_grammar] [ref:grammar] */

term:
  TYPE |
  IDENTIFIER |
  non_arrow_term THIN_ARROW term |
  LEFT_PAREN IDENTIFIER COLON term RIGHT_PAREN THIN_ARROW term |
  LEFT_PAREN IDENTIFIER COLON term RIGHT_PAREN THICK_ARROW term |
  application |
  IDENTIFIER EQUALS term SEMICOLON term |
  LEFT_PAREN term RIGHT_PAREN ;

application:
  applicand non_arrow_term ;

applicand:
  TYPE |
  IDENTIFIER |
  LEFT_PAREN term RIGHT_PAREN ;

non_arrow_term:
  TYPE |
  IDENTIFIER |
  application |
  LEFT_PAREN term RIGHT_PAREN ;
