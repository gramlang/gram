/*
  This Bison file exists only to ensure that the grammar is unambigous. The resulting Bison-
  generated parser isn't used. Instead, we use a hand-written packrat parser. See src/parser.rs for
  the implementation.
*/

/*
  The default parsing algorithm is LALR(1), but we can accept more grammars by switching to an
  LR(1) algorithm, such as IELR(1) (Inadequacy Elimination LR(1)). Since we aren't going to use the
  generated parser, we don't care about the computational complexity of the algorithm. We only care
  about the set of languages that it accepts.
*/

%define lr.type ielr

/* [tag:bison_tokens] [ref:tokens] */

%token COLON
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
