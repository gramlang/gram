/*
  This Bison file exists only to ensure that the grammar is unambigous. The resulting Bison-
  generated parser isn't used. Instead, we use the hand-written packrat parser in `src/parser.rs`
  for better control over error messages.

  Some naturally left-associative binary operations such as application and subtraction are encoded
  as right-associative in this grammar to avoid left-recursion, since packrat parsers can't handle
  left-recursion. Such operations are reassociated to the left in a post-processing step.
*/

/*
  Bison's default parsing algorithm is LALR(1), but more grammars are possible if we configure it
  to use an LR(1) algorithm instead. Since we aren't going to use the generated parser, the
  computational complexity of the algorithm doesn't matter. We choose IELR(1) over the canonical
  LR(1) algorithm because it results in fewer conflicts when the grammar isn't LR(1), which makes
  grammar development easier.
*/

%define lr.type ielr

/* [tag:bison_tokens] [ref:tokens] */

%token ASTERISK
%token BOOLEAN
%token COLON
%token DOUBLE_EQUALS
%token ELSE
%token EQUALS
%token FALSE
%token GREATER_THAN
%token GREATER_THAN_OR_EQUAL
%token IDENTIFIER
%token IF
%token INTEGER
%token INTEGER_LITERAL
%token LEFT_PAREN
%token LESS_THAN
%token LESS_THAN_OR_EQUAL
%token MINUS
%token PLUS
%token RIGHT_PAREN
%token SLASH
%token TERMINATOR
%token THEN
%token THICK_ARROW
%token THIN_ARROW
%token TRUE
%token TYPE

%%

/* [tag:bison_grammar] [ref:grammar] */

term: let | jumbo_term;

type: TYPE;

variable: IDENTIFIER;

lambda: LEFT_PAREN IDENTIFIER COLON jumbo_term RIGHT_PAREN THICK_ARROW term;

pi: LEFT_PAREN IDENTIFIER COLON jumbo_term RIGHT_PAREN THIN_ARROW term;

non_dependent_pi: small_term THIN_ARROW term;

application: atom small_term;

let: IDENTIFIER let_annotation EQUALS term TERMINATOR term;

let_annotation: %empty | COLON small_term;

integer: INTEGER;

integer_literal: INTEGER_LITERAL;

boolean: BOOLEAN;

true: TRUE;

false: FALSE;

if: IF term THEN term ELSE term

sum: medium_term PLUS large_term;

difference: medium_term MINUS large_term;

product: small_term ASTERISK medium_term;

quotient: small_term SLASH medium_term;

less_than: large_term LESS_THAN large_term;

less_than_or_equal_to: large_term LESS_THAN_OR_EQUAL large_term;

equal_to: large_term DOUBLE_EQUALS large_term;

greater_than: large_term GREATER_THAN large_term;

greater_than_or_equal_to: large_term GREATER_THAN_OR_EQUAL large_term;

group: LEFT_PAREN term RIGHT_PAREN;

atom: type | variable | integer | integer_literal | boolean | true | false | group;

small_term: application | atom;

medium_term: product | quotient | small_term;

large_term: sum | difference | medium_term;

huge_term: less_than | less_than_or_equal_to | equal_to | greater_than | greater_than_or_equal_to;

jumbo_term: non_dependent_pi | lambda | pi | if | huge_term;
