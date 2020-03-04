/*
  This Bison file exists only to ensure that the grammar is unambigous. The resulting Bison-
  generated parser isn't used. Instead, we use the hand-written packrat parser in `src/parser.rs`
  for better control over error messages.

  Application is right-associative in this grammar to avoid left-recursion, since packrat parsers
  can't handle left-recursion. Note however that non-grouped applications are re-associated to the
  left in a post-processing step, because left-associative application gives better ergonomics for
  currying. See [ref:reassociate_applications] for details.
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

%token COLON
%token EQUALS
%token IDENTIFIER
%token INTEGER
%token INTEGER_LITERAL
%token LEFT_PAREN
%token MINUS
%token PLUS
%token RIGHT_PAREN
%token TERMINATOR
%token THICK_ARROW
%token THIN_ARROW
%token TYPE

%%

/* [tag:bison_grammar] [ref:grammar] */

term: let | complex_term;

type: TYPE;

variable: IDENTIFIER;

lambda: LEFT_PAREN IDENTIFIER COLON complex_term RIGHT_PAREN THICK_ARROW term;

pi: LEFT_PAREN IDENTIFIER COLON complex_term RIGHT_PAREN THIN_ARROW term;

non_dependent_pi: simple_term THIN_ARROW term;

application: atom simple_term;

let: IDENTIFIER let_annotation EQUALS term TERMINATOR term;

let_annotation: %empty | COLON simple_term;

integer: INTEGER;

integer_literal: INTEGER_LITERAL;

group: LEFT_PAREN term RIGHT_PAREN;

atom: type | variable | integer | integer_literal | group;

simple_term: application | atom;

sum: simple_term PLUS medium_term;

difference: simple_term MINUS medium_term;

medium_term: sum | difference | simple_term;

complex_term: non_dependent_pi | lambda | pi | medium_term;
