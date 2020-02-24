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
  LR(1) algorithm because it results in fewer conflicts when the grammar is ambiguous, which makes
  grammar development easier.
*/

%define lr.type ielr

/* [tag:bison_tokens] [ref:tokens] */

%token COLON
%token EQUALS
%token LEFT_PAREN
%token RIGHT_PAREN
%token TERMINATOR
%token THICK_ARROW
%token THIN_ARROW
%token TYPE
%token IDENTIFIER

%%

/* [tag:bison_grammar] [ref:grammar] */

term: non_dependent_pi | application | let | type | variable | pi | lambda | group;

type: TYPE;

variable: IDENTIFIER;

non_dependent_pi: term_minus_arrows_let THIN_ARROW term;

pi: LEFT_PAREN IDENTIFIER COLON term_minus_let RIGHT_PAREN THIN_ARROW term;

lambda: LEFT_PAREN IDENTIFIER COLON term_minus_let RIGHT_PAREN THICK_ARROW term;

application: applicand term_minus_arrows_let;

let: IDENTIFIER let_annotation EQUALS term TERMINATOR term;

let_annotation: %empty | COLON term_minus_arrows_let;

group: LEFT_PAREN term RIGHT_PAREN;

applicand: type | variable | group;

term_minus_arrows_let: application | type | variable | group;

term_minus_let: non_dependent_pi | application | type | variable | pi | lambda | group;
