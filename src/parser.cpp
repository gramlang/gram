#include "error.h"
#include "parser.h"
#include <functional>
#include <tuple>
#include <unordered_map>

/*
  Gram uses a packrat parser, i.e., a recursive descent parser with
  memoization. This guarantees linear-time parsing.

  In the following augmented Backus-Naur form (ABNF) grammars, nonterminals are
  written in UpperCamelCase and terminals (tokens) are written in MACRO_CASE.

  We start with a grammar which formalizes the language constructs, but without
  concerning ourselves with precedence and associativity. The grammar below is
  ambiguous, and below we will resolve ambiguities by encoding precedence and
  associativity into the production rules.

    Node = Term / Definition
    Term = Variable / Group / Abstraction / ArrowType / Application
    Variable = IDENTIFIER
    Abstraction = Variable THIN_ARROW Term
    ArrowType = Term THICK_ARROW Term
    Application = Term Term
    Group = LEFT_PAREN *(Node SEPARATOR) Term RIGHT_PAREN
    Definition = Variable COLON Term EQUALS Term

  We note the following ambiguities, and the chosen resolutions:

    # Resolution: Abstraction has higher precedence than ArrowType.
    (x -> t) => t
    x -> (t => t)

    # Resolution: ArrowType is right-associative.
    t => (t => t)
    (t => t) => t

    # Resolution: Application has higher precedence than ArrowType.
    t => (t t)
    (t => t) t

    # Resolution: Application has higher precedence than Abstraction.
    x -> (t t)
    (x -> t) t

    # Resolution: Application has higher precedence than ArrowType.
    (t x) => t
    t (x => t)

    # Resolution: Application is left-associative.
    (t x) t
    t (x t)

    # Resolution: The right side of an Application cannot be an Abstraction.
    t (x -> (x t))
    (t (x -> x)) t

  We can resolve the ambiguities in the grammar by expanding definitions and
  eliminating alternatives:

    Node = Term / Definition
    Term = Variable / Group / Abstraction / ArrowType / Application
    Variable = IDENTIFIER
    Abstraction = Variable THIN_ARROW (
      Variable / Group / Abstraction / Application
    )
    ArrowType = (Variable / Group / Abstraction / Application) THICK_ARROW Term
    Application = (Variable / Group / Application) (Variable / Group)
    Group = LEFT_PAREN *(Node SEPARATOR) Term RIGHT_PAREN
    Definition = Variable COLON Term EQUALS Term

  There is still a problem with this grammar: the Application rule is left-
  recursive, and packrat parsers can't handle left-recursion:

    Application = (Variable / Group / Application) (Variable / Group)

  To fix this, we rewrite the rule to use right-recursion instead:

    Application = (Variable / Group) (Variable / Group / Application)

  This makes Application have right-associativity, which is not what we want.
  In the parsing rule for Application, we use a special trick to flip the
  associativity from right to left. Instead of building up the tree from the
  results of right-recursive calls, we pass the left term (`application_prior`)
  to the right-recursive call and let it assemble the tree with left-
  associativity.
*/

///////////////////////////////////////////////////////////////////////////////
// Memoization                                                               //
///////////////////////////////////////////////////////////////////////////////

enum class MemoType {
  NODE,
  TERM,
  VARIABLE,
  ABSTRACTION,
  ARROW_TYPE,
  APPLICATION,
  GROUP,
  DEFINITION
};

using MemoKey = std::tuple<
  MemoType,
  std::vector<gram::Token>::iterator, // The start token
  bool, // Whether this term is the top-level group
  std::shared_ptr<gram::Term> // The `application_prior` term
>;

using MemoValue = std::tuple<
  std::shared_ptr<gram::Node>, // The returned term
  std::vector<gram::Token>::iterator // The token after the returned term
>;

using MemoMap = std::unordered_map<
  MemoKey,
  MemoValue,
  std::function<size_t(const MemoKey &key)>
>;

#define MEMO_KEY_GENERAL( \
  memo_type, \
  begin, \
  top_level, \
  application_prior \
) \
  make_tuple( \
    MemoType::memo_type, \
    (begin), \
    (top_level), \
    (application_prior) \
  )

#define MEMO_KEY_SIMPLE(memo_type, begin) \
  MEMO_KEY_GENERAL(memo_type, (begin), false, std::shared_ptr<gram::Term>())

#define MEMO_CHECK(memo, key, return_type, next) do { \
  auto &m = (memo); \
  auto memo_result = m.find((key)); \
  if (memo_result != m.end()) { \
    (next) = std::get<1>(memo_result->second); \
    return std::dynamic_pointer_cast<gram::return_type>( \
      std::get<0>(memo_result->second) \
    ); \
  } \
} while (false)

#define MEMOIZE_AND_RETURN(memo_key, node, next) do { \
  auto n = (node); \
  memo.insert({(memo_key), make_tuple( \
    std::dynamic_pointer_cast<gram::Node>(n), \
    (next) \
  )}); \
  return n; \
} while (false)

#define MEMOIZE_AND_FAIL(memo_key, type, begin, next) do { \
  auto n = std::shared_ptr<gram::type>(); \
  (next) = (begin); \
  memo.insert({(memo_key), make_tuple( \
    std::dynamic_pointer_cast<gram::Node>(n), \
    (next) \
  )}); \
  return n; \
} while (false)

///////////////////////////////////////////////////////////////////////////////
// Parsing                                                                   //
///////////////////////////////////////////////////////////////////////////////

#define TRY_RULE(begin, next, node, candidate) do { \
  auto old_next = (next); \
  (next) = (begin); \
  auto c = (candidate); \
  if (c) { \
    if (node) { \
      if (c->end_pos > node->end_pos) { \
        (node) = c; \
      } else { \
        (next) = old_next; \
      } \
    } else { \
      (node) = c; \
    } \
  } else { \
    (next) = old_next; \
  } \
} while (false)

void span_tokens(
  gram::Node &node,
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end
);

void span_nodes(
  gram::Node &node,
  gram::Node &begin,
  gram::Node &end
);

std::shared_ptr<gram::Node> parse_node(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
);

std::shared_ptr<gram::Term> parse_term(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
);

std::shared_ptr<gram::Variable> parse_variable(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
);

std::shared_ptr<gram::Abstraction> parse_abstraction(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
);

std::shared_ptr<gram::ArrowType> parse_arrow_type(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
);

std::shared_ptr<gram::Application> parse_application(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next,
  std::shared_ptr<gram::Term> application_prior
);

std::shared_ptr<gram::Group> parse_group(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next,
  bool top_level
);

std::shared_ptr<gram::Definition> parse_definition(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
);

void span_tokens(
  gram::Node &node,
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end
) {
  if (begin < end) {
    node.source_name = begin->source_name;
    node.source = begin->source;
    node.start_pos = begin->start_pos;
    node.end_pos = (end - 1)->end_pos;
  }
}

void span_nodes(
  gram::Node &node,
  gram::Node &begin,
  gram::Node &end
) {
  node.source_name = begin.source_name;
  node.source = begin.source;
  node.start_pos = begin.start_pos;
  node.end_pos = end.end_pos;
}

std::shared_ptr<gram::Node> parse_node(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_SIMPLE(NODE, begin);
  MEMO_CHECK(memo, memo_key, Node, next);

  // This is what we will return to the caller.
  std::shared_ptr<gram::Node> node;

  // Term
  TRY_RULE(begin, next, node, parse_term(memo, tokens, next));

  // Definition
  TRY_RULE(begin, next, node, parse_definition(memo, tokens, next));

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, node, next);
}

std::shared_ptr<gram::Term> parse_term(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_SIMPLE(TERM, begin);
  MEMO_CHECK(memo, memo_key, Term, next);

  // A term is one of the following constructs.
  std::shared_ptr<gram::Term> term;
  TRY_RULE(begin, next, term, parse_variable(memo, tokens, next));
  TRY_RULE(begin, next, term, parse_group(memo, tokens, next, false));
  TRY_RULE(begin, next, term, parse_abstraction(memo, tokens, next));
  TRY_RULE(begin, next, term, parse_arrow_type(memo, tokens, next));
  TRY_RULE(
    begin,
    next,
    term,
    parse_application(memo, tokens, next, std::shared_ptr<gram::Term>())
  );

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, term, next);
}

std::shared_ptr<gram::Variable> parse_variable(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_SIMPLE(VARIABLE, begin);
  MEMO_CHECK(memo, memo_key, Variable, next);

  // Make sure we have an IDENTIFIER.
  if (next == tokens.end() || next->type != gram::TokenType::IDENTIFIER) {
    MEMOIZE_AND_FAIL(memo_key, Variable, begin, next);
  }

  // Construct the Variable.
  auto variable = std::make_shared<gram::Variable>(next->literal);
  ++next;
  span_tokens(*variable, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, variable, next);
}

std::shared_ptr<gram::Abstraction> parse_abstraction(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_SIMPLE(ABSTRACTION, begin);
  MEMO_CHECK(memo, memo_key, Abstraction, next);

  // Parse the variable.
  auto variable = parse_variable(memo, tokens, next);
  if (!variable) {
    MEMOIZE_AND_FAIL(memo_key, Abstraction, begin, next);
  }

  // Parse the THIN_ARROW token.
  if (next == tokens.end() || next->type != gram::TokenType::THIN_ARROW) {
    MEMOIZE_AND_FAIL(memo_key, Abstraction, begin, next);
  }
  ++next;

  // Parse the body.
  if (next == tokens.end()) {
    throw gram::Error(
      "This function needs a body.",
      *(begin->source), *(begin->source_name),
      begin->start_pos, (next - 1)->end_pos
    );
  }
  std::shared_ptr<gram::Term> body;
  auto body_begin = next;
  TRY_RULE(body_begin, next, body, parse_variable(memo, tokens, next));
  TRY_RULE(body_begin, next, body, parse_group(memo, tokens, next, false));
  TRY_RULE(body_begin, next, body, parse_abstraction(memo, tokens, next));
  TRY_RULE(
    body_begin,
    next,
    body,
    parse_application(memo, tokens, next, std::shared_ptr<gram::Term>())
  );
  if (!body) {
    throw gram::Error(
      "A function body was expected here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Construct the Abstraction.
  auto abstraction = std::make_shared<gram::Abstraction>(variable, body);
  span_tokens(*abstraction, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, abstraction, next);
}

std::shared_ptr<gram::ArrowType> parse_arrow_type(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_SIMPLE(ARROW_TYPE, begin);
  MEMO_CHECK(memo, memo_key, ArrowType, next);

  // Make sure we have some tokens to parse.
  if (next == tokens.end()) {
    MEMOIZE_AND_FAIL(memo_key, ArrowType, begin, next);
  }

  // Parse the domain.
  std::shared_ptr<gram::Term> domain;
  auto domain_begin = next;
  TRY_RULE(domain_begin, next, domain, parse_variable(memo, tokens, next));
  TRY_RULE(domain_begin, next, domain, parse_group(memo, tokens, next, false));
  TRY_RULE(domain_begin, next, domain, parse_abstraction(memo, tokens, next));
  TRY_RULE(
    domain_begin,
    next,
    domain,
    parse_application(memo, tokens, next, std::shared_ptr<gram::Term>())
  );
  if (!domain) {
    MEMOIZE_AND_FAIL(memo_key, ArrowType, begin, next);
  }

  // Parse the THICK_ARROW.
  if (next == tokens.end() || next->type != gram::TokenType::THICK_ARROW) {
    MEMOIZE_AND_FAIL(memo_key, ArrowType, begin, next);
  }
  ++next;

  // Parse the codomain.
  if (next == tokens.end()) {
    throw gram::Error(
      "This function type needs a codomain.",
      *(begin->source), *(begin->source_name),
      begin->start_pos, (next - 1)->end_pos
    );
  }
  auto codomain = parse_term(memo, tokens, next);
  if (!codomain) {
    throw gram::Error(
      "A codomain was expected here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Construct the ArrowType.
  auto arrow_type = std::make_shared<gram::ArrowType>(domain, codomain);
  span_nodes(*arrow_type, *domain, *codomain);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, arrow_type, next);
}

std::shared_ptr<gram::Application> parse_application(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next,
  std::shared_ptr<gram::Term> application_prior
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_GENERAL(
    APPLICATION,
    begin,
    false,
    application_prior
  );
  MEMO_CHECK(memo, memo_key, Application, next);

  // Parse the left term.
  std::shared_ptr<gram::Term> left_term;
  auto left_term_begin = next;
  TRY_RULE(
    left_term_begin,
    next,
    left_term,
    parse_variable(memo, tokens, next)
  );
  TRY_RULE(
    left_term_begin,
    next,
    left_term,
    parse_group(memo, tokens, next, false)
  );
  if (!left_term) {
    MEMOIZE_AND_FAIL(memo_key, Application, begin, next);
  }

  // Parse the right term.
  std::shared_ptr<gram::Term> right_term;
  auto right_term_begin = next;
  TRY_RULE(
    right_term_begin,
    next,
    right_term,
    parse_variable(memo, tokens, next)
  );
  TRY_RULE(
    right_term_begin,
    next,
    right_term,
    parse_group(memo, tokens, next, false)
  );
  std::shared_ptr<gram::Application> right_application;
  if (application_prior) {
    auto prior_of_left = std::make_shared<gram::Application>(
      application_prior,
      left_term
    );
    span_nodes(*prior_of_left, *application_prior, *left_term);
    TRY_RULE(
      right_term_begin,
      next,
      right_term,
      parse_application(memo, tokens, next, prior_of_left)
    );
  } else {
    TRY_RULE(
      right_term_begin,
      next,
      right_term,
      parse_application(memo, tokens, next, left_term)
    );
  }
  if (!right_term) {
    MEMOIZE_AND_FAIL(memo_key, Application, begin, next);
  }

  // Construct the Application. Special care is taken to construct the tree
  // with left-associativity, even though we are parsing with right-recursion.
  std::shared_ptr<gram::Application> application;
  right_application = std::dynamic_pointer_cast<gram::Application>(
    right_term
  );
  if (right_application) {
    application = right_application;
  } else {
    if (application_prior) {
      application = std::make_shared<gram::Application>(
        application = std::make_shared<gram::Application>(
          application_prior,
          left_term
        ),
        right_term
      );
      span_nodes(*application, *application_prior, *right_term);
    } else {
      application = std::make_shared<gram::Application>(
        left_term,
        right_term
      );
      span_nodes(*application, *left_term, *right_term);
    }
  }

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, application, next);
}

std::shared_ptr<gram::Group> parse_group(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next,
  bool top_level
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_GENERAL(
    GROUP,
    begin,
    top_level,
    std::shared_ptr<gram::Term>()
  );
  MEMO_CHECK(memo, memo_key, Group, next);

  // Parse the LEFT_PAREN, if applicable.
  if (next == tokens.end()) {
    MEMOIZE_AND_FAIL(memo_key, Group, begin, next);
  }
  if (!top_level) {
    if (next->type != gram::TokenType::LEFT_PAREN) {
      MEMOIZE_AND_FAIL(memo_key, Group, begin, next);
    }
    ++next;
  }

  // Keep eating the input until we reach a RIGHT_PAREN token or the end of
  // the stream. Note: the tokenizer guarantees that all LEFT_*/RIGHT_* tokens
  // are matched, so we don't need to worry about ensuring there is a
  // RIGHT_PAREN.
  std::vector<std::shared_ptr<gram::Node>> body;
  bool expecting_separator = false;
  auto last_separator_iter = tokens.end(); // Sentinel value
  while (next != tokens.end() && next->type != gram::TokenType::RIGHT_PAREN) {
    // Skip SEPARATOR tokens.
    if (next->type == gram::TokenType::SEPARATOR) {
      last_separator_iter = next;
      if (!expecting_separator) {
        throw gram::Error(
          "Superfluous separator.",
          *(next->source), *(next->source_name),
          next->start_pos, next->end_pos
        );
      }

      expecting_separator = false;
      ++next;
      continue;
    }

    if (expecting_separator) {
      // There shouldn't be any situations where this could happen.
      throw gram::Error(
        "Missing separator before this symbol.",
        *(next->source), *(next->source_name),
        next->start_pos, next->end_pos
      );
    }

    // Do recursive descent to get a body node.
    auto prev = next;
    auto node = parse_node(memo, tokens, next);

    // If we are in this loop, there better be at least one node.
    // If we didn't get one, throw an error.
    if (!node) {
      throw gram::Error(
        "Unexpected symbol.",
        *(prev->source), *(prev->source_name),
        prev->start_pos, prev->end_pos
      );
    }

    // Add the node to the body.
    body.push_back(node);
    expecting_separator = true;
    last_separator_iter = tokens.end();
  }

  if (last_separator_iter != tokens.end()) {
    throw gram::Error(
      "Superfluous separator.",
      *(last_separator_iter->source),
      *(last_separator_iter->source_name),
      last_separator_iter->start_pos,
      last_separator_iter->end_pos
    );
  }

  // Make sure the abstraction has something to return.
  if (body.empty()) {
    if (top_level) {
      MEMOIZE_AND_FAIL(memo_key, Group, begin, next);
    } else {
      throw gram::Error(
        "Empty group.",
        *(begin->source), *(begin->source_name),
        begin->start_pos, (next - 1)->end_pos
      );
    }
  }
  if (!top_level && !std::dynamic_pointer_cast<gram::Term>(body.back())) {
    throw gram::Error(
      "A group must end with an expression.",
      *(body.back()->source), *(body.back()->source_name),
      body.back()->start_pos, body.back()->end_pos
    );
  }

  // Skip the RIGHT_PAREN token if there is one.
  if (!top_level) {
    ++next;
  }

  // Construct the Group.
  auto group = std::make_shared<gram::Group>(body);
  span_tokens(*group, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, group, next);
}

std::shared_ptr<gram::Definition> parse_definition(
  MemoMap &memo,
  std::vector<gram::Token> &tokens,
  std::vector<gram::Token>::iterator &next
) {
  // Check if we can reuse a memoized result.
  auto begin = next;
  auto memo_key = MEMO_KEY_SIMPLE(DEFINITION, begin);
  MEMO_CHECK(memo, memo_key, Definition, next);

  // Parse the variable.
  auto variable = parse_variable(memo, tokens, next);
  if (!variable) {
    MEMOIZE_AND_FAIL(memo_key, Definition, begin, next);
  }

  // Parse the COLON.
  if (next == tokens.end() || next->type != gram::TokenType::COLON) {
    MEMOIZE_AND_FAIL(memo_key, Definition, begin, next);
  }
  ++next;

  // Parse the ascription.
  if (next == tokens.end()) {
    throw gram::Error(
      "This binding needs an ascription.",
      *(begin->source), *(begin->source_name),
      begin->start_pos, (next - 1)->end_pos
    );
  }
  auto ascription = parse_term(memo, tokens, next);
  if (!ascription) {
    throw gram::Error(
      "An ascription for '" + variable->name + "' was expected here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Parse the EQUALS.
  if (next == tokens.end()) {
    throw gram::Error(
      "A definition for '" + variable->name + "' was expected here.",
      *(begin->source), *(begin->source_name),
      begin->start_pos, (next - 1)->end_pos
    );
  }
  if (next->type != gram::TokenType::EQUALS) {
    throw gram::Error(
      "A definition for '" + variable->name + "' was expected here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }
  ++next;

  // Parse the body.
  if (next == tokens.end()) {
    throw gram::Error(
      "This binding needs a definition.",
      *(begin->source), *(begin->source_name),
      begin->start_pos, (next - 1)->end_pos
    );
  }
  auto body = parse_term(memo, tokens, next);
  if (!body) {
    throw gram::Error(
      "A definition for '" + variable->name + "' was expected here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Construct the Definition.
  auto definition = std::make_shared<gram::Definition>(
    variable,
    ascription,
    body
  );
  span_tokens(*definition, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, definition, next);
}

///////////////////////////////////////////////////////////////////////////////
// Public interface                                                          //
///////////////////////////////////////////////////////////////////////////////

std::shared_ptr<gram::Node> gram::parse(std::vector<gram::Token> &tokens) {
  // Memoize the results of recursive descent calls.
  // This is the "packrat parser" technique.
  MemoMap memo(1000, [&tokens](const MemoKey &key) {
    // Unpack the tuple.
    auto memo_type = std::get<0>(key);
    auto begin = std::get<1>(key);
    auto top_level = std::get<2>(key);
    auto application_prior = std::get<3>(key);

    // Get the hash of each component.
    size_t memo_type_hash =
      static_cast<typename std::underlying_type<MemoType>::type>(memo_type);
    size_t begin_hash = 0;
    if (begin != tokens.end()) {
      begin_hash = reinterpret_cast<size_t>(&(*begin));
    }
    size_t top_level_hash = top_level ? 1 : 0;
    size_t application_prior_hash = 0;
    if (application_prior) {
      application_prior_hash = reinterpret_cast<size_t>(&(*application_prior));
    }

    // To combine the hashes, we use the hash_combine trick from Boost.
    size_t combined_hash = memo_type_hash;
    combined_hash ^= 0x9e3779b9 +
      (combined_hash << 6) + (combined_hash >> 2) + begin_hash;
    combined_hash ^= 0x9e3779b9 +
      (combined_hash << 6) + (combined_hash >> 2) + top_level_hash;
    combined_hash ^= 0x9e3779b9 +
      (combined_hash << 6) + (combined_hash >> 2) + application_prior_hash;
    return combined_hash;
  });

  // Let the helper do all the work.
  std::vector<gram::Token>::iterator next = tokens.begin();
  std::shared_ptr<gram::Node> node = parse_group(memo, tokens, next, true);

  // Make sure we parsed the whole file.
  if (next != tokens.end()) {
    throw gram::Error(
      "The end of the file was expected here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Go on to type checking!
  return node;
}
