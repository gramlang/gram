#include "error.h"
#include "parser.h"
#include <functional>
#include <tuple>
#include <unordered_map>

enum class MemoType {
  NODE,
  TERM,
  VARIABLE,
  ABSTRACTION,
  ARROW_TYPE,
  GROUP,
  DEFINITION
};

using MemoKey = std::tuple<
  MemoType,
  std::vector<gram::Token>::iterator, // begin
  std::vector<gram::Token>::iterator, // end
  std::shared_ptr<gram::Term> // prior_term
>;

using MemoValue = std::tuple<
  std::shared_ptr<gram::Node>, // the returned term
  std::vector<gram::Token>::iterator // next
>;

using MemoMap = std::unordered_map<
  MemoKey,
  MemoValue,
  std::function<size_t(const MemoKey &key)>
>;

#define MEMO_KEY_WITH_PRIOR_TERM(memo_type, begin, end, prior_term) \
  make_tuple( \
    MemoType::memo_type, begin, end, prior_term \
  )

#define MEMO_KEY_WITHOUT_PRIOR_TERM(memo_type, begin, end) \
  MEMO_KEY_WITH_PRIOR_TERM( \
    memo_type, begin, end, std::shared_ptr<gram::Term>() \
  )

#define MEMO_CHECK(memo, key, return_type) do { \
  auto memo_result = memo.find(key); \
  if (memo_result != memo.end()) { \
    next = std::get<1>(memo_result->second); \
    return std::dynamic_pointer_cast<gram::return_type>( \
      std::get<0>(memo_result->second) \
    ); \
  } \
} while (false)

#define MEMOIZE_AND_RETURN(memo_key, node, next) do { \
  memo.insert({memo_key, make_tuple( \
    std::dynamic_pointer_cast<gram::Node>(node), \
    next \
  )}); \
  return node; \
} while (false)

#define MEMOIZE_AND_FAIL(memo_key, type, begin, next) do { \
  next = begin; \
  auto node = std::shared_ptr<gram::type>(); \
  memo.insert({memo_key, make_tuple( \
    std::dynamic_pointer_cast<gram::Node>(node), \
    next \
  )}); \
  return node; \
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
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
);

std::shared_ptr<gram::Term> parse_term(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,

  // Used to parse applications with left-associativity
  std::shared_ptr<gram::Term> prior_term,

  MemoMap &memo
);

std::shared_ptr<gram::Variable> parse_variable(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
);

std::shared_ptr<gram::Abstraction> parse_abstraction(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
);

std::shared_ptr<gram::ArrowType> parse_arrow_type(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  std::shared_ptr<gram::Term> prior_term,
  MemoMap &memo
);

std::shared_ptr<gram::Group> parse_group(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  bool top_level,
  MemoMap &memo
);

std::shared_ptr<gram::Definition> parse_definition(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
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
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = MEMO_KEY_WITHOUT_PRIOR_TERM(NODE, begin, end);
  MEMO_CHECK(memo, memo_key, Node);

  // This is what we will return to the caller.
  std::shared_ptr<gram::Node> node;

  // Definition
  if (!node) {
    node = parse_definition(begin, end, next, memo);
  }

  // Term
  if (!node) {
    node = parse_term(begin, end, next, std::shared_ptr<gram::Term>(), memo);
  }

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, node, next);
}

std::shared_ptr<gram::Term> parse_term(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  std::shared_ptr<gram::Term> prior_term,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = MEMO_KEY_WITH_PRIOR_TERM(TERM, begin, end, prior_term);
  MEMO_CHECK(memo, memo_key, Term);

  // This is what we will return to the caller.
  std::shared_ptr<gram::Term> term;

  // Group
  if (!term) {
    term = parse_group(begin, end, next, false, memo);
  }

  // Abstraction
  if (!term) {
    term = parse_abstraction(begin, end, next, memo);
  }

  // ArrowType
  if (!term) {
    term = parse_arrow_type(begin, end, next, prior_term, memo);
    if (term) {
      prior_term = std::shared_ptr<gram::Term>();
    }
  }

  // Variable
  if (!term) {
    term = parse_variable(begin, end, next, memo);
  }

  // Application (we use the foldl method to parse with left-associativity)
  if (prior_term && term) {
    auto application = std::make_shared<gram::Application>(
      prior_term,
      term
    );
    span_nodes(*application, *prior_term, *term);
    term = parse_term(next, end, next, application, memo);
  } else {
    if (prior_term && !term) {
      term = prior_term;
    } else if (term && !prior_term) {
      term = parse_term(next, end, next, term, memo);
    }
  }

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, term, next);
}

std::shared_ptr<gram::Variable> parse_variable(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = MEMO_KEY_WITHOUT_PRIOR_TERM(VARIABLE, begin, end);
  MEMO_CHECK(memo, memo_key, Variable);

  // Make sure we have some tokens to read.
  if (begin == end) {
    MEMOIZE_AND_FAIL(memo_key, Variable, begin, next);
  }

  // Parse the IDENTIFIER token.
  if (begin->type != gram::TokenType::IDENTIFIER) {
    MEMOIZE_AND_FAIL(memo_key, Variable, begin, next);
  }
  ++next;

  // Construct the node.
  auto variable = std::make_shared<gram::Variable>(begin->literal);
  span_tokens(*variable, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, variable, next);
}

std::shared_ptr<gram::Abstraction> parse_abstraction(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = MEMO_KEY_WITHOUT_PRIOR_TERM(ABSTRACTION, begin, end);
  MEMO_CHECK(memo, memo_key, Abstraction);

  // Parse the variable.
  auto variable = parse_variable(begin, end, next, memo);
  if (!variable) {
    MEMOIZE_AND_FAIL(memo_key, Abstraction, begin, next);
  }

  // Parse the THIN_ARROW token.
  if (next == end || next->type != gram::TokenType::THIN_ARROW) {
    MEMOIZE_AND_FAIL(memo_key, Abstraction, begin, next);
  }
  ++next;

  // Parse the body.
  if (next == end) {
    throw gram::Error(
      "Missing body for abstraction of '" + variable->name + "'.",
      *((next - 1)->source), *((next - 1)->source_name),
      (next - 1)->start_pos, (next - 1)->end_pos
    );
  }
  auto body = parse_term(
    next,
    end,
    next,
    std::shared_ptr<gram::Term>(),
    memo
  );
  if (!body) {
    throw gram::Error(
      "Missing body for abstraction of '" + variable->name + "'.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Construct the node.
  auto abstraction = std::make_shared<gram::Abstraction>(variable, body);
  span_tokens(*abstraction, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, abstraction, next);
}

std::shared_ptr<gram::ArrowType> parse_arrow_type(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  std::shared_ptr<gram::Term> prior_term,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = MEMO_KEY_WITH_PRIOR_TERM(ARROW_TYPE, begin, end, prior_term);
  MEMO_CHECK(memo, memo_key, ArrowType);

  // Make sure we have some tokens to read.
  if (begin == end) {
    MEMOIZE_AND_FAIL(memo_key, ArrowType, begin, next);
  }

  // Parse the THICK_ARROW token.
  if (begin->type != gram::TokenType::THICK_ARROW) {
    MEMOIZE_AND_FAIL(memo_key, ArrowType, begin, next);
  }
  ++next;

  // Make sure we have a domain.
  if (!prior_term) {
    throw gram::Error(
      "Missing domain for arrow type.",
      *((next - 1)->source), *((next - 1)->source_name),
      (next - 1)->start_pos, (next - 1)->end_pos
    );
  }

  // Parse the codomain.
  auto codomain = parse_term(
    next,
    end,
    next,
    std::shared_ptr<gram::Term>(),
    memo
  );

  // Construct the arrow type.
  auto arrow_type = std::make_shared<gram::ArrowType>(prior_term, codomain);
  span_nodes(*arrow_type, *prior_term, *codomain);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, arrow_type, next);
}

std::shared_ptr<gram::Group> parse_group(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  bool top_level,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = MEMO_KEY_WITHOUT_PRIOR_TERM(GROUP, begin, end);
  MEMO_CHECK(memo, memo_key, Group);

  // Make sure we have some tokens to read.
  if (next == end) {
    MEMOIZE_AND_FAIL(memo_key, Group, begin, next);
  }

  // Make sure we are actually parsing a group.
  if (next->type != gram::TokenType::LEFT_PAREN && !top_level) {
    MEMOIZE_AND_FAIL(memo_key, Group, begin, next);
  }

  // Skip the LEFT_PAREN token, if there is one.
  if (!top_level) {
    ++next;
  }

  // Keep eating the input until we reach a RIGHT_PAREN token or the end of
  // the stream. Note: the lexer guarantees that all LEFT_*/RIGHT_* tokens
  // are matched, so we don't need to worry about ensuring there is a
  // RIGHT_PAREN.
  std::vector<std::shared_ptr<gram::Node>> body;
  bool expecting_separator = false;
  auto explicit_separator_pos = end; // Sentinel value
  while (next != end && next->type != gram::TokenType::RIGHT_PAREN) {
    // Skip SEPARATOR tokens.
    if (next->type == gram::TokenType::SEPARATOR) {
      if (next->explicit_separator) {
        explicit_separator_pos = next;
        if (!expecting_separator) {
          throw gram::Error(
            "Unnecessary separator encountered here.",
            *(next->source), *(next->source_name),
            next->start_pos, next->end_pos
          );
        }
      } else {
        if (explicit_separator_pos != end) {
          throw gram::Error(
            "Unnecessary separator encountered here.",
            *(explicit_separator_pos->source),
            *(explicit_separator_pos->source_name),
            explicit_separator_pos->start_pos,
            explicit_separator_pos->end_pos
          );
        }
      }

      expecting_separator = false;
      ++next;
      continue;
    }

    if (expecting_separator) {
      // There shouldn't be any situations where this could happen.
      throw gram::Error(
        "Missing separator before this.",
        *(next->source), *(next->source_name),
        next->start_pos, next->end_pos
      );
    }

    // Do recursive descent to get a body node.
    auto prev = next;
    auto node = parse_node(
      next,
      end,
      next,
      memo
    );

    // If we are in this loop, there better be at least one node.
    // If we didn't get one, throw an error.
    if (!node) {
      throw gram::Error(
        "Unexpected symbol encountered here.",
        *(prev->source), *(prev->source_name),
        prev->start_pos, prev->end_pos
      );
    }

    // Add the node to the body.
    body.push_back(node);
    expecting_separator = true;
    explicit_separator_pos = end;
  }

  if (explicit_separator_pos != end) {
    throw gram::Error(
      "Unnecessary separator encountered here.",
      *(explicit_separator_pos->source),
      *(explicit_separator_pos->source_name),
      explicit_separator_pos->start_pos,
      explicit_separator_pos->end_pos
    );
  }

  // Make sure the abstraction has something to return.
  if (body.empty()) {
    if (top_level) {
      MEMOIZE_AND_FAIL(memo_key, Group, begin, next);
    } else {
      throw gram::Error(
        "A group cannot be empty.",
        *(begin->source), *(begin->source_name),
        begin->start_pos, (next - 1)->end_pos
      );
    }
  }
  if (!top_level && !std::dynamic_pointer_cast<gram::Term>(body.back())) {
    throw gram::Error(
      "A group must end with a term.",
      *(body.back()->source), *(body.back()->source_name),
      body.back()->start_pos, body.back()->end_pos
    );
  }

  // Skip the RIGHT_PAREN token if there is one.
  if (!top_level) {
    ++next;
  }

  // Construct the node.
  auto group = std::make_shared<gram::Group>(body);
  span_tokens(*group, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, group, next);
}

std::shared_ptr<gram::Definition> parse_definition(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = MEMO_KEY_WITHOUT_PRIOR_TERM(DEFINITION, begin, end);
  MEMO_CHECK(memo, memo_key, Definition);

  // Parse the variable.
  auto variable = parse_variable(begin, end, next, memo);
  if (!variable) {
    MEMOIZE_AND_FAIL(memo_key, Definition, begin, next);
  }

  // Parse the EQUALS token.
  if (next == end || next->type != gram::TokenType::EQUALS) {
    MEMOIZE_AND_FAIL(memo_key, Definition, begin, next);
  }
  ++next;

  // Parse the body.
  if (next == end) {
    throw gram::Error(
      "Missing definition of '" + variable->name + "'.",
      *(variable->source), *(variable->source_name),
      variable->start_pos, (next - 1)->end_pos
    );
  }
  auto body = parse_term(
    next,
    end,
    next,
    std::shared_ptr<gram::Term>(),
    memo
  );
  if (!body) {
    throw gram::Error(
      "Unexpected symbol encountered here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Construct the node.
  auto definition = std::make_shared<gram::Definition>(variable, body);
  span_tokens(*definition, begin, next);

  // Memoize whatever we parsed and return it.
  MEMOIZE_AND_RETURN(memo_key, definition, next);
}

std::shared_ptr<gram::Node> gram::parse(std::vector<gram::Token> &tokens) {
  // Memoize the results of recursive descent calls.
  // This is the "packrat parser" technique.
  MemoMap memo(1000, [&tokens](const MemoKey &key) {
    // Unpack the tuple.
    auto memo_type = std::get<0>(key);
    auto begin = std::get<1>(key);
    auto end = std::get<2>(key);
    auto prior_term = std::get<3>(key);

    // Get the hash of each component.
    size_t memo_type_hash =
      static_cast<typename std::underlying_type<MemoType>::type>(memo_type);
    size_t begin_hash = 0;
    size_t end_hash = 0;
    size_t prior_term_hash = 0;
    if (begin != tokens.end()) {
      begin_hash = reinterpret_cast<size_t>(&(*begin));
    }
    if (end != tokens.end()) {
      end_hash = reinterpret_cast<size_t>(&(*end));
    }
    if (prior_term) {
      prior_term_hash = reinterpret_cast<size_t>(&(*prior_term));
    }

    // To combine the hashes, we use the hash_combine trick from Boost.
    size_t combined_hash = begin_hash;
    combined_hash ^= 0x9e3779b9 +
      (combined_hash << 6) + (combined_hash >> 2) + memo_type_hash;
    combined_hash ^= 0x9e3779b9 +
      (combined_hash << 6) + (combined_hash >> 2) + end_hash;
    combined_hash ^= 0x9e3779b9 +
      (combined_hash << 6) + (combined_hash >> 2) + prior_term_hash;
    return combined_hash;
  });

  // Let the helper do all the work.
  std::vector<gram::Token>::iterator next = tokens.begin();
  std::shared_ptr<gram::Node> node = parse_group(
    tokens.begin(),
    tokens.end(),
    next,
    true,
    memo
  );

  // Make sure we parsed the whole file.
  if (next != tokens.end()) {
    throw gram::Error(
      "Unexpected symbol encountered here.",
      *(next->source), *(next->source_name),
      next->start_pos, next->end_pos
    );
  }

  // Go on to type checking!
  return node;
}
