#include "error.h"
#include "parser.h"
#include <functional>
#include <tuple>
#include <unordered_map>
#include <utility>

///////////////
// AST NODES //
///////////////

gram::Node::~Node() {
}

void gram::Node::span_tokens(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end
) {
  if (begin < end) {
    source_name = begin->source_name;
    source = begin->source;
    start_line = begin->start_line;
    start_col = begin->start_col;
    end_line = (end - 1)->end_line;
    end_col = (end - 1)->end_col;
  }
}

gram::Term::~Term() {
}

gram::Abstraction::Abstraction(
  std::string argument_name,
  std::shared_ptr<gram::Term> argument_type,
  std::shared_ptr<gram::Term> body) :
  argument_name(argument_name),
  argument_type(argument_type),
  body(body) {
}

std::string gram::Abstraction::show() {
  return "(" +
    argument_name + ": " +
    (argument_type ? argument_type->show() : "?") + " -> " +
    (body ? body->show() : "?") +
  ")";
}

std::unique_ptr<gram::Node> gram::Abstraction::clone() {
  return std::unique_ptr<Node>(new Abstraction(
    argument_name,
    std::shared_ptr<Term>(
      argument_type ?
        static_cast<Term*>(argument_type->clone().release()) :
        nullptr
    ), std::shared_ptr<Term>(
      body ?
        static_cast<Term*>(body->clone().release()) :
        nullptr
    )
  ));
}

gram::ArrowType::ArrowType(
  std::string argument_name,
  std::shared_ptr<gram::Term> argument_type,
  std::shared_ptr<gram::Term> body) :
  argument_name(argument_name),
  argument_type(argument_type),
  body(body) {
}

std::string gram::ArrowType::show() {
  return "(" +
    argument_name + ": " +
    (argument_type ? argument_type->show() : "?") + " => " +
    (body ? body->show() : "?") +
  ")";
}

std::unique_ptr<gram::Node> gram::ArrowType::clone() {
  return std::unique_ptr<Node>(new ArrowType(
    argument_name,
    std::shared_ptr<Term>(
      argument_type ?
        static_cast<Term*>(argument_type->clone().release()) :
        nullptr
    ), std::shared_ptr<Term>(
      body ?
        static_cast<Term*>(body->clone().release()) :
        nullptr
    )
  ));
}

gram::Variable::Variable(std::string name) :
  name(name) {
}

std::string gram::Variable::show() {
  return name;
}

std::unique_ptr<gram::Node> gram::Variable::clone() {
  return std::unique_ptr<Node>(new Variable(name));
}

gram::Application::Application(
  std::shared_ptr<gram::Term> abstraction,
  std::shared_ptr<gram::Term> operand) :
  abstraction(abstraction),
  operand(operand) {
}

std::string gram::Application::show() {
  return "(" +
    std::string(abstraction ? abstraction->show() : "?") + " " +
    std::string(operand ? operand->show() : "?") +
  ")";
}

std::unique_ptr<gram::Node> gram::Application::clone() {
  return std::unique_ptr<Node>(new Application(
    std::shared_ptr<Term>(
      abstraction ?
        static_cast<Term*>(abstraction->clone().release()) :
        nullptr
    ), std::shared_ptr<Term>(
      operand ?
        static_cast<Term*>(operand->clone().release()) :
        nullptr
    )
  ));
}

gram::Block::Block(std::vector<std::shared_ptr<gram::Node>> body) : body(body) {
}

std::string gram::Block::show() {
  std::string result = "(";
  bool first = true;
  for (const auto &term : body) {
    if (first) {
      first = false;
    } else {
      result += "; ";
    }
    result += term ? term->show() : "<null>";
  }
  result += ")";
  return result;
}

std::unique_ptr<gram::Node> gram::Block::clone() {
  std::vector<std::shared_ptr<Node>> clone_body;
  for (const auto & node : body) {
    clone_body.push_back(
      node ?
        std::shared_ptr<Node>(node->clone().release()) :
        nullptr
    );
  }
  return std::unique_ptr<Node>(new Block(clone_body));
}

gram::Definition::Definition(std::string name, std::shared_ptr<gram::Term> value) :
  name(name), value(value) {
}

std::string gram::Definition::show() {
  return name + " = " + (value ? value->show() : "?");
}

std::unique_ptr<gram::Node> gram::Definition::clone() {
  return std::unique_ptr<Node>(new Definition(
    name,
    std::shared_ptr<Term>(
      value ?
        static_cast<Term*>(value->clone().release()) :
        nullptr
    )
  ));
}

////////////////
// THE PARSER //
////////////////

enum class MemoType {
  NODE,
  TERM,
  ABSTRACTION_OR_ARROW_TYPE,
  VARIABLE,
  BLOCK,
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

std::shared_ptr<gram::Node> parse_node(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next, // Only mutated if a Node is returned
  MemoMap &memo
);

std::shared_ptr<gram::Term> parse_term(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next, // Only mutated if a Node is returned
  std::shared_ptr<gram::Term> prior_term, // Used to parse abstractions with left-associativity
  MemoMap &memo
);

std::shared_ptr<gram::Term> parse_abstraction_or_arrow_type(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
);

std::shared_ptr<gram::Term> parse_variable(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
);

std::shared_ptr<gram::Term> parse_block(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  bool top_level,
  MemoMap &memo
);

std::shared_ptr<gram::Node> parse_definition(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
);

std::shared_ptr<gram::Node> parse_node(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = make_tuple(MemoType::TERM, begin, end, std::shared_ptr<gram::Term>());
  auto memo_result = memo.find(memo_key);
  if (memo_result != memo.end()) {
    next = std::get<1>(memo_result->second);
    return std::get<0>(memo_result->second);
  }

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
  memo.insert({memo_key, make_tuple(node, next)});
  return node;
}

std::shared_ptr<gram::Term> parse_term(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  std::shared_ptr<gram::Term> prior_term,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = make_tuple(MemoType::TERM, begin, end, prior_term);
  auto memo_result = memo.find(memo_key);
  if (memo_result != memo.end()) {
    next = std::get<1>(memo_result->second);
    return std::dynamic_pointer_cast<gram::Term>(std::get<0>(memo_result->second));
  }

  // This is what we will return to the caller.
  std::shared_ptr<gram::Term> term;

  // Block
  if (!term) {
    term = parse_block(begin, end, next, false, memo);
  }

  // Abstraction or arrow type
  if (!term) {
    term = parse_abstraction_or_arrow_type(begin, end, next, memo);
  }

  // Variable
  if (!term) {
    term = parse_variable(begin, end, next, memo);
  }

  // Application (we use the foldl method to parse with left-associativity)
  if (prior_term && term) {
    auto start_line = prior_term->start_line;
    auto start_col = prior_term->start_col;
    auto end_line = term->end_line;
    auto end_col = term->end_col;
    auto application = std::make_shared<gram::Application>(
      prior_term,
      term
    );
    application->start_line = start_line;
    application->start_col = start_col;
    application->end_line = end_line;
    application->end_col = end_col;
    term = parse_term(next, end, next, application, memo);
  } else {
    if (prior_term && !term) {
      term = prior_term;
    } else if (term && !prior_term) {
      term = parse_term(next, end, next, term, memo);
    }
  }

  // Memoize whatever we parsed and return it.
  memo.insert({memo_key, make_tuple(
    std::dynamic_pointer_cast<gram::Node>(term),
    next
  )});
  return term;
}

std::shared_ptr<gram::Term> parse_abstraction_or_arrow_type(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = make_tuple(MemoType::ABSTRACTION_OR_ARROW_TYPE, begin, end, std::shared_ptr<gram::Term>());
  auto memo_result = memo.find(memo_key);
  if (memo_result != memo.end()) {
    next = std::get<1>(memo_result->second);
    return std::dynamic_pointer_cast<gram::Term>(std::get<0>(memo_result->second));
  }

  // Make sure we have some tokens to read.
  if (begin == end) {
    next = begin;
    return std::shared_ptr<gram::Term>();
  }

  // Make sure we are actually parsing an abstraction or arrow type.
  if (begin + 1 >= end || begin->type != gram::TokenType::IDENTIFIER || (
    (begin + 1)->type != gram::TokenType::COLON &&
    (begin + 1)->type != gram::TokenType::THIN_ARROW &&
    (begin + 1)->type != gram::TokenType::THICK_ARROW
  )) {
    next = begin;
    return std::shared_ptr<gram::Term>();
  }

  // Get the name of the argument.
  auto pos = begin;
  std::string argument_name = pos->literal;
  ++pos;

  // Get the type of the argument, if provided.
  std::shared_ptr<gram::Term> argument_type;
  if (pos->type == gram::TokenType::COLON) {
    ++pos;

    // Find the position of the arrow.
    // We do this so that the argument type isn't greedily parsed until the end of the file.
    auto arrow_pos = pos;
    int indentation = 0;
    int colons_minus_arrows = 0;
    while (arrow_pos != end) {
      if (arrow_pos->type == gram::TokenType::BEGIN) {
        ++indentation;
      }
      if (arrow_pos->type == gram::TokenType::END) {
        --indentation;
      }
      if (indentation == 0) {
        if (arrow_pos->type == gram::TokenType::COLON) {
          ++colons_minus_arrows;
        }
        if (arrow_pos->type == gram::TokenType::THIN_ARROW ||
          arrow_pos->type == gram::TokenType::THICK_ARROW) {
          if (colons_minus_arrows == 0) {
            break;
          }
          --colons_minus_arrows;
        }
      }
      ++arrow_pos;
    }
    if (arrow_pos == end) {
      throw gram::Error(
        "This looks like the beginning of an abstraction or arrow type, but there is no arrow.",
        *(begin->source), *(begin->source_name),
        begin->start_line, begin->start_col,
        (begin + 1)->end_line, (begin + 1)->end_col
      );
    }

    // Parse the argument type by recursive descent.
    argument_type = parse_term(pos,
      arrow_pos,
      pos,
      std::shared_ptr<gram::Term>(),
      memo
    );
    if (!argument_type) {
      throw gram::Error(
        "Expected a type annotation here.",
        *(pos->source), *(pos->source_name),
        pos->start_line, pos->start_col,
        pos->end_line, pos->end_col
      );
    }
  }

  // Determine the type of arrow.
  if (pos == end || (
    pos->type != gram::TokenType::THIN_ARROW &&
    pos->type != gram::TokenType::THICK_ARROW
  )) {
    throw gram::Error(
      "Expected an arrow here.",
      *(pos->source), *(pos->source_name),
      pos->start_line, pos->start_col,
      pos->end_line, pos->end_col
    );
  }
  bool thin_arrow = (pos->type == gram::TokenType::THIN_ARROW);
  ++pos;

  // Parse the body by recursive descent.
  if (pos == end) {
    throw gram::Error(
      "Missing body for abstraction or arrow type of '" + argument_name + "'.",
      *((pos - 1)->source), *((pos - 1)->source_name),
      (pos - 1)->start_line, (pos - 1)->start_col,
      (pos - 1)->end_line, (pos - 1)->end_col
    );
  }
  auto body = parse_term(
    pos,
    end,
    pos,
    std::shared_ptr<gram::Term>(),
    memo
  );
  if (!body) {
    throw gram::Error(
      "Missing body for abstraction or arrow type of '" + argument_name + "'.",
      *(pos->source), *(pos->source_name),
      pos->start_line, pos->start_col,
      pos->end_line, pos->end_col
    );
  }

  // Tell the caller where we ended up.
  next = pos;

  // Construct the node.
  std::shared_ptr<gram::Term> abstraction_or_arrow_type;
  if (thin_arrow) {
    abstraction_or_arrow_type = std::make_shared<gram::Abstraction>(
      argument_name,
      argument_type,
      body
    );
  } else {
    abstraction_or_arrow_type = std::make_shared<gram::ArrowType>(
      argument_name,
      argument_type,
      body
    );
  }
  abstraction_or_arrow_type->span_tokens(begin, next);

  // Memoize whatever we parsed and return it.
  memo.insert({memo_key, make_tuple(
    std::dynamic_pointer_cast<gram::Node>(abstraction_or_arrow_type),
    next
  )});
  return abstraction_or_arrow_type;
}

std::shared_ptr<gram::Term> parse_variable(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = make_tuple(MemoType::VARIABLE, begin, end, std::shared_ptr<gram::Term>());
  auto memo_result = memo.find(memo_key);
  if (memo_result != memo.end()) {
    next = std::get<1>(memo_result->second);
    return std::dynamic_pointer_cast<gram::Term>(std::get<0>(memo_result->second));
  }

  // Make sure we have some tokens to read.
  if (begin == end) {
    next = begin;
    return std::shared_ptr<gram::Term>();
  }

  // Make sure we are actually parsing a variable.
  if (begin->type != gram::TokenType::IDENTIFIER) {
    next = begin;
    return std::shared_ptr<gram::Term>();
  }

  // Tell the caller where we ended up.
  next = begin + 1;

  // Construct the node.
  auto variable = std::make_shared<gram::Variable>(begin->literal);
  variable->span_tokens(begin, next);

  // Memoize whatever we parsed and return it.
  memo.insert({memo_key, make_tuple(
    std::dynamic_pointer_cast<gram::Node>(variable),
    next
  )});
  return variable;
}

std::shared_ptr<gram::Term> parse_block(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  bool top_level,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = make_tuple(MemoType::BLOCK, begin, end, std::shared_ptr<gram::Term>());
  auto memo_result = memo.find(memo_key);
  if (!top_level && memo_result != memo.end()) {
    next = std::get<1>(memo_result->second);
    return std::dynamic_pointer_cast<gram::Term>(std::get<0>(memo_result->second));
  }

  // Make sure we have some tokens to read.
  if (begin == end) {
    next = begin;
    return std::shared_ptr<gram::Term>();
  }

  // Make sure we are actually parsing a block.
  if (begin->type != gram::TokenType::BEGIN && !top_level) {
    next = begin;
    return std::shared_ptr<gram::Term>();
  }

  // Skip the BEGIN token, if there is one.
  auto pos = begin;
  if (!top_level) {
    ++pos;
  }

  // Keep eating the input until we reach an END token or the end of the stream.
  // Note: the lexer guarantees that all BEGIN/END tokens are matched, so we
  // don't need to worry about ensuring there is an END.
  std::vector<std::shared_ptr<gram::Node>> body;
  while (pos != end && pos->type != gram::TokenType::END) {
    // Skip sequencers.
    if (pos->type == gram::TokenType::SEQUENCER) {
      ++pos;
      continue;
    }

    // Do recursive descent to get a body node.
    auto node = parse_node(
      pos,
      end,
      pos,
      memo
    );

    // If we are in this loop, there better be at least one node.
    // If we didn't get one, throw an error.
    if (!node) {
      throw gram::Error(
        "Unexpected token encountered here.",
        *(pos->source), *(pos->source_name),
        pos->start_line, pos->start_col,
        pos->end_line, pos->end_col
      );
    }

    // Add the node to the body.
    body.push_back(node);
  }

  // Make sure the abstraction has something to return.
  if (body.empty()) {
    if (top_level) {
      next = begin;
      return std::shared_ptr<gram::Term>();
    } else {
      throw gram::Error(
        "A block must end with a term.",
        *(pos->source), *(pos->source_name),
        pos->start_line, pos->start_col,
        pos->end_line, pos->end_col
      );
    }
  }
  if (!top_level && !std::dynamic_pointer_cast<gram::Term>(body.back())) {
    throw gram::Error(
      "A block must end with a term.",
      *(pos->source), *(pos->source_name),
      body.back()->start_line, body.back()->start_col,
      body.back()->end_line, body.back()->end_col
    );
  }

  // Tell the caller where we ended up.
  // Skip the END token if there is one.
  next = pos;
  if (!top_level) {
    ++next;
  }

  // Construct the node.
  auto block = std::make_shared<gram::Block>(body);
  block->span_tokens(begin, next);

  // Memoize whatever we parsed and return it.
  memo.insert({memo_key, make_tuple(
    std::dynamic_pointer_cast<gram::Node>(block),
    next
  )});
  return block;
}

std::shared_ptr<gram::Node> parse_definition(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  MemoMap &memo
) {
  // Check if we can reuse a memoized result.
  auto memo_key = make_tuple(MemoType::DEFINITION, begin, end, std::shared_ptr<gram::Term>());
  auto memo_result = memo.find(memo_key);
  if (memo_result != memo.end()) {
    next = std::get<1>(memo_result->second);
    return std::dynamic_pointer_cast<gram::Term>(std::get<0>(memo_result->second));
  }

  // Make sure we have some tokens to read.
  if (begin == end) {
    next = begin;
    return std::shared_ptr<gram::Node>();
  }

  // Make sure we are actually parsing a definition.
  if (begin + 1 >= end ||
    begin->type != gram::TokenType::IDENTIFIER ||
    (begin + 1)->type != gram::TokenType::EQUALS
  ) {
    next = begin;
    return std::shared_ptr<gram::Node>();
  }

  // Get the name of the variable.
  auto pos = begin;
  std::string variable_name = pos->literal;
  pos += 2;

  // Parse the body by recursive descent.
  if (pos == end) {
    throw gram::Error(
      "Missing definition of '" + variable_name + "'.",
      *((pos - 1)->source), *((pos - 1)->source_name),
      (pos - 1)->start_line, (pos - 1)->start_col,
      (pos - 1)->end_line, (pos - 1)->end_col
    );
  }
  auto body = parse_term(
    pos,
    end,
    next,
    std::shared_ptr<gram::Term>(),
    memo
  );
  if (!body) {
    throw gram::Error(
      "Unexpected token encountered here.",
      *(pos->source), *(pos->source_name),
      pos->start_line, pos->start_col,
      pos->end_line, pos->end_col
    );
  }

  // Construct the node.
  auto definition = std::make_shared<gram::Definition>(variable_name, body);
  definition->span_tokens(begin, next);

  // Memoize whatever we parsed and return it.
  memo.insert({memo_key, make_tuple(
    std::dynamic_pointer_cast<gram::Node>(definition),
    next
  )});
  return definition;
}

std::shared_ptr<gram::Node> gram::parse(std::vector<gram::Token> &tokens) {
  // Memoize the results of recursive descent calls.
  // This is the "packrat parser" technique.
  MemoMap memo(1000, [=](const MemoKey &key) {
    // Unpack the tuple.
    auto memo_type = std::get<0>(key);
    auto begin = std::get<1>(key);
    auto end = std::get<2>(key);
    auto prior_term = std::get<3>(key);

    // Get the hash of each component.
    size_t memo_type_hash = static_cast<typename std::underlying_type<MemoType>::type>(memo_type);
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
    combined_hash ^= 0x9e3779b9 + (combined_hash << 6) + (combined_hash >> 2) + memo_type_hash;
    combined_hash ^= 0x9e3779b9 + (combined_hash << 6) + (combined_hash >> 2) + end_hash;
    combined_hash ^= 0x9e3779b9 + (combined_hash << 6) + (combined_hash >> 2) + prior_term_hash;
    return combined_hash;
  });

  // Let the helper do all the work.
  std::vector<gram::Token>::iterator next;
  std::shared_ptr<gram::Node> node = parse_block(
    tokens.begin(),
    tokens.end(),
    next,
    true,
    memo
  );

  // Make sure we parsed the whole file.
  if (next != tokens.end()) {
    throw gram::Error(
      "Unexpected token encountered here.",
      *(next->source), *(next->source_name),
      next->start_line, next->start_col,
      next->end_line, next->end_col
    );
  }

  // Go on to type checking!
  return node;
}
