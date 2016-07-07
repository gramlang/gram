#include "error.h"
#include "parser.h"
#include <utility>

std::shared_ptr<gram::Node> greedy_parse(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next, // Only mutated if a Node is returned
  std::shared_ptr<gram::Node> prior_node // Used to parse abstractions with left-associativity
);

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
  argument_type(std::move(argument_type)),
  body(std::move(body)) {
}

std::string gram::Abstraction::show() {
  return "(" +
    argument_name + ": " +
    (argument_type ? argument_type->show() : "?") + " -> " +
    (body ? body->show() : "?") +
  ")";
}

gram::Variable::Variable(std::string name) :
  name(name) {
}

std::string gram::Variable::show() {
  return name;
}

gram::Application::Application(
  std::shared_ptr<gram::Term> abstraction,
  std::shared_ptr<gram::Term> operand) :
  abstraction(std::move(abstraction)),
  operand(std::move(operand)) {
}

std::string gram::Application::show() {
  return "(" +
    std::string(abstraction ? abstraction->show() : "?") + " " +
    std::string(operand ? operand->show() : "?") +
  ")";
}

gram::PiType::PiType(
  std::string argument_name,
  std::shared_ptr<gram::Term> argument_type,
  std::shared_ptr<gram::Term> body) :
  argument_name(argument_name),
  argument_type(std::move(argument_type)),
  body(std::move(body)) {
}

std::string gram::PiType::show() {
  return "(" +
    argument_name + ": " +
    (argument_type ? argument_type->show() : "?") + " => " +
    (body ? body->show() : "?") +
  ")";
}

gram::Type::Type() {
}

std::string gram::Type::show() {
  return "Type";
}

gram::Block::Block(std::vector<std::shared_ptr<gram::Node>> body) : body(std::move(body)) {
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
    if (term) {
      result += term->show();
    }
  }
  result += ")";
  return result;
}

gram::Definition::Definition(std::string name, std::shared_ptr<gram::Term> value) :
  name(name), value(std::move(value)) {
}

std::string gram::Definition::show() {
  return name + " = " + (value ? value->show() : "?");
}

std::shared_ptr<gram::Term> node_to_term(std::shared_ptr<gram::Node> node) {
  auto term = std::dynamic_pointer_cast<gram::Term>(node);
  if (node && !term) {
    throw gram::Error(
      "Expected a term here.",
      *(node->source), *(node->source_name),
      node->start_line, node->start_col,
      node->end_line, node->end_col
    );
  }
  return std::shared_ptr<gram::Term>(term);
}

std::shared_ptr<gram::Node> parse_block(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  bool top_level
) {
  // Make sure we have some tokens to read.
  if (begin == end) {
    next = begin;
    return std::shared_ptr<gram::Node>();
  }

  // Make sure we are actually parsing a block.
  if (begin->type != gram::TokenType::BEGIN && !top_level) {
    next = begin;
    return std::shared_ptr<gram::Node>();
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
    auto node = greedy_parse(pos, end, pos, std::shared_ptr<gram::Node>());

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
    body.push_back(std::move(node));
  }

  // Make sure the abstraction has something to return.
  if (body.empty()) {
    if (top_level) {
      next = begin;
      return std::shared_ptr<gram::Node>();
    } else {
      throw gram::Error(
        "A block must end with a term.",
        *(pos->source), *(pos->source_name),
        pos->start_line, pos->start_col,
        pos->end_line, pos->end_col
      );
    }
  }
  if (!top_level && dynamic_cast<gram::Term *>(body.back().get()) == nullptr) {
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

  // Create the node and pass ownership to the caller.
  auto block = std::shared_ptr<gram::Node>(new gram::Block(std::move(body)));
  block->span_tokens(begin, next);
  return block;
}

std::shared_ptr<gram::Node> parse_abstraction_or_pi_type(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next
) {
  // Make sure we have some tokens to read.
  if (begin == end) {
    next = begin;
    return std::shared_ptr<gram::Node>();
  }

  // Make sure we are actually parsing an abstraction or pi type.
  if (begin + 1 >= end || begin->type != gram::TokenType::IDENTIFIER || (
    (begin + 1)->type != gram::TokenType::COLON &&
    (begin + 1)->type != gram::TokenType::THIN_ARROW &&
    (begin + 1)->type != gram::TokenType::THICK_ARROW
  )) {
    next = begin;
    return std::shared_ptr<gram::Node>();
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
        "This looks like the beginning of an abstraction or pi type, but there is no arrow.",
        *(begin->source), *(begin->source_name),
        begin->start_line, begin->start_col,
        (begin + 1)->end_line, (begin + 1)->end_col
      );
    }

    // Parse the argument type by recursive descent.
    argument_type = node_to_term(
      greedy_parse(pos, arrow_pos, pos, std::shared_ptr<gram::Node>())
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
      "Missing body for abstraction or pi type of '" + argument_name + "'.",
      *((pos - 1)->source), *((pos - 1)->source_name),
      (pos - 1)->start_line, (pos - 1)->start_col,
      (pos - 1)->end_line, (pos - 1)->end_col
    );
  }
  auto body = node_to_term(greedy_parse(pos, end, pos, std::shared_ptr<gram::Node>()));
  if (!body) {
    throw gram::Error(
      "Missing body for abstraction or pi type of '" + argument_name + "'.",
      *(pos->source), *(pos->source_name),
      pos->start_line, pos->start_col,
      pos->end_line, pos->end_col
    );
  }

  // Tell the caller where we ended up.
  next = pos;

  // Create the node and pass ownership to the caller.
  std::shared_ptr<gram::Node> abstraction_or_pi_type;
  if (thin_arrow) {
    abstraction_or_pi_type = std::shared_ptr<gram::Node>(
      new gram::Abstraction(argument_name, std::move(argument_type), std::move(body))
    );
  } else {
    abstraction_or_pi_type = std::shared_ptr<gram::Node>(
      new gram::PiType(argument_name, std::move(argument_type), std::move(body))
    );
  }
  abstraction_or_pi_type->span_tokens(begin, next);
  return abstraction_or_pi_type;
}

std::shared_ptr<gram::Node> parse_definition(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next
) {
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
  auto body = node_to_term(greedy_parse(pos, end, next, std::shared_ptr<gram::Node>()));
  if (!body) {
    throw gram::Error(
      "Missing definition of '" + variable_name + "'.",
      *(pos->source), *(pos->source_name),
      pos->start_line, pos->start_col,
      pos->end_line, pos->end_col
    );
  }

  // Create the node and pass ownership to the caller.
  auto definition = std::shared_ptr<gram::Node>(
    new gram::Definition(variable_name, std::move(body))
  );
  definition->span_tokens(begin, next);
  return definition;
}

std::shared_ptr<gram::Node> parse_variable(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next
) {
  // Make sure we have some tokens to read.
  if (begin == end) {
    next = begin;
    return std::shared_ptr<gram::Node>();
  }

  // Make sure we are actually parsing a variable.
  if (begin->type != gram::TokenType::IDENTIFIER) {
    next = begin;
    return std::shared_ptr<gram::Node>();
  }

  // Tell the caller where we ended up.
  next = begin + 1;

  // Create the node and pass ownership to the caller.
  auto variable = std::shared_ptr<gram::Node>(new gram::Variable(begin->literal));
  variable->span_tokens(begin, next);
  return variable;
}

std::shared_ptr<gram::Node> greedy_parse(
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  std::shared_ptr<gram::Node> prior_node
) {
  // This function just implements recursive descent, relying on the other functions
  // to do all the work. The following node is what we will return to the caller.
  std::shared_ptr<gram::Node> node;

  // Block
  if (!node) {
    node = parse_block(begin, end, next, false);
  }

  // Abstraction or pi type
  if (!node) {
    node = parse_abstraction_or_pi_type(begin, end, next);
  }

  // Definition
  if (!node) {
    node = parse_definition(begin, end, next);
  }

  // Variable
  if (!node) {
    node = parse_variable(begin, end, next);
  }

  // Application (we use the foldl method to parse with left-associativity)
  if (prior_node && node) {
    auto start_line = prior_node->start_line;
    auto start_col = prior_node->start_col;
    auto end_line = node->end_line;
    auto end_col = node->end_col;
    auto application = std::shared_ptr<gram::Node>(
      new gram::Application(
        node_to_term(std::move(prior_node)),
        node_to_term(std::move(node))
      )
    );
    application->start_line = start_line;
    application->start_col = start_col;
    application->end_line = end_line;
    application->end_col = end_col;
    node = greedy_parse(next, end, next, std::move(application));
  } else {
    if (prior_node && !node) {
      node = std::move(prior_node);
    } else if (node && !prior_node) {
      node = greedy_parse(next, end, next, std::move(node));
    }
  }

  // Return whatever we parsed.
  return node;
}

std::shared_ptr<gram::Node> gram::parse(std::vector<gram::Token> &tokens) {
  // Let the helper do all the work.
  std::vector<gram::Token>::iterator next;
  std::shared_ptr<gram::Node> node = parse_block(tokens.begin(), tokens.end(), next, true);

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
