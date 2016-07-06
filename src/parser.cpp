#include "error.h"
#include "parser.h"
#include <utility>

gram::Node::~Node() {
}

gram::Term::~Term() {
}

gram::Abstraction::Abstraction(
  std::string argument_name,
  std::unique_ptr<gram::Term> argument_type,
  std::unique_ptr<gram::Term> body) :
  argument_name(argument_name), argument_type(std::move(argument_type)), body(std::move(body)) {
}

std::string gram::Abstraction::show() {
  return "(" + argument_name + ": " + (argument_type ? argument_type->show() : "?") + " -> " +
    (body ? body->show() : "?") + ")";
}

gram::Variable::Variable(std::string name) :
  name(name) {
}

std::string gram::Variable::show() {
  return name;
}

gram::Application::Application(
  std::unique_ptr<gram::Term> abstraction, std::unique_ptr<gram::Term> argument
) : abstraction(std::move(abstraction)), argument(std::move(argument)) {
}

std::string gram::Application::show() {
  return std::string(abstraction ? abstraction->show() : "?") + " " +
    std::string(argument ? argument->show() : "?");
}

gram::PiType::PiType(
  std::string argument_name,
  std::unique_ptr<gram::Term> argument_type,
  std::unique_ptr<gram::Term> body) :
  argument_name(argument_name), argument_type(std::move(argument_type)), body(std::move(body)) {
}

std::string gram::PiType::show() {
  return "(" + argument_name + ": " + (argument_type ? argument_type->show() : "?") + " => " +
    (body ? body->show() : "?") + ")";
}

gram::Type::Type() {
}

std::string gram::Type::show() {
  return "Type";
}

gram::Block::Block(std::vector<std::unique_ptr<gram::Node>> body) : body(std::move(body)) {
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

gram::Definition::Definition(std::string name, std::unique_ptr<gram::Term> value) :
  name(name), value(std::move(value)) {
}

std::string gram::Definition::show() {
  return name + " = " + (value ? value->show() : "?");
}

std::unique_ptr<gram::Node> parse_block(
  const std::string &source,
  std::string source_name,
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  bool top_level
) {
  // Make sure we have some tokens to read.
  if (begin == end) {
    return std::unique_ptr<gram::Node>();
  }

  // Make sure we are actually parsing a block
  if (begin->type != gram::TokenType::BEGIN && !top_level) {
    next = begin;
    return std::unique_ptr<gram::Node>();
  }

  // Skip the BEGIN token, if there is one.
  auto pos = begin;
  if (!top_level) {
    ++pos;
  }

  // Keep eating the input until we reach an END token or the end of the stream.
  // Note: the lexer guarantees that all BEGIN/END tokens are matched, so we
  // don't need to worry about ensuring there is an END.
  std::vector<std::unique_ptr<gram::Node>> body;
  while (pos != end && pos->type != gram::TokenType::END) {
    // Skip sequencers.
    if (pos->type == gram::TokenType::SEQUENCER) {
      ++pos;
      continue;
    }

    // Do recursive descent to get a body node
    auto node = gram::parse(source, source_name, pos, end, pos, false);

    // If we are in this loop, there better be at least one node.
    // If we didn't get one, throw an error.
    if (!node) {
      throw gram::Error(
        "Unexpected token: " + pos->show(),
        source, source_name,
        pos->start_line, pos->start_col,
        pos->end_line, pos->end_col
      );
    }

    // Add the node to the body.
    body.push_back(std::move(node));
  }

  // Tell the caller where we ended up.
  // Skip the END token if there is one.
  next = pos;
  if (!top_level) {
    ++next;
  }

  // Create the node and pass ownership to the caller.
  return std::unique_ptr<gram::Node>(new gram::Block(std::move(body)));
}

std::unique_ptr<gram::Node> gram::parse(
  const std::string &source,
  std::string source_name,
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator end,
  std::vector<gram::Token>::iterator &next,
  bool top_level
) {
  // This function just implements recursive descent, relying on the other functions
  // to do all the work. The following node is what we will return to the caller.
  std::unique_ptr<Node> node;

  // Blocks (or the top-level source)
  node = parse_block(source, source_name, begin, end, next, top_level);
  if (node) {
    return node;
  }

  // If we made it this far, nothing was parsed.
  return node;
}
