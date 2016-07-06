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

std::unique_ptr<gram::Node> gram::parse(
  const std::string &source,
  std::string source_name,
  std::vector<gram::Token>::iterator begin,
  std::vector<gram::Token>::iterator &end,
  bool top_level
) {
  // Make sure we have some tokens to read.
  if (begin == end) {
    return std::unique_ptr<Node>();
  }

  // Blocks
  if (begin->type == TokenType::BEGIN || top_level) {
    std::vector<std::unique_ptr<gram::Node>> body;
    auto pos = begin;
    if (!top_level) {
      ++pos;
    }
    while (pos != end && pos->type != TokenType::END) {
      while (pos->type == TokenType::SEQUENCER) {
        ++pos;
      }
      auto new_end = end;
      auto node = parse(source, source_name, pos, new_end, false);
      if (new_end == pos) {
        throw Error(
          "Unexpected token: " + new_end->show(),
          source, source_name,
          new_end->start_line, new_end->start_col,
          new_end->end_line, new_end->end_col
        );
      }
      if (node) {
        body.push_back(std::move(node));
      }
      pos = new_end;
    }
    end = pos;
    if (!top_level) {
      ++end;
    }
    return std::unique_ptr<Node>(new Block(std::move(body)));
  }

  // If we made it this far, nothing was parsed.
  end = begin;
  return std::unique_ptr<Node>();
}
