#include "parser.h"

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
  std::vector<gram::Token> &tokens,
  std::string &source_name
) {
  return std::unique_ptr<Node>();
}
