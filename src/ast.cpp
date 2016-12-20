#include "ast.h"

gram::Node::~Node() {
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

gram::Block::Block(
  std::vector<std::shared_ptr<gram::Node>> body
) : body(body) {
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

gram::Definition::Definition(
  std::string name, std::shared_ptr<gram::Term> value
) : name(name), value(value) {
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
