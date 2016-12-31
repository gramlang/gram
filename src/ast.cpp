#include "ast.h"

gram::Node::~Node() {
}

gram::Term::~Term() {
}

std::string gram::Term::show_type_and_ascription() {
  return
    (type ? (" :: (" + type->show() + ")") : "") +
    (ascription ? (" : (" + ascription->show() + ")") : "");
}

gram::Variable::Variable(std::string name) :
  name(name) {
}

std::string gram::Variable::show() {
  return name + show_type_and_ascription();
}

std::unique_ptr<gram::Node> gram::Variable::clone() {
  return std::unique_ptr<Node>(new Variable(name));
}

gram::Abstraction::Abstraction(
  std::shared_ptr<gram::Variable> variable,
  std::shared_ptr<gram::Term> body) :
  variable(variable),
  body(body) {
}

std::string gram::Abstraction::show() {
  return "(" +
    (variable ? variable->show() : "?") +
    " -> " +
    (body ? body->show() : "?") +
  ")" + show_type_and_ascription();
}

std::unique_ptr<gram::Node> gram::Abstraction::clone() {
  return std::unique_ptr<Node>(new Abstraction(
    std::shared_ptr<Variable>(
      variable ?
        static_cast<Variable*>(variable->clone().release()) :
        nullptr
    ), std::shared_ptr<Term>(
      body ?
        static_cast<Term*>(body->clone().release()) :
        nullptr
    )
  ));
}

gram::ArrowType::ArrowType(
  std::shared_ptr<gram::Term> domain,
  std::shared_ptr<gram::Term> codomain) :
  domain(domain),
  codomain(codomain) {
}

std::string gram::ArrowType::show() {
  return "(" +
    (domain ? domain->show() : "?") +
    " => " +
    (codomain ? codomain->show() : "?") +
  ")" + show_type_and_ascription();
}

std::unique_ptr<gram::Node> gram::ArrowType::clone() {
  return std::unique_ptr<Node>(new ArrowType(
    std::shared_ptr<Term>(
      domain ?
        static_cast<Term*>(domain->clone().release()) :
        nullptr
    ), std::shared_ptr<Term>(
      codomain ?
        static_cast<Term*>(codomain->clone().release()) :
        nullptr
    )
  ));
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
  ")" + show_type_and_ascription();
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

gram::Group::Group(
  std::vector<std::shared_ptr<gram::Node>> body
) : body(body) {
}

std::string gram::Group::show() {
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
  result += ")" + show_type_and_ascription();
  return result;
}

std::unique_ptr<gram::Node> gram::Group::clone() {
  std::vector<std::shared_ptr<Node>> clone_body;
  for (const auto & node : body) {
    clone_body.push_back(
      node ?
        std::shared_ptr<Node>(node->clone().release()) :
        nullptr
    );
  }
  return std::unique_ptr<Node>(new Group(clone_body));
}

gram::Definition::Definition(
  std::shared_ptr<gram::Variable> variable, std::shared_ptr<gram::Term> value
) : variable(variable), value(value) {
}

std::string gram::Definition::show() {
  return
    (variable ? variable->show() : "?") +
    " = " +
    (value ? value->show() : "?");
}

std::unique_ptr<gram::Node> gram::Definition::clone() {
  return std::unique_ptr<Node>(new Definition(
    std::shared_ptr<Variable>(
      variable ?
        static_cast<Variable*>(variable->clone().release()) :
        nullptr
    ),
    std::shared_ptr<Term>(
      value ?
        static_cast<Term*>(value->clone().release()) :
        nullptr
    )
  ));
}
