#include "parser.h"

gram::Term::~Term() {
}

gram::Abstraction::Abstraction(
  std::string argument_name,
  gram::Term *argument_type,
  gram::Term *body) :
  argument_name(argument_name),
  argument_type(argument_type),
  body(body) {
}

std::string gram::Abstraction::show() {
  return "(" + argument_name + ": " + (argument_type ? argument_type->show() : "?") + " -> " +
    (body ? body->show() : "?") + ")";
}

gram::Variable::Variable(std::string name, gram::Abstraction *abstraction) :
  name(name), abstraction(abstraction) {
}

std::string gram::Variable::show() {
  return name;
}

gram::Application::Application(gram::Term *abstraction, gram::Term *argument) :
  abstraction(abstraction), argument(argument) {
}

std::string gram::Application::show() {
  return std::string(abstraction ? abstraction->show() : "?") + " " +
    std::string(argument ? argument->show() : "?");
}

gram::PiType::PiType(
  std::string argument_name,
  gram::Term *argument_type,
  gram::Term *body) :
  argument_name(argument_name),
  argument_type(argument_type),
  body(body) {
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
