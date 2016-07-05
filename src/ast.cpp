#include "ast.h"

  gram::Term::Term() {
  }

  gram::Abstraction::Abstraction(
    std::string argument_name,
    gram::Term *argument_type,
    gram::Term *body) :
    argument_name(argument_name),
    argument_type(argument_type),
    body(body) {
  }

  gram::Variable::Variable(std::string name, gram::Abstraction *abstraction) :
    name(name), abstraction(abstraction) {
  }

  gram::Application::Application(gram::Term *abstraction, gram::Term *argument) :
    abstraction(abstraction), argument(argument) {
  }

  gram::PiType::PiType(std::string argument_name, gram::Term *argument_type, gram::Term *body) :
    argument_name(argument_name), argument_type(argument_type), body(body) {
  }

  gram::Type::Type() {
  }
