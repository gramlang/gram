/*
  This header declares the abstract syntax tree.
*/

#ifndef GRAM_AST_H
#define GRAM_AST_H

#include <string>

namespace gram {

  class Term {
  protected:
    Term();
  };

  class Abstraction : public Term {
  public:
    std::string argument_name;
    gram::Term *argument_type;
    gram::Term *body;
    Abstraction(std::string argument_name, gram::Term *argument_type, gram::Term *body);
  };

  class Variable : public Term {
  public:
    std::string name;
    gram::Abstraction *abstraction;
    Variable(std::string name, gram::Abstraction *abstraction);
  };

  class Application : public Term {
  public:
    gram::Term *abstraction;
    gram::Term *argument;
    Application(gram::Term *abstraction, gram::Term *argument);
  };

  class PiType : public Term {
  public:
    std::string argument_name;
    gram::Term *argument_type;
    gram::Term *body;
    PiType(std::string argument_name, gram::Term *argument_type, gram::Term *body);
  };

  class Type : public Term {
  private:
    static Type *instance;
    Type();

  public:
    static Type &get_instance() {
      static Type instance;
      return instance;
    }
  };

}

#endif
