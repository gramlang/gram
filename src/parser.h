/*
  This header declares the interface to the parser.
*/

#ifndef GRAM_PARSER_H
#define GRAM_PARSER_H

#include <string>

namespace gram {

  class Term {
  public:
    virtual std::string show() = 0;
    virtual ~Term();
  };

  class Abstraction : public Term {
  public:
    std::string argument_name;
    gram::Term *argument_type;
    gram::Term *body;
    Abstraction(std::string argument_name, gram::Term *argument_type, gram::Term *body);
    std::string show();
  };

  class Variable : public Term {
  public:
    std::string name;
    gram::Abstraction *abstraction;
    Variable(std::string name, gram::Abstraction *abstraction);
    std::string show();
  };

  class Application : public Term {
  public:
    gram::Term *abstraction;
    gram::Term *argument;
    Application(gram::Term *abstraction, gram::Term *argument);
    std::string show();
  };

  class PiType : public Term {
  public:
    std::string argument_name;
    gram::Term *argument_type;
    gram::Term *body;
    PiType(std::string argument_name, gram::Term *argument_type, gram::Term *body);
    std::string show();
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
    std::string show();
  };

}

#endif
