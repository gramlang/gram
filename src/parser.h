/*
  This header declares the interface to the parser.
*/

#ifndef GRAM_PARSER_H
#define GRAM_PARSER_H

#include "lexer.h"
#include <memory>
#include <string>

namespace gram {

  class Node {
  public:
    virtual std::string show() = 0;
    virtual ~Node();
  };

  class Term : public Node {
  public:
    virtual std::string show() = 0;
    virtual ~Term();
  };

  class Abstraction : public Term {
  public:
    std::string argument_name;
    std::unique_ptr<gram::Term> argument_type;
    std::unique_ptr<gram::Term> body;
    Abstraction(
      std::string argument_name,
      std::unique_ptr<gram::Term> argument_type,
      std::unique_ptr<gram::Term> body
    );
    std::string show();
  };

  class Variable : public Term {
  public:
    std::string name;
    explicit Variable(std::string name);
    std::string show();
  };

  class Application : public Term {
  public:
    std::unique_ptr<gram::Term> abstraction;
    std::unique_ptr<gram::Term> argument;
    Application(std::unique_ptr<gram::Term> abstraction, std::unique_ptr<gram::Term> argument);
    std::string show();
  };

  class PiType : public Term {
  public:
    std::string argument_name;
    std::unique_ptr<gram::Term> argument_type;
    std::unique_ptr<gram::Term> body;
    PiType(
      std::string argument_name,
      std::unique_ptr<gram::Term> argument_type,
      std::unique_ptr<gram::Term> body
    );
    std::string show();
  };

  class Type : public Term {
  public:
    Type();
    std::string show();
  };

  class Block : public Term {
  public:
    std::vector<std::unique_ptr<gram::Node>> body;
    explicit Block(std::vector<std::unique_ptr<gram::Node>> body);
    std::string show();
  };

  class Definition : public Node {
  public:
    std::string name;
    std::unique_ptr<gram::Term> value;
    Definition(std::string name, std::unique_ptr<gram::Term> value);
    std::string show();
  };

  // Parse a stream of tokens.
  // The parser is greedy, meaning it will consume as many tokens as possible.
  // The `end` iterator is passed by reference, so the parser can inform the
  // caller how many tokens were parsed.
  std::unique_ptr<gram::Node> parse(
    const std::string &source,
    std::string source_name,
    std::vector<gram::Token>::iterator begin,
    std::vector<gram::Token>::iterator &end,
    bool top_level=true
  );

}

#endif
