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
    std::shared_ptr<std::string> source_name;
    std::shared_ptr<std::string> source;
    size_t start_line, start_col, // Zero-indexed, inclusive
      end_line, end_col; // Zero-indexed, exclusive

    virtual ~Node();
    virtual std::string show() = 0;

    // Sets the following members based on a slice of tokens,
    // assumed to be from the same source file:
    // - source_name
    // - source
    // - start_line
    // - start_col
    // - end_line
    // - end_col
    void span_tokens(
      std::vector<gram::Token>::iterator begin,
      std::vector<gram::Token>::iterator end
    );
  };

  class Term : public Node {
  public:
    virtual ~Term();
    virtual std::string show() = 0;
  };

  class Abstraction : public Term {
  public:
    std::string argument_name;
    std::shared_ptr<gram::Term> argument_type;
    std::shared_ptr<gram::Term> body;

    Abstraction(
      std::string argument_name,
      std::shared_ptr<gram::Term> argument_type,
      std::shared_ptr<gram::Term> body
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
    std::shared_ptr<gram::Term> abstraction;
    std::shared_ptr<gram::Term> operand;

    Application(
      std::shared_ptr<gram::Term> abstraction,
      std::shared_ptr<gram::Term> operand
    );
    std::string show();
  };

  class PiType : public Term {
  public:
    std::string argument_name;
    std::shared_ptr<gram::Term> argument_type;
    std::shared_ptr<gram::Term> body;

    PiType(
      std::string argument_name,
      std::shared_ptr<gram::Term> argument_type,
      std::shared_ptr<gram::Term> body
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
    std::vector<std::shared_ptr<gram::Node>> body;

    explicit Block(std::vector<std::shared_ptr<gram::Node>> body);
    std::string show();
  };

  class Definition : public Node {
  public:
    std::string name;
    std::shared_ptr<gram::Term> value;

    Definition(std::string name, std::shared_ptr<gram::Term> value);
    std::string show();
  };

  // Parse a stream of tokens.
  std::shared_ptr<gram::Node> parse(std::vector<gram::Token> &tokens);

}

#endif
