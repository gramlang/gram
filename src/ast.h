/*
  This header declares the abstract syntax tree.
*/

#ifndef GRAM_AST_H
#define GRAM_AST_H

#include <memory>
#include <string>
#include <vector>

namespace gram {

  class Node {
  public:
    std::shared_ptr<std::string> source_name;
    std::shared_ptr<std::string> source;
    size_t start_pos; // Inclusive
    size_t end_pos; // Exclusive

    virtual ~Node();
    virtual std::string show() = 0;
    virtual std::unique_ptr<Node> clone() = 0;
  };

  class Term : public Node {
  public:
    std::shared_ptr<gram::Term> ascription; // User-provided type
    std::shared_ptr<gram::Term> type; // Computed type

    virtual ~Term();
    std::string show_type_and_ascription();
  };

  class Variable : public Term {
  public:
    std::string name;

    explicit Variable(std::string name);
    std::unique_ptr<Node> clone();
    std::string show();
  };

  class Abstraction : public Term {
  public:
    std::shared_ptr<gram::Variable> variable;
    std::shared_ptr<gram::Term> body;

    Abstraction(
      std::shared_ptr<gram::Variable> variable,
      std::shared_ptr<gram::Term> body
    );
    std::unique_ptr<Node> clone();
    std::string show();
  };

  class ArrowType : public Term {
  public:
    std::shared_ptr<gram::Term> domain;
    std::shared_ptr<gram::Term> codomain;

    ArrowType(
      std::shared_ptr<gram::Term> domain,
      std::shared_ptr<gram::Term> codomain
    );
    std::unique_ptr<Node> clone();
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
    std::unique_ptr<Node> clone();
    std::string show();
  };

  class Block : public Term {
  public:
    std::vector<std::shared_ptr<gram::Node>> body;

    explicit Block(std::vector<std::shared_ptr<gram::Node>> body);
    std::unique_ptr<Node> clone();
    std::string show();
  };

  class Definition : public Node {
  public:
    std::shared_ptr<gram::Variable> variable;
    std::shared_ptr<gram::Term> value;

    Definition(
      std::shared_ptr<gram::Variable> variable,
      std::shared_ptr<gram::Term> value
    );
    std::unique_ptr<Node> clone();
    std::string show();
  };

}

#endif
