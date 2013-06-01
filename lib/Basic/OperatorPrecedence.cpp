//===--- Operatorprec.cpp ---------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines and computes precedence levels for binary/ternary operators.
///
//===----------------------------------------------------------------------===//
#include "vlang/Basic/OperatorPrecedence.h"

namespace vlang {

prec::Level getBinOpPrecedence(tok::TokenKind Kind) {
  bool allowAssign = true;
  bool unaryOp = false;
  switch (Kind) {
  default: return prec::Unknown;

  case tok::lessequal:
	  return (allowAssign) ? prec::Assignment : prec::Relational;
	  break;

  case tok::plus:
  case tok::minus:
	  return (unaryOp) ? prec::Unary : prec::Additive;
	  break;

  case tok::amp:
	  return (unaryOp) ? prec::Unary : prec::BitAnd;
	  break;

  case tok::caret:
  case tok::tildecaret:
  case tok::carettilde:
	  return (unaryOp) ? prec::Unary : prec::BitXor;
	  break;

  case tok::pipe:
	  return (unaryOp) ? prec::Unary : prec::BitOr;
	  break;

  case tok::exclaim:
  case tok::tilde:
  case tok::tildeamp:
  case tok::tildepipe:
  case tok::plusplus:
  case tok::minusminus:
	  return prec::Unary;
	  break;

  case tok::starstar:
	  return prec::Power;
	  break;

  case tok::star:
  case tok::slash:
  case tok::percent:
	  return prec::Multiplicative;
	  break;

  case tok::lessless:
  case tok::greatergreater:
  case tok::lesslessless:
  case tok::greatergreatergreater:
	  return prec::Shift;
	  break;

  case tok::less:
  case tok::greater:
  case tok::greaterequal:
  case tok::kw_inside:
  case tok::kw_dist:
	  return prec::Relational;
	  break;

  case tok::equalequal:
  case tok::exclaimequal:
  case tok::equalequalequal:
  case tok::exclaimequalequal:
  case tok::equalequalquestion:
  case tok::exclaimequalquestion:
	  return prec::Equality;
	  break;

  case tok::ampamp:
	  return prec::LogicalAnd;
	  break;

  case tok::pipepipe:
	  return prec::LogicalOr;
	  break;

  case tok::question:
  case tok::colon:
	  return prec::Conditional;
	  break;

  case tok::arrow:
  case tok::lessminusgreater:
	  return prec::Implication;
	  break;

  case tok::equal:
  case tok::plusequal:
  case tok::minusequal:
  case tok::starequal:
  case tok::slashequal:
  case tok::percentequal:
  case tok::caretequal:
  case tok::pipeequal:
  case tok::lesslessequal:
  case tok::greatergreaterequal:
  case tok::lesslesslessequal:
  case tok::greatergreatergreaterequal:
  case tok::colonequal:
  case tok::colonslash:
	  return prec::Assignment;
	  break;

  case tok::l_brace:
  case tok::r_brace:
	  return prec::Assignment;
	  break;
  }
}

}  // namespace vlang
