//===--- OperatorPrecedence.h - Operator precedence levels ------*- C++ -*-===//
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

#ifndef LLVM_VLANG_OPERATOR_PRECEDENCE_H
#define LLVM_VLANG_OPERATOR_PRECEDENCE_H

#include "vlang/Basic/TokenKinds.h"

namespace vlang {

/// PrecedenceLevels - These are precedences for the binary/ternary
/// operators in the C99 grammar.  These have been named to relate
/// with the C99 grammar productions.  Low precedences numbers bind
/// more weakly than high numbers.
namespace prec {
  enum Level {

	  Unknown         = 0,    // Not binary operator.
	  Concatenation   = 1,    // {} {{}}
	  Assignment      = 2,    // = += -= *= /= %= &= ^= |= <<= >>= <<<= >>>= := :/ <=
	  Implication     = 3,    // -> <->
	  Conditional     = 4,    // ?: (conditional operator)
	  LogicalOr       = 5,    // ||
	  LogicalAnd      = 6,    // &&
	  BitOr           = 7,    // | (binary)
	  BitXor          = 8,    // ^ ~^ ^~ (binary)
	  BitAnd          = 9,    // & (binary)
	  Equality        = 10,   // == != === !== ==? !=?
	  Relational      = 11,   // < <= > >= inside dist
	  Shift           = 12,   // << >> <<< >>>
	  Additive        = 13,   // + - (binary)
	  Multiplicative  = 14,   // * / %
	  Power           = 15,   // **
	  Unary           = 16,   // + - ! ~ & ~& | ~| ^ ~^ ^~ ++ -- (unary)
	  ParenBraceColon = 17    // () [] :: .
  };
}

/// \brief Return the precedence of the specified binary operator token.
prec::Level getBinOpPrecedence(tok::TokenKind Kind);

}  // end namespace vlang

#endif  // LLVM_VLANG_OPERATOR_PRECEDENCE_H
