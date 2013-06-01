//===--- TokenKinds.h - Enum values for C Token Kinds -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the vlang::TokenKind enum and support functions.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_TOKENKINDS_H
#define LLVM_VLANG_TOKENKINDS_H

namespace vlang {

namespace tok {

/// \brief Provides a simple uniform namespace for tokens from all verilog languages.
enum TokenKind {
#define TOK(X) X,
#include "vlang/Basic/TokenKinds.def"
  NUM_TOKENS
};

/// \brief Determines the name of a token as used within the front end.
///
/// The name of a token will be an internal name (such as "l_square")
/// and should not be used as part of diagnostic messages.
const char *getTokenName(enum TokenKind Kind);

/// \brief Determines the spelling of simple punctuation tokens like
/// '!' or '%', and returns NULL for literal and annotation tokens.
///
/// This routine only retrieves the "simple" spelling of the token,
/// and will not produce any alternative spellings (e.g., a
/// digraph). For the actual spelling of a given Token, use
/// Preprocessor::getSpelling().
const char *getTokenSimpleSpelling(enum TokenKind Kind);

/// \brief Return true if this is a raw identifier or an identifier kind.
inline bool isAnyIdentifier(TokenKind K) {
  return (K == tok::identifier) || (K == tok::raw_identifier);
}

/// \brief Return true if this is a string-literal
inline bool isStringLiteral(TokenKind K) {
  return K == tok::string_literal;
}

/// \brief Return true if this is a "literal" kind, like a numeric
/// constant, string, etc.
inline bool isLiteral(TokenKind K) {
  return K == tok::numeric_constant || K == tok::numeric_constant_xz || isStringLiteral(K);
}

/// \brief Return true if this is any of tok::annot_* kinds.
inline bool isAnnotation(TokenKind K) {
#define ANNOTATION(NAME) \
  if (K == tok::annot_##NAME) \
    return true;
#include "vlang/Basic/TokenKinds.def"
  return false;
}

}  // end namespace tok
}  // end namespace vlang

#endif
