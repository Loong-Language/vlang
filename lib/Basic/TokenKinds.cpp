//===--- TokenKinds.cpp - Token Kinds Support -----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the TokenKind enum and support functions.
//
//===----------------------------------------------------------------------===//

#include "vlang/Basic/TokenKinds.h"
#include <cassert>
using namespace vlang;

static const char * const TokNames[] = {
#define TOK(X) #X,
#define KEYWORD(X,Y) #X,
#include "vlang/Basic/TokenKinds.def"
  0
};

const char *tok::getTokenName(enum TokenKind Kind) {
  assert(Kind < tok::NUM_TOKENS);
  return TokNames[Kind];
}

const char *tok::getTokenSimpleSpelling(enum TokenKind Kind) {
  switch (Kind) {
#define PUNCTUATOR(X,Y) case X: return Y;
#include "vlang/Basic/TokenKinds.def"
  default: break;
  }

  return 0;
}
