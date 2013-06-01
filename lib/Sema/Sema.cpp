//===--- Sema.cpp - AST Builder and Semantic Analysis Implementation ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the actions class which performs semantic analysis and
// builds an AST out of a parse stream.
//
//===----------------------------------------------------------------------===//

#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/PartialDiagnostic.h"
#include "vlang/Lex/CodeCompletionHandler.h"
#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Lex/Preprocessor.h"
#include "vlang/Sema/Sema.h"
#include <llvm/ADT/SmallSet.h>
#include <llvm/Support/raw_ostream.h>
using namespace vlang;

Sema::Sema(Preprocessor &pp, TranslationUnitKind TUKind,
           CodeCompleteConsumer *CodeCompleter)
  : LangOpts(pp.getLangOpts()), PP(pp),
    Diags(PP.getDiagnostics()), SourceMgr(PP.getSourceManager()),
    CollectStats(false), CodeCompleter(CodeCompleter),
    CurScope(0)
{
  TUScope = 0;
}

void Sema::Initialize() {
}

Sema::~Sema() {

}

/// \brief Print out statistics about the semantic analysis.
void Sema::PrintStats() const {
  llvm::errs() << "\n*** Semantic Analysis Stats:\n";

  BumpAlloc.PrintStats();
}

//===----------------------------------------------------------------------===//
// Helper functions.
//===----------------------------------------------------------------------===//


void Sema::EmitCurrentDiagnostic(unsigned DiagID) {

  // Emit the diagnostic.
  if (!Diags.EmitCurrentDiagnostic())
    return;
}

Sema::SemaDiagnosticBuilder
Sema::Diag(SourceLocation Loc, const PartialDiagnostic& PD) {
  SemaDiagnosticBuilder Builder(Diag(Loc, PD.getDiagID()));
  PD.Emit(Builder);

  return Builder;
}

/// \brief Looks through the macro-expansion chain for the given
/// location, looking for a macro expansion with the given name.
/// If one is found, returns true and sets the location to that
/// expansion loc.
bool Sema::findMacroSpelling(SourceLocation &locref, StringRef name) {
  SourceLocation loc = locref;
  if (!loc.isMacroID()) return false;

  // There's no good way right now to look at the intermediate
  // expansions, so just jump to the expansion location.
  loc = getSourceManager().getExpansionLoc(loc);

  // If that's written with the name, stop here.
  SmallVector<char, 16> buffer;
  if (getPreprocessor().getSpelling(loc, buffer) == name) {
    locref = loc;
    return true;
  }
  return false;
}
