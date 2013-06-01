//===--- Sema.h - Semantic Analysis & AST Building --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Sema class, which performs semantic analysis and
// builds ASTs.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_SEMA_SEMA_H
#define LLVM_CLANG_SEMA_SEMA_H

#include "llvm/ADT/OwningPtr.h"

namespace llvm {
  template <typename ValueT> struct DenseMapInfo;
  template <typename ValueT, typename ValueInfoT> class DenseSet;
  class SmallBitVector;
}

namespace vlang {
  class LangOptions;
  class Preprocessor;
  class Token;
  class DiagnosticsEngine;
  class SourceManager;
  class CodeCompleteConsumer;
  class Scope;

/// Sema - This implements semantic analysis and AST building for C.
class Sema {
  Sema(const Sema &) LLVM_DELETED_FUNCTION;
  void operator=(const Sema &) LLVM_DELETED_FUNCTION;

public:

  const LangOptions &LangOpts;
  Preprocessor &PP;
  DiagnosticsEngine &Diags;
  SourceManager &SourceMgr;

  /// \brief Flag indicating whether or not to collect detailed statistics.
  bool CollectStats;

  /// \brief Code-completion consumer.
  CodeCompleteConsumer *CodeCompleter;
 
  /// Translation Unit Scope - useful to Objective-C actions that need
  /// to lookup file scope declarations in the "ordinary" C decl namespace.
  /// For example, user-defined classes, built-in "id" type, etc.
  Scope *TUScope;

  llvm::BumpPtrAllocator BumpAlloc;

  /// \brief Cause the active diagnostic on the DiagosticsEngine to be
  /// emitted. This is closely coupled to the SemaDiagnosticBuilder class and
  /// should not be used elsewhere.
  void EmitCurrentDiagnostic(unsigned DiagID);

public:
  Sema(Preprocessor &pp,
       TranslationUnitKind TUKind = TU_Complete,
       CodeCompleteConsumer *CompletionConsumer = 0);
  ~Sema();

  /// \brief Perform initialization that occurs after the parser has been
  /// initialized but before it parses anything.
  void Initialize();

  const LangOptions &getLangOpts() const { return LangOpts; }

  DiagnosticsEngine &getDiagnostics() const { return Diags; }
  SourceManager &getSourceManager() const { return SourceMgr; }
  Preprocessor &getPreprocessor() const { return PP; }

  void PrintStats() const;

  /// \brief Helper class that creates diagnostics with optional
  /// template instantiation stacks.
  ///
  /// This class provides a wrapper around the basic DiagnosticBuilder
  /// class that emits diagnostics. SemaDiagnosticBuilder is
  /// responsible for emitting the diagnostic (as DiagnosticBuilder
  /// does) and, if the diagnostic comes from inside a template
  /// instantiation, printing the template instantiation stack as
  /// well.
  class SemaDiagnosticBuilder : public DiagnosticBuilder {
    Sema &SemaRef;
    unsigned DiagID;

  public:
    SemaDiagnosticBuilder(DiagnosticBuilder &DB, Sema &SemaRef, unsigned DiagID)
      : DiagnosticBuilder(DB), SemaRef(SemaRef), DiagID(DiagID) { }

    ~SemaDiagnosticBuilder() {
      // If we aren't active, there is nothing to do.
      if (!isActive()) return;

      // Otherwise, we need to emit the diagnostic. First flush the underlying
      // DiagnosticBuilder data, and clear the diagnostic builder itself so it
      // won't emit the diagnostic in its own destructor.
      //
      // This seems wasteful, in that as written the DiagnosticBuilder dtor will
      // do its own needless checks to see if the diagnostic needs to be
      // emitted. However, because we take care to ensure that the builder
      // objects never escape, a sufficiently smart compiler will be able to
      // eliminate that code.
      FlushCounts();
      Clear();

      // Dispatch to Sema to emit the diagnostic.
      SemaRef.EmitCurrentDiagnostic(DiagID);
    }
  };

  /// \brief Emit a diagnostic.
  SemaDiagnosticBuilder Diag(SourceLocation Loc, unsigned DiagID) {
    DiagnosticBuilder DB = Diags.Report(Loc, DiagID);
    return SemaDiagnosticBuilder(DB, *this, DiagID);
  }

  /// \brief Emit a partial diagnostic.
  SemaDiagnosticBuilder Diag(SourceLocation Loc, const PartialDiagnostic& PD);

  /// \brief Build a partial diagnostic.
  PartialDiagnostic PDiag(unsigned DiagID = 0); // in SemaInternal.h

  bool findMacroSpelling(SourceLocation &loc, StringRef name);

private:
  /// \brief The parser's current scope.
  ///
  /// The parser maintains this state here.
  Scope *CurScope;

protected:
  friend class Parser;

public:
  /// \brief Retrieve the parser's current scope.
  ///
  /// This routine must only be used when it is certain that semantic analysis
  /// and the parser are in precisely the same context, which is not the case
  /// when, e.g., we are performing any kind of template instantiation.
  /// Therefore, the only safe places to use this scope are in the parser
  /// itself and in routines directly invoked from the parser and *never* from
  /// template substitution or instantiation.
  Scope *getCurScope() const { return CurScope; }


};

}  // end namespace vlang

#endif
