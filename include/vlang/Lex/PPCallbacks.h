//===--- PPCallbacks.h - Callbacks for Preprocessor actions -----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the PPCallbacks interface.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_LEX_PPCALLBACKS_H
#define LLVM_VLANG_LEX_PPCALLBACKS_H

#include "vlang/Diag/DiagnosticIDs.h"
#include "vlang/Basic/SourceLocation.h"
#include "vlang/Lex/DirectoryLookup.h"
#include "llvm/ADT/StringRef.h"
#include <string>

namespace vlang {
  class SourceLocation;
  class Token;
  class IdentifierInfo;
  class MacroDirective;
  class MacroArgs;

/// \brief This interface provides a way to observe the actions of the
/// preprocessor as it does its thing.
///
/// Clients can define their hooks here to implement preprocessor level tools.
class PPCallbacks {
public:
  virtual ~PPCallbacks();

  enum FileChangeReason {
    EnterFile, ExitFile, RenameFile
  };

  /// \brief Callback invoked whenever a source file is entered or exited.
  ///
  /// \param Loc Indicates the new location.
  /// \param PrevFID the file that was exited if \p Reason is ExitFile.
  virtual void FileChanged(SourceLocation Loc, FileChangeReason Reason,
                           SrcMgr::CharacteristicKind FileType,
                           FileID PrevFID = FileID()) {
  }

  /// \brief Callback invoked whenever a source file is skipped as the result
  /// of header guard optimization.
  ///
  /// \param ParentFile The file that \`included the skipped file.
  ///
  /// \param FilenameTok The token in ParentFile that indicates the
  /// skipped file.
  virtual void FileSkipped(const FileEntry &ParentFile,
                           const Token &FilenameTok,
                           SrcMgr::CharacteristicKind FileType) {
  }

  /// \brief Callback invoked whenever an inclusion directive results in a
  /// file-not-found error.
  ///
  /// \param FileName The name of the file being included, as written in the 
  /// source code.
  ///
  /// \param RecoveryPath If this client indicates that it can recover from 
  /// this missing file, the client should set this as an additional header
  /// search patch.
  ///
  /// \returns true to indicate that the preprocessor should attempt to recover
  /// by adding \p RecoveryPath as a header search path.
  virtual bool FileNotFound(StringRef FileName,
                            SmallVectorImpl<char> &RecoveryPath) {
    return false;
  }

  /// \brief Callback invoked whenever an inclusion directive of
  /// any kind (\`include, etc.) has been processed, regardless
  /// of whether the inclusion will actually result in an inclusion.
  ///
  /// \param HashLoc The location of the '#' that starts the inclusion 
  /// directive.
  ///
  /// \param IncludeTok The token that indicates the kind of inclusion 
  /// directive, e.g., 'include'.
  ///
  /// \param FileName The name of the file being included, as written in the 
  /// source code.
  ///
  /// \param IsAngled Whether the file name was enclosed in angle brackets;
  /// otherwise, it was enclosed in quotes.
  ///
  /// \param FilenameRange The character range of the quotes or angle brackets
  /// for the written file name.
  ///
  /// \param File The actual file that may be included by this inclusion 
  /// directive.
  ///
  /// \param SearchPath Contains the search path which was used to find the file
  /// in the file system. If the file was found via an absolute include path,
  /// SearchPath will be empty.
  ///
  /// \param RelativePath The path relative to SearchPath, at which the include
  /// file was found. This is equal to FileName except for framework includes.
  ///
  virtual void InclusionDirective(SourceLocation HashLoc,
                                  const Token &IncludeTok,
                                  StringRef FileName,
                                  bool IsAngled,
                                  CharSourceRange FilenameRange,
                                  const FileEntry *File,
                                  StringRef SearchPath,
                                  StringRef RelativePath) {
  }


  /// \brief Callback invoked when the end of the main file is reached.
  ///
  /// No subsequent callbacks will be made.
  virtual void EndOfMainFile() {
  }


  /// \brief Called by Preprocessor::HandleMacroExpandedIdentifier when a
  /// macro invocation is found.
  virtual void MacroExpands(const Token &MacroNameTok, const MacroDirective *MD,
                            SourceRange Range, const MacroArgs *Args) {
  }

  /// \brief Hook called whenever a macro definition is seen.
  virtual void MacroDefined(const Token &MacroNameTok,
                            const MacroDirective *MD) {
  }

  /// \brief Hook called whenever a macro \#undef is seen.
  ///
  /// MD is released immediately following this callback.
  virtual void MacroUndefined(const Token &MacroNameTok,
                              const MacroDirective *MD) {
  }
  
  /// \brief Hook called whenever the 'defined' operator is seen.
  /// \param MD The MacroDirective if the name was a macro, null otherwise.
  virtual void Defined(const Token &MacroNameTok, const MacroDirective *MD) {
  }
  
  /// \brief Hook called when a source range is skipped.
  /// \param Range The SourceRange that was skipped. The range begins at the
  /// \#if/\#else directive and ends after the \#endif/\#else directive.
  virtual void SourceRangeSkipped(SourceRange Range) {
  }

  /// \brief Hook called whenever an \`elsif is seen.
  /// \param Loc the source location of the directive.
  /// \param ConditionRange The SourceRange of the expression being tested.
  /// \param IfLoc the source location of the \#if/\#ifdef/\#ifndef directive.
  // FIXME: better to pass in a list (or tree!) of Tokens.
  virtual void Elsif(SourceLocation Loc, SourceRange ConditionRange,
                    SourceLocation IfLoc) {
  }

  /// \brief Hook called whenever an \`ifdef is seen.
  /// \param Loc the source location of the directive.
  /// \param MacroNameTok Information on the token being tested.
  /// \param MD The MacroDirective if the name was a macro, null otherwise.
  virtual void Ifdef(SourceLocation Loc, const Token &MacroNameTok,
                     const MacroDirective *MD) {
  }

  /// \brief Hook called whenever an \`ifndef is seen.
  /// \param Loc the source location of the directive.
  /// \param MacroNameTok Information on the token being tested.
  /// \param MD The MacroDirective if the name was a macro, null otherwise.
  virtual void Ifndef(SourceLocation Loc, const Token &MacroNameTok,
                      const MacroDirective *MD) {
  }

  /// \brief Hook called whenever an \`else is seen.
  /// \param Loc the source location of the directive.
  /// \param IfLoc the source location of the \`ifdef/\`ifndef directive.
  virtual void Else(SourceLocation Loc, SourceLocation IfLoc) {
  }

  /// \brief Hook called whenever an \`endif is seen.
  /// \param Loc the source location of the directive.
  /// \param IfLoc the source location of the \`ifdef/\`ifndef directive.
  virtual void Endif(SourceLocation Loc, SourceLocation IfLoc) {
  }
};

/// \brief Simple wrapper class for chaining callbacks.
class PPChainedCallbacks : public PPCallbacks {
  virtual void anchor();
  PPCallbacks *First, *Second;

public:
  PPChainedCallbacks(PPCallbacks *_First, PPCallbacks *_Second)
    : First(_First), Second(_Second) {}
  ~PPChainedCallbacks() {
    delete Second;
    delete First;
  }

  virtual void FileChanged(SourceLocation Loc, FileChangeReason Reason,
                           SrcMgr::CharacteristicKind FileType,
                           FileID PrevFID) {
    First->FileChanged(Loc, Reason, FileType, PrevFID);
    Second->FileChanged(Loc, Reason, FileType, PrevFID);
  }

  virtual void FileSkipped(const FileEntry &ParentFile,
                           const Token &FilenameTok,
                           SrcMgr::CharacteristicKind FileType) {
    First->FileSkipped(ParentFile, FilenameTok, FileType);
    Second->FileSkipped(ParentFile, FilenameTok, FileType);
  }

  virtual bool FileNotFound(StringRef FileName,
                            SmallVectorImpl<char> &RecoveryPath) {
    return First->FileNotFound(FileName, RecoveryPath) ||
           Second->FileNotFound(FileName, RecoveryPath);
  }

  virtual void InclusionDirective(SourceLocation HashLoc,
                                  const Token &IncludeTok,
                                  StringRef FileName,
                                  bool IsAngled,
                                  CharSourceRange FilenameRange,
                                  const FileEntry *File,
                                  StringRef SearchPath,
                                  StringRef RelativePath) {
    First->InclusionDirective(HashLoc, IncludeTok, FileName, IsAngled,
                              FilenameRange, File, SearchPath, RelativePath);
    Second->InclusionDirective(HashLoc, IncludeTok, FileName, IsAngled,
                               FilenameRange, File, SearchPath, RelativePath);
  }


  virtual void EndOfMainFile() {
    First->EndOfMainFile();
    Second->EndOfMainFile();
  }

  virtual void MacroExpands(const Token &MacroNameTok, const MacroDirective *MD,
                            SourceRange Range, const MacroArgs *Args) {
    First->MacroExpands(MacroNameTok, MD, Range, Args);
    Second->MacroExpands(MacroNameTok, MD, Range, Args);
  }

  virtual void MacroDefined(const Token &MacroNameTok, const MacroDirective *MD) {
    First->MacroDefined(MacroNameTok, MD);
    Second->MacroDefined(MacroNameTok, MD);
  }

  virtual void MacroUndefined(const Token &MacroNameTok,
                              const MacroDirective *MD) {
    First->MacroUndefined(MacroNameTok, MD);
    Second->MacroUndefined(MacroNameTok, MD);
  }

  virtual void Defined(const Token &MacroNameTok, const MacroDirective *MD) {
    First->Defined(MacroNameTok, MD);
    Second->Defined(MacroNameTok, MD);
  }

  virtual void SourceRangeSkipped(SourceRange Range) {
    First->SourceRangeSkipped(Range);
    Second->SourceRangeSkipped(Range);
  }

  /// \brief Hook called whenever an \#if is seen.
  virtual void Elsif(SourceLocation Loc, SourceRange ConditionRange,
                    SourceLocation IfLoc) {
    First->Elsif(Loc, ConditionRange, IfLoc);
    Second->Elsif(Loc, ConditionRange, IfLoc);
  }

  /// \brief Hook called whenever an \#ifdef is seen.
  virtual void Ifdef(SourceLocation Loc, const Token &MacroNameTok,
                     const MacroDirective *MD) {
    First->Ifdef(Loc, MacroNameTok, MD);
    Second->Ifdef(Loc, MacroNameTok, MD);
  }

  /// \brief Hook called whenever an \#ifndef is seen.
  virtual void Ifndef(SourceLocation Loc, const Token &MacroNameTok,
                      const MacroDirective *MD) {
    First->Ifndef(Loc, MacroNameTok, MD);
    Second->Ifndef(Loc, MacroNameTok, MD);
  }

  /// \brief Hook called whenever an \#else is seen.
  virtual void Else(SourceLocation Loc, SourceLocation IfLoc) {
    First->Else(Loc, IfLoc);
    Second->Else(Loc, IfLoc);
  }

  /// \brief Hook called whenever an \#endif is seen.
  virtual void Endif(SourceLocation Loc, SourceLocation IfLoc) {
    First->Endif(Loc, IfLoc);
    Second->Endif(Loc, IfLoc);
  }
};

}  // end namespace vlang

#endif
