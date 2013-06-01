//===--- PPLexerChange.cpp - Handle changing lexers in the preprocessor ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements pieces of the Preprocessor interface that manage the
// current lexer stack.
//
//===----------------------------------------------------------------------===//

#include "vlang/Lex/Preprocessor.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/SourceManager.h"
#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Lex/MacroInfo.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/PathV2.h"
using namespace vlang;

PPCallbacks::~PPCallbacks() {}

//===----------------------------------------------------------------------===//
// Miscellaneous Methods.
//===----------------------------------------------------------------------===//

/// isInPrimaryFile - Return true if we're in the top-level file, not in a
/// \`include.  This looks through macro expansions
bool Preprocessor::isInPrimaryFile() const {
  if (IsFileLexer())
    return IncludeMacroStack.empty();

  // If there are any stacked lexers, we're in a #include.
  assert(IsFileLexer(IncludeMacroStack[0]) &&
         "Top level include stack isn't our primary lexer?");
  for (unsigned i = 1, e = IncludeMacroStack.size(); i != e; ++i)
    if (IsFileLexer(IncludeMacroStack[i]))
      return false;
  return true;
}

/// getCurrentLexer - Return the current file lexer being lexed from.  Note
/// that this ignores any potentially active macro expansions going on at the time.
PreprocessorLexer *Preprocessor::getCurrentFileLexer() const {
  if (IsFileLexer())
    return CurPPLexer;

  // Look for a stacked lexer.
  for (unsigned i = IncludeMacroStack.size(); i != 0; --i) {
    const IncludeStackInfo& ISI = IncludeMacroStack[i-1];
    if (IsFileLexer(ISI))
      return ISI.ThePPLexer;
  }
  return 0;
}


//===----------------------------------------------------------------------===//
// Methods for Entering and Callbacks for leaving various contexts
//===----------------------------------------------------------------------===//

/// EnterSourceFile - Add a source file to the top of the include stack and
/// start lexing tokens from it instead of the current buffer.
void Preprocessor::EnterSourceFile(FileID FID, const DirectoryLookup *CurDir,
                                   SourceLocation Loc) {
  assert(CurTokenLexer == 0 && "Cannot #include a file inside a macro!");
  ++NumEnteredSourceFiles;

  if (MaxIncludeStackDepth < IncludeMacroStack.size())
    MaxIncludeStackDepth = IncludeMacroStack.size();
  
  // Get the MemoryBuffer for this FID, if it fails, we fail.
  bool Invalid = false;
  const llvm::MemoryBuffer *InputFile = 
    getSourceManager().getBuffer(FID, Loc, &Invalid);
  if (Invalid) {
    SourceLocation FileStart = SourceMgr.getLocForStartOfFile(FID);
    Diag(Loc, diag::err_pp_error_opening_file)
      << std::string(SourceMgr.getBufferName(FileStart)) << "";
    return;
  }

  if (isCodeCompletionEnabled() &&
      SourceMgr.getFileEntryForID(FID) == CodeCompletionFile) {
    CodeCompletionFileLoc = SourceMgr.getLocForStartOfFile(FID);
    CodeCompletionLoc =
        CodeCompletionFileLoc.getLocWithOffset(CodeCompletionOffset);
  }

  EnterSourceFileWithLexer(new Lexer(FID, InputFile, *this), CurDir);
  return;
}

/// EnterSourceFileWithLexer - Add a source file to the top of the include stack
///  and start lexing tokens from it instead of the current buffer.
void Preprocessor::EnterSourceFileWithLexer(Lexer *TheLexer,
                                            const DirectoryLookup *CurDir) {

  // Add the current lexer to the include stack.
  if (CurPPLexer || CurTokenLexer)
    PushIncludeMacroStack();

  CurLexer.reset(TheLexer);
  CurPPLexer = TheLexer;
  CurDirLookup = CurDir;
  CurLexerKind = CLK_Lexer;
  
  // Notify the client, if desired, that we are in a new source file.
  if (Callbacks ) {
    SrcMgr::CharacteristicKind FileType =
       SourceMgr.getFileCharacteristic(CurLexer->getFileLoc());

    Callbacks->FileChanged(CurLexer->getFileLoc(),
                           PPCallbacks::EnterFile, FileType);
  }
}

/// EnterMacro - Add a Macro to the top of the include stack and start lexing
/// tokens from it instead of the current buffer.
void Preprocessor::EnterMacro(Token &Tok, SourceLocation ILEnd,
                              MacroInfo *Macro, MacroArgs *Args) {
  TokenLexer *TokLexer;
  if (NumCachedTokenLexers == 0) {
    TokLexer = new TokenLexer(Tok, ILEnd, Macro, Args, *this);
  } else {
    TokLexer = TokenLexerCache[--NumCachedTokenLexers];
    TokLexer->Init(Tok, ILEnd, Macro, Args);
  }

  PushIncludeMacroStack();
  CurDirLookup = 0;
  CurTokenLexer.reset(TokLexer);
  CurLexerKind = CLK_TokenLexer;
}

/// EnterTokenStream - Add a "macro" context to the top of the include stack,
/// which will cause the lexer to start returning the specified tokens.
///
/// If DisableMacroExpansion is true, tokens lexed from the token stream will
/// not be subject to further macro expansion.  Otherwise, these tokens will
/// be re-macro-expanded when/if expansion is enabled.
///
/// If OwnsTokens is false, this method assumes that the specified stream of
/// tokens has a permanent owner somewhere, so they do not need to be copied.
/// If it is true, it assumes the array of tokens is allocated with new[] and
/// must be freed.
///
void Preprocessor::EnterTokenStream(const Token *Toks, unsigned NumToks,
                                    bool DisableMacroExpansion,
                                    bool OwnsTokens) {
  // Create a macro expander to expand from the specified token stream.
  TokenLexer *TokLexer;
  if (NumCachedTokenLexers == 0) {
    TokLexer = new TokenLexer(Toks, NumToks, DisableMacroExpansion,
                              OwnsTokens, *this);
  } else {
    TokLexer = TokenLexerCache[--NumCachedTokenLexers];
    TokLexer->Init(Toks, NumToks, DisableMacroExpansion, OwnsTokens);
  }

  // Save our current state.
  PushIncludeMacroStack();
  CurDirLookup = 0;
  CurTokenLexer.reset(TokLexer);
  CurLexerKind = CLK_TokenLexer;
}

/// \brief Compute the relative path that names the given file relative to
/// the given directory.
static void computeRelativePath(FileManager &FM, const DirectoryEntry *Dir,
                                const FileEntry *File,
                                SmallString<128> &Result) {
  Result.clear();

  StringRef FilePath = File->getDir()->getName();
  StringRef Path = FilePath;
  while (!Path.empty()) {
    if (const DirectoryEntry *CurDir = FM.getDirectory(Path)) {
      if (CurDir == Dir) {
        Result = FilePath.substr(Path.size());
        llvm::sys::path::append(Result, 
                                llvm::sys::path::filename(File->getName()));
        return;
      }
    }
    
    Path = llvm::sys::path::parent_path(Path);
  }
  
  Result = File->getName();
}

/// HandleEndOfFile - This callback is invoked when the lexer hits the end of
/// the current file.  This either returns the EOF token or pops a level off
/// the include stack and keeps going.
bool Preprocessor::HandleEndOfFile(Token &Result, bool isEndOfMacro) {
  assert(!CurTokenLexer &&
         "Ending a file when currently in a macro!");

  // See if this file had a controlling macro.
  if (CurPPLexer) {  // Not ending a macro, ignore it.
    if (const IdentifierInfo *ControllingMacro =
          CurPPLexer->MIOpt.GetControllingMacroAtEndOfFile()) {
      // Okay, this has a controlling macro, remember in HeaderFileInfo.
      if (const FileEntry *FE =
            SourceMgr.getFileEntryForID(CurPPLexer->getFileID()))
        HeaderInfo.SetFileControllingMacro(FE, ControllingMacro);
    }
  }

  // If this is a `include'd file, pop it off the include stack and continue
  // lexing the `includer file.
  if (!IncludeMacroStack.empty()) {

    // If we lexed the code-completion file, act as if we reached EOF.
    if (isCodeCompletionEnabled() && CurPPLexer &&
        SourceMgr.getLocForStartOfFile(CurPPLexer->getFileID()) ==
            CodeCompletionFileLoc) {
      if (CurLexer) {
        Result.startToken();
        CurLexer->FormTokenWithChars(Result, CurLexer->BufferEnd, tok::eof);
        CurLexer.reset();
      } else {
        assert(0 && "Got EOF but no current lexer set!");
      }

      CurPPLexer = 0;
      return true;
    }

    if (!isEndOfMacro && CurPPLexer &&
        SourceMgr.getIncludeLoc(CurPPLexer->getFileID()).isValid()) {
      // Notify SourceManager to record the number of FileIDs that were created
      // during lexing of the #include'd file.
      unsigned NumFIDs =
          SourceMgr.local_sloc_entry_size() -
          CurPPLexer->getInitialNumSLocEntries() + 1/*`include'd file*/;
      SourceMgr.setNumCreatedFIDsForFileID(CurPPLexer->getFileID(), NumFIDs);
    }

    FileID ExitedFID;
    if (Callbacks && !isEndOfMacro && CurPPLexer)
      ExitedFID = CurPPLexer->getFileID();
    
    // We're done with the `included file.
    RemoveTopOfLexerStack();

    // Notify the client, if desired, that we are in a new source file.
    if (Callbacks && !isEndOfMacro && CurPPLexer) {
      SrcMgr::CharacteristicKind FileType =
        SourceMgr.getFileCharacteristic(CurPPLexer->getSourceLocation());
      Callbacks->FileChanged(CurPPLexer->getSourceLocation(),
                             PPCallbacks::ExitFile, FileType, ExitedFID);
    }

    // Client should lex another token.
    return false;
  }

  // If the file ends with a newline, form the EOF token on the newline itself,
  // rather than "on the line following it", which doesn't exist.  This makes
  // diagnostics relating to the end of file include the last file that the user
  // actually typed, which is goodness.
  if (CurLexer) {
    const char *EndPos = CurLexer->BufferEnd;
    if (EndPos != CurLexer->BufferStart &&
        (EndPos[-1] == '\n' || EndPos[-1] == '\r')) {
      --EndPos;

      // Handle \n\r and \r\n:
      if (EndPos != CurLexer->BufferStart &&
          (EndPos[-1] == '\n' || EndPos[-1] == '\r') &&
          EndPos[-1] != EndPos[0])
        --EndPos;
    }

    Result.startToken();
    CurLexer->BufferPtr = EndPos;
    CurLexer->FormTokenWithChars(Result, EndPos, tok::eof);

    if (isCodeCompletionEnabled()) {
      // Inserting the code-completion point increases the source buffer by 1,
      // but the main FileID was created before inserting the point.
      // Compensate by reducing the EOF location by 1, otherwise the location
      // will point to the next FileID.
      // FIXME: This is hacky, the code-completion point should probably be
      // inserted before the main FileID is created.
      if (CurLexer->getFileLoc() == CodeCompletionFileLoc)
        Result.setLocation(Result.getLocation().getLocWithOffset(-1));
    }

    if (!isIncrementalProcessingEnabled())
      // We're done with lexing.
      CurLexer.reset();
  } else {
    assert(0 && "Got EOF but no current lexer set!");
  }
  
  if (!isIncrementalProcessingEnabled())
    CurPPLexer = 0;

  // This is the end of the top-level file. 'WarnUnusedMacroLocs' has collected
  // all macro locations that we need to warn because they are not used.
  for (WarnUnusedMacroLocsTy::iterator
         I=WarnUnusedMacroLocs.begin(), E=WarnUnusedMacroLocs.end(); I!=E; ++I)
    Diag(*I, diag::pp_macro_not_used);


  return true;
}

/// HandleEndOfTokenLexer - This callback is invoked when the current TokenLexer
/// hits the end of its token stream.
bool Preprocessor::HandleEndOfTokenLexer(Token &Result) {
  assert(CurTokenLexer && !CurPPLexer &&
         "Ending a macro when currently in a #include file!");

  if (!MacroExpandingLexersStack.empty() &&
      MacroExpandingLexersStack.back().first == CurTokenLexer.get())
    removeCachedMacroExpandedTokensOfLastLexer();

  // Delete or cache the now-dead macro expander.
  if (NumCachedTokenLexers == TokenLexerCacheSize)
    CurTokenLexer.reset();
  else
    TokenLexerCache[NumCachedTokenLexers++] = CurTokenLexer.take();

  // Handle this like a #include file being popped off the stack.
  return HandleEndOfFile(Result, true);
}

/// RemoveTopOfLexerStack - Pop the current lexer/macro exp off the top of the
/// lexer stack.  This should only be used in situations where the current
/// state of the top-of-stack lexer is unknown.
void Preprocessor::RemoveTopOfLexerStack() {
  assert(!IncludeMacroStack.empty() && "Ran out of stack entries to load");

  if (CurTokenLexer) {
    // Delete or cache the now-dead macro expander.
    if (NumCachedTokenLexers == TokenLexerCacheSize)
      CurTokenLexer.reset();
    else
      TokenLexerCache[NumCachedTokenLexers++] = CurTokenLexer.take();
  }

  PopIncludeMacroStack();
}
