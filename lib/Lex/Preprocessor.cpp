//===--- Preprocess.cpp - C Language Family Preprocessor Implementation ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Preprocessor interface.
//
//===----------------------------------------------------------------------===//
//
// Options to support:
//   -H       - Print the name of each header file used.
//   -d[DNI] - Dump various things.
//   -fworking-directory - #line's with preprocessor's working dir.
//   -fpreprocessed
//   -dependency-file,-M,-MM,-MF,-MG,-MP,-MT,-MQ,-MD,-MMD
//   -W*
//   -w
//
// Messages to emit:
//   "Multiple include guards may be useful for:\n"
//
//===----------------------------------------------------------------------===//

#include "vlang/Lex/Preprocessor.h"
#include "vlang/Lex/MacroArgs.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/SourceManager.h"
#include "vlang/Lex/CodeCompletionHandler.h"
#include "vlang/Lex/ExternalPreprocessorSource.h"
#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Lex/LiteralSupport.h"
#include "vlang/Lex/MacroInfo.h"
#include "vlang/Lex/PreprocessingRecord.h"
#include "vlang/Lex/PreprocessorOptions.h"
#include "vlang/Lex/ScratchBuffer.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Capacity.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
using namespace vlang;

//===----------------------------------------------------------------------===//
ExternalPreprocessorSource::~ExternalPreprocessorSource() { }

Preprocessor::Preprocessor(IntrusiveRefCntPtr<PreprocessorOptions> PPOpts,
                           DiagnosticsEngine &diags, LangOptions &opts,
                           SourceManager &SM,
                           HeaderSearch &Headers,
                           IdentifierInfoLookup *IILookup, bool OwnsHeaders,
                           bool DelayInitialization, bool IncrProcessing)
    : PPOpts(PPOpts), Diags(&diags), LangOpts(opts),
      FileMgr(Headers.getFileMgr()), SourceMgr(SM), HeaderInfo(Headers),
      ExternalSource(0),
      Identifiers(opts, IILookup), IncrementalProcessing(IncrProcessing),
      CodeComplete(0), CodeCompletionFile(0), CodeCompletionOffset(0),
      CodeCompletionReached(0), SkipMainFilePreamble(0, true), CurPPLexer(0),
      CurDirLookup(0), CurLexerKind(CLK_Lexer), Callbacks(0),
      MacroArgCache(0), Record(0), MIChainHead(0), MICache(0),
      DeserialMIChainHead(0) {
  OwnsHeaderSearch = OwnsHeaders;
  
  ScratchBuf = new ScratchBuffer(SourceMgr);
  
  // Clear stats.
  NumDirectives = NumDefined = NumUndefined = 0;
  NumIf = NumElse = NumEndif = 0;
  NumEnteredSourceFiles = 0;
  NumMacroExpanded = NumFnMacroExpanded = NumBuiltinMacroExpanded = 0;
  NumFastMacroExpanded = NumTokenPaste = NumFastTokenPaste = 0;
  MaxIncludeStackDepth = 0;
  NumSkipped = 0;
  
  // Default to discarding comments.
  KeepComments = false;
  KeepMacroComments = false;
  SuppressIncludeNotFoundError = false;
  
  // Macro expansion is enabled.
  DisableMacroExpansion = false;
  MacroExpansionInDirectivesOverride = false;
  InMacroArgs = false;
  InMacroArgPreExpansion = false;
  NumCachedTokenLexers = 0;
  ParsingIfOrElifDirective = false;
  PreprocessedOutput = false;

  CachedLexPos = 0;

  // We haven't read anything from the external source.
  ReadMacrosFromExternalSource = false;
 
  if (!DelayInitialization) {
    Initialize();
  }
}

Preprocessor::~Preprocessor() {
  assert(BacktrackPositions.empty() && "EnableBacktrack/Backtrack imbalance!");

  while (!IncludeMacroStack.empty()) {
    delete IncludeMacroStack.back().TheLexer;
    delete IncludeMacroStack.back().TheTokenLexer;
    IncludeMacroStack.pop_back();
  }

  // Free any macro definitions.
  for (MacroInfoChain *I = MIChainHead ; I ; I = I->Next)
    I->MI.Destroy();

  // Free any cached macro expanders.
  for (unsigned i = 0, e = NumCachedTokenLexers; i != e; ++i)
    delete TokenLexerCache[i];

  for (DeserializedMacroInfoChain *I = DeserialMIChainHead ; I ; I = I->Next)
    I->MI.Destroy();

  // Free any cached MacroArgs.
  for (MacroArgs *ArgList = MacroArgCache; ArgList; )
    ArgList = ArgList->deallocate();

  // Delete the scratch buffer info.
  delete ScratchBuf;

  // Delete the header search info, if we own it.
  if (OwnsHeaderSearch)
    delete &HeaderInfo;

  delete Callbacks;
}

void Preprocessor::Initialize() {
  SystaskInfo.InitializeTarget();
  SystaskInfo.InitializeSystasks(getIdentifierTable(), getLangOpts());
}

void Preprocessor::DumpToken(const Token &Tok, bool DumpFlags) const {
  llvm::errs() << tok::getTokenName(Tok.getKind()) << " '"
               << getSpelling(Tok) << "'";

  if (!DumpFlags) return;

  llvm::errs() << "\t";
  if (Tok.isAtStartOfLine())
    llvm::errs() << " [StartOfLine]";
  if (Tok.hasLeadingSpace())
    llvm::errs() << " [LeadingSpace]";
  if (Tok.isExpandDisabled())
    llvm::errs() << " [ExpandDisabled]";
  if (Tok.needsCleaning()) {
    const char *Start = SourceMgr.getCharacterData(Tok.getLocation());
    llvm::errs() << " [UnClean='" << StringRef(Start, Tok.getLength())
                 << "']";
  }

  llvm::errs() << "\tLoc=<";
  DumpLocation(Tok.getLocation());
  llvm::errs() << ">";
}

void Preprocessor::DumpLocation(SourceLocation Loc) const {
  Loc.dump(SourceMgr);
}

void Preprocessor::DumpMacro(const MacroInfo &MI) const {
  llvm::errs() << "MACRO: ";
  for (unsigned i = 0, e = MI.getNumTokens(); i != e; ++i) {
    DumpToken(MI.getReplacementToken(i));
    llvm::errs() << "  ";
  }
  llvm::errs() << "\n";
}

void Preprocessor::PrintStats() {
  llvm::errs() << "\n*** Preprocessor Stats:\n";
  llvm::errs() << NumDirectives << " directives found:\n";
  llvm::errs() << "  " << NumDefined << " #define.\n";
  llvm::errs() << "  " << NumUndefined << " #undef.\n";
  llvm::errs() << "  #include/#include_next/#import:\n";
  llvm::errs() << "    " << NumEnteredSourceFiles << " source files entered.\n";
  llvm::errs() << "    " << MaxIncludeStackDepth << " max include stack depth\n";
  llvm::errs() << "  " << NumIf << " #if/#ifndef/#ifdef.\n";
  llvm::errs() << "  " << NumElse << " #else/#elif.\n";
  llvm::errs() << "  " << NumEndif << " #endif.\n";
  llvm::errs() << NumSkipped << " #if/#ifndef#ifdef regions skipped\n";

  llvm::errs() << NumMacroExpanded << "/" << NumFnMacroExpanded << "/"
             << NumBuiltinMacroExpanded << " obj/fn/builtin macros expanded, "
             << NumFastMacroExpanded << " on the fast path.\n";
  llvm::errs() << (NumFastTokenPaste+NumTokenPaste)
             << " token paste (##) operations performed, "
             << NumFastTokenPaste << " on the fast path.\n";

  llvm::errs() << "\nPreprocessor Memory: " << getTotalMemory() << "B total";

  llvm::errs() << "\n  BumpPtr: " << BP.getTotalMemory();
  llvm::errs() << "\n  Macro Expanded Tokens: "
               << llvm::capacity_in_bytes(MacroExpandedTokens);
  llvm::errs() << "\n  Predefines Buffer: " << Predefines.capacity();
  llvm::errs() << "\n  Macros: " << llvm::capacity_in_bytes(Macros);
  llvm::errs() << "\n  Poison Reasons: "
               << llvm::capacity_in_bytes(PoisonReasons);
  llvm::errs() << "\n  Comment Handlers: "
               << llvm::capacity_in_bytes(CommentHandlers) << "\n";
}

Preprocessor::macro_iterator
Preprocessor::macro_begin(bool IncludeExternalMacros) const {
  if (IncludeExternalMacros && ExternalSource &&
      !ReadMacrosFromExternalSource) {
    ReadMacrosFromExternalSource = true;
    ExternalSource->ReadDefinedMacros();
  }

  return Macros.begin();
}

size_t Preprocessor::getTotalMemory() const {
  return BP.getTotalMemory()
    + llvm::capacity_in_bytes(MacroExpandedTokens)
    + Predefines.capacity() /* Predefines buffer. */
    + llvm::capacity_in_bytes(Macros)
    + llvm::capacity_in_bytes(PoisonReasons)
    + llvm::capacity_in_bytes(CommentHandlers);
}

Preprocessor::macro_iterator
Preprocessor::macro_end(bool IncludeExternalMacros) const {
  if (IncludeExternalMacros && ExternalSource &&
      !ReadMacrosFromExternalSource) {
    ReadMacrosFromExternalSource = true;
    ExternalSource->ReadDefinedMacros();
  }

  return Macros.end();
}

/// \brief Compares macro tokens with a specified token value sequence.
static bool MacroDefinitionEquals(const MacroInfo *MI,
                                  ArrayRef<TokenValue> Tokens) {
  return Tokens.size() == MI->getNumTokens() &&
      std::equal(Tokens.begin(), Tokens.end(), MI->tokens_begin());
}

StringRef Preprocessor::getLastMacroWithSpelling(
                                    SourceLocation Loc,
                                    ArrayRef<TokenValue> Tokens) const {
  SourceLocation BestLocation;
  StringRef BestSpelling;
  for (Preprocessor::macro_iterator I = macro_begin(), E = macro_end();
       I != E; ++I) {
    if (!I->second->getMacroInfo()->isObjectLike())
      continue;
    const MacroDirective::DefInfo
      Def = I->second->findDirectiveAtLoc(Loc, SourceMgr);
    if (!Def)
      continue;
    if (!MacroDefinitionEquals(Def.getMacroInfo(), Tokens))
      continue;
    SourceLocation Location = Def.getLocation();
    // Choose the macro defined latest.
    if (BestLocation.isInvalid() ||
        (Location.isValid() &&
         SourceMgr.isBeforeInTranslationUnit(BestLocation, Location))) {
      BestLocation = Location;
      BestSpelling = I->first->getName();
    }
  }
  return BestSpelling;
}

void Preprocessor::recomputeCurLexerKind() {
  if (CurLexer)
    CurLexerKind = CLK_Lexer;
  else if (CurTokenLexer)
    CurLexerKind = CLK_TokenLexer;
  else 
    CurLexerKind = CLK_CachingLexer;
}

bool Preprocessor::SetCodeCompletionPoint(const FileEntry *File,
                                          unsigned CompleteLine,
                                          unsigned CompleteColumn) {
  assert(File);
  assert(CompleteLine && CompleteColumn && "Starts from 1:1");
  assert(!CodeCompletionFile && "Already set");

  using llvm::MemoryBuffer;

  // Load the actual file's contents.
  bool Invalid = false;
  const MemoryBuffer *Buffer = SourceMgr.getMemoryBufferForFile(File, &Invalid);
  if (Invalid)
    return true;

  // Find the byte position of the truncation point.
  const char *Position = Buffer->getBufferStart();
  for (unsigned Line = 1; Line < CompleteLine; ++Line) {
    for (; *Position; ++Position) {
      if (*Position != '\r' && *Position != '\n')
        continue;

      // Eat \r\n or \n\r as a single line.
      if ((Position[1] == '\r' || Position[1] == '\n') &&
          Position[0] != Position[1])
        ++Position;
      ++Position;
      break;
    }
  }

  Position += CompleteColumn - 1;

  // Insert '\0' at the code-completion point.
  if (Position < Buffer->getBufferEnd()) {
    CodeCompletionFile = File;
    CodeCompletionOffset = Position - Buffer->getBufferStart();

    MemoryBuffer *NewBuffer =
        MemoryBuffer::getNewUninitMemBuffer(Buffer->getBufferSize() + 1,
                                            Buffer->getBufferIdentifier());
    char *NewBuf = const_cast<char*>(NewBuffer->getBufferStart());
    char *NewPos = std::copy(Buffer->getBufferStart(), Position, NewBuf);
    *NewPos = '\0';
    std::copy(Position, Buffer->getBufferEnd(), NewPos+1);
    SourceMgr.overrideFileContents(File, NewBuffer);
  }

  return false;
}

void Preprocessor::CodeCompleteNaturalLanguage() {
  if (CodeComplete)
    CodeComplete->CodeCompleteNaturalLanguage();
  setCodeCompletionReached();
}

/// getSpelling - This method is used to get the spelling of a token into a
/// SmallVector. Note that the returned StringRef may not point to the
/// supplied buffer if a copy can be avoided.
StringRef Preprocessor::getSpelling(const Token &Tok,
                                          SmallVectorImpl<char> &Buffer,
                                          bool *Invalid) const {
  // NOTE: this has to be checked *before* testing for an IdentifierInfo.
  if (Tok.isNot(tok::raw_identifier) ) {
    // Try the fast path.
    if (const IdentifierInfo *II = Tok.getIdentifierInfo())
      return II->getName();
  }

  // Resize the buffer if we need to copy into it.
  if (Tok.needsCleaning())
    Buffer.resize(Tok.getLength());

  const char *Ptr = Buffer.data();
  unsigned Len = getSpelling(Tok, Ptr, Invalid);
  return StringRef(Ptr, Len);
}

/// CreateString - Plop the specified string into a scratch buffer and return a
/// location for it.  If specified, the source location provides a source
/// location for the token.
void Preprocessor::CreateString(StringRef Str, Token &Tok,
                                SourceLocation ExpansionLocStart,
                                SourceLocation ExpansionLocEnd) {
  Tok.setLength(Str.size());

  const char *DestPtr;
  SourceLocation Loc = ScratchBuf->getToken(Str.data(), Str.size(), DestPtr);

  if (ExpansionLocStart.isValid())
    Loc = SourceMgr.createExpansionLoc(Loc, ExpansionLocStart,
                                       ExpansionLocEnd, Str.size());
  Tok.setLocation(Loc);

  // If this is a raw identifier or a literal token, set the pointer data.
  if (Tok.is(tok::raw_identifier))
    Tok.setRawIdentifierData(DestPtr);
  else if (Tok.isLiteral())
    Tok.setLiteralData(DestPtr);
}


//===----------------------------------------------------------------------===//
// Preprocessor Initialization Methods
//===----------------------------------------------------------------------===//


/// EnterMainSourceFile - Enter the specified FileID as the main source file,
/// which implicitly adds the builtin defines etc.
void Preprocessor::EnterMainSourceFile() {
  // We do not allow the preprocessor to reenter the main file.  Doing so will
  // cause FileID's to accumulate information from both runs (e.g. #line
  // information) and predefined macros aren't guaranteed to be set properly.
  assert(NumEnteredSourceFiles == 0 && "Cannot reenter the main file!");
  FileID MainFileID = SourceMgr.getMainFileID();

  // If MainFileID is loaded it means we loaded an AST file, no need to enter
  // a main file.
  if (!SourceMgr.isLoadedFileID(MainFileID)) {
    // Enter the main file source buffer.
    EnterSourceFile(MainFileID, 0, SourceLocation());
  
    // If we've been asked to skip bytes in the main file (e.g., as part of a
    // precompiled preamble), do so now.
    if (SkipMainFilePreamble.first > 0)
      CurLexer->SkipBytes(SkipMainFilePreamble.first, 
                          SkipMainFilePreamble.second);
    
    // Tell the header info that the main file was entered.  If the file is later
    // #imported, it won't be re-entered.
    if (const FileEntry *FE = SourceMgr.getFileEntryForID(MainFileID))
      HeaderInfo.IncrementIncludeCount(FE);
  }

  /*
  // Preprocess Predefines to populate the initial preprocessor state.
  llvm::MemoryBuffer *SB =
    llvm::MemoryBuffer::getMemBufferCopy(Predefines, "<built-in>");
  assert(SB && "Cannot create predefined source buffer");
  FileID FID = SourceMgr.createFileIDForMemBuffer(SB);
  assert(!FID.isInvalid() && "Could not create FileID for predefines?");
  setPredefinesFileID(FID);

  // Start parsing the predefines.
  EnterSourceFile(FID, 0, SourceLocation());
  */
}

void Preprocessor::EndSourceFile() {
  // Notify the client that we reached the end of the source file.
  if (Callbacks)
    Callbacks->EndOfMainFile();
}

//===----------------------------------------------------------------------===//
// Lexer Event Handling.
//===----------------------------------------------------------------------===//

static void appendCodePoint(unsigned Codepoint,
                            llvm::SmallVectorImpl<char> &Str) {
  char ResultBuf[4];
  char *ResultPtr = ResultBuf;
  bool Res = llvm::ConvertCodePointToUTF8(Codepoint, ResultPtr);
  (void)Res;
  assert(Res && "Unexpected conversion failure");
  Str.append(ResultBuf, ResultPtr);
}

/// LookUpIdentifierInfo - Given a tok::raw_identifier token, look up the
/// identifier information for the token and install it into the token,
/// updating the token kind accordingly.
IdentifierInfo *Preprocessor::LookUpIdentifierInfo(Token &Identifier) const {
  assert(Identifier.getRawIdentifierData() != 0 && "No raw identifier data!");

  // Look up this token, see if it is a macro, or if it is a language keyword.
  IdentifierInfo *II;
  if (!Identifier.needsCleaning() ) {
    // No cleaning needed, just use the characters from the lexed buffer.
    II = getIdentifierInfo(StringRef(Identifier.getRawIdentifierData(),
                                     Identifier.getLength()));
  } else {
    // Cleaning needed, alloca a buffer, clean into it, then use the buffer.
    SmallString<64> IdentifierBuffer;
    StringRef CleanedStr = getSpelling(Identifier, IdentifierBuffer);

    II = getIdentifierInfo(CleanedStr);
  }

  // Update the token info (identifier info and appropriate token kind).
  Identifier.setIdentifierInfo(II);
  Identifier.setKind(II->getTokenID());

  return II;
}

/// HandleIdentifier - This callback is invoked when the lexer reads an
/// identifier.  This callback looks up the identifier in the map and/or
/// potentially macro expands it or turns it into a named token (like 'for').
///
/// Note that callers of this method are guarded by checking the
/// IdentifierInfo's 'isHandleIdentifierCase' bit.  If this method changes, the
/// IdentifierInfo methods that compute these properties will need to change to
/// match.
void Preprocessor::HandleIdentifier(Token &Identifier) {
  assert(Identifier.getIdentifierInfo() &&
         "Can't handle identifiers without identifier info!");

  IdentifierInfo &II = *Identifier.getIdentifierInfo();
 
}

bool Preprocessor::FinishLexStringLiteral(Token &Result, std::string &String,
                                          const char *DiagnosticTag,
                                          bool AllowMacroExpansion) {
  // We need at least one string literal.
  if (Result.isNot(tok::string_literal)) {
    Diag(Result, diag::err_expected_string_literal)
      << /*Source='in...'*/0 << DiagnosticTag;
    return false;
  }

  // Lex string literal tokens, optionally with macro expansion.
  SmallVector<Token, 4> StrToks;
  do {
    StrToks.push_back(Result);

    if (AllowMacroExpansion)
      Lex(Result);
    else
      LexUnexpandedToken(Result);
  } while (Result.is(tok::string_literal));

  // Concatenate and parse the strings.
  StringLiteralParser Literal(&StrToks[0], StrToks.size(), *this);
  assert(Literal.isAscii() && "Didn't allow wide strings in");

  if (Literal.hadError)
    return false;

  String = Literal.GetString();
  return true;
}

void Preprocessor::addCommentHandler(CommentHandler *Handler) {
  assert(Handler && "NULL comment handler");
  assert(std::find(CommentHandlers.begin(), CommentHandlers.end(), Handler) ==
         CommentHandlers.end() && "Comment handler already registered");
  CommentHandlers.push_back(Handler);
}

void Preprocessor::removeCommentHandler(CommentHandler *Handler) {
  std::vector<CommentHandler *>::iterator Pos
  = std::find(CommentHandlers.begin(), CommentHandlers.end(), Handler);
  assert(Pos != CommentHandlers.end() && "Comment handler not registered");
  CommentHandlers.erase(Pos);
}

bool Preprocessor::HandleComment(Token &result, SourceRange Comment) {
  bool AnyPendingTokens = false;
  for (std::vector<CommentHandler *>::iterator H = CommentHandlers.begin(),
       HEnd = CommentHandlers.end();
       H != HEnd; ++H) {
    if ((*H)->HandleComment(*this, Comment))
      AnyPendingTokens = true;
  }
  if (!AnyPendingTokens || getCommentRetentionState())
    return false;
  Lex(result);
  return true;
}

CommentHandler::~CommentHandler() { }

CodeCompletionHandler::~CodeCompletionHandler() { }

void Preprocessor::createPreprocessingRecord() {
  if (Record)
    return;
  
  Record = new PreprocessingRecord(getSourceManager());
  addPPCallbacks(Record);
}
