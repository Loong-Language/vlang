//===--- TokenLexer.cpp - Lex from a token stream -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the TokenLexer interface.
//
//===----------------------------------------------------------------------===//

#include "vlang/Lex/TokenLexer.h"
#include "vlang/Lex/MacroArgs.h"
#include "vlang/Basic/SourceManager.h"
#include "vlang/Lex/LexDiagnostic.h"
#include "vlang/Lex/MacroInfo.h"
#include "vlang/Lex/Preprocessor.h"
#include "llvm/ADT/SmallString.h"
using namespace vlang;


/// Create a TokenLexer for the specified macro with the specified actual
/// arguments.  Note that this ctor takes ownership of the ActualArgs pointer.
void TokenLexer::Init(Token &Tok, SourceLocation ELEnd, MacroInfo *MI,
                      MacroArgs *Actuals) {
  // If the client is reusing a TokenLexer, make sure to free any memory
  // associated with it.
  destroy();

  Macro = MI;
  ActualArgs = Actuals;
  CurToken = 0;

  ExpandLocStart = Tok.getLocation();
  ExpandLocEnd = ELEnd;
  AtStartOfLine = Tok.isAtStartOfLine();
  HasLeadingSpace = Tok.hasLeadingSpace();
  Tokens = &*Macro->tokens_begin();
  OwnsTokens = false;
  DisableMacroExpansion = false;
  NumTokens = Macro->tokens_end()-Macro->tokens_begin();
  MacroExpansionStart = SourceLocation();

  SourceManager &SM = PP.getSourceManager();
  MacroStartSLocOffset = SM.getNextLocalOffset();

  if (NumTokens > 0) {
    assert(Tokens[0].getLocation().isValid());
    assert((Tokens[0].getLocation().isFileID() || Tokens[0].is(tok::comment)) &&
           "Macro defined in macro?");
    assert(ExpandLocStart.isValid());

    // Reserve a source location entry chunk for the length of the macro
    // definition. Tokens that get lexed directly from the definition will
    // have their locations pointing inside this chunk. This is to avoid
    // creating separate source location entries for each token.
    MacroDefStart = SM.getExpansionLoc(Tokens[0].getLocation());
    MacroDefLength = Macro->getDefinitionLength(SM);
    MacroExpansionStart = SM.createExpansionLoc(MacroDefStart,
                                                ExpandLocStart,
                                                ExpandLocEnd,
                                                MacroDefLength);
  }

  // If this is a function-like macro, expand the arguments and change
  // Tokens to point to the expanded tokens.
  if (Macro->isFunctionLike() && Macro->getNumArgs())
    ExpandFunctionArguments();

  // Mark the macro as currently disabled, so that it is not recursively
  // expanded.  The macro must be disabled only after argument pre-expansion of
  // function-like macro arguments occurs.
  Macro->DisableMacro();
}



/// Create a TokenLexer for the specified token stream.  This does not
/// take ownership of the specified token vector.
void TokenLexer::Init(const Token *TokArray, unsigned NumToks,
                      bool disableMacroExpansion, bool ownsTokens) {
  // If the client is reusing a TokenLexer, make sure to free any memory
  // associated with it.
  destroy();

  Macro = 0;
  ActualArgs = 0;
  Tokens = TokArray;
  OwnsTokens = ownsTokens;
  DisableMacroExpansion = disableMacroExpansion;
  NumTokens = NumToks;
  CurToken = 0;
  ExpandLocStart = ExpandLocEnd = SourceLocation();
  AtStartOfLine = false;
  HasLeadingSpace = false;
  MacroExpansionStart = SourceLocation();

  // Set HasLeadingSpace/AtStartOfLine so that the first token will be
  // returned unmodified.
  if (NumToks != 0) {
    AtStartOfLine   = TokArray[0].isAtStartOfLine();
    HasLeadingSpace = TokArray[0].hasLeadingSpace();
  }
}


void TokenLexer::destroy() {
  // If this was a function-like macro that actually uses its arguments, delete
  // the expanded tokens.
  if (OwnsTokens) {
    delete [] Tokens;
    Tokens = 0;
    OwnsTokens = false;
  }

  // TokenLexer owns its formal arguments.
  if (ActualArgs) ActualArgs->destroy(PP);
}

/// Expand the arguments of a function-like macro so that we can quickly
/// return preexpanded tokens from Tokens.
void TokenLexer::ExpandFunctionArguments() {

  SmallVector<Token, 128> ResultToks;

  // Loop through 'Tokens', expanding them into ResultToks.  Keep
  // track of whether we change anything.  If not, no need to keep them.  If so,
  // we install the newly expanded sequence as the new 'Tokens' list.
  bool MadeChange = false;

  // NextTokGetsSpace - When this is true, the next token appended to the
  // output list will get a leading space, regardless of whether it had one to
  // begin with or not.  This is used for placemarker support.
  bool NextTokGetsSpace = false;

  for (unsigned i = 0, e = NumTokens; i != e; ++i) {
    // If we found the stringify operator, get the argument stringified.  The
    // preprocessor already verified that the following token is a macro name
    // when the #define was parsed.
    const Token &CurTok = Tokens[i];
    if (CurTok.is(tok::tick) ) {
      int ArgNo = Macro->getArgumentNum(Tokens[i+1].getIdentifierInfo());
      assert(ArgNo != -1 && "Token following # is not an argument?");

      SourceLocation ExpansionLocStart =
          getExpansionLocForMacroDefLoc(CurTok.getLocation());
      SourceLocation ExpansionLocEnd =
          getExpansionLocForMacroDefLoc(Tokens[i+1].getLocation());

      Token Res;
      if (CurTok.is(tok::tick))  // Stringify
        Res = ActualArgs->getStringifiedArgument(ArgNo, PP,
                                                 ExpansionLocStart,
                                                 ExpansionLocEnd);
      else {
        // 'charify': don't bother caching these.
        Res = MacroArgs::StringifyArgument(ActualArgs->getUnexpArgument(ArgNo),
                                           PP,
                                           ExpansionLocStart,
                                           ExpansionLocEnd);
      }

      // The stringified/charified string leading space flag gets set to match
      // the #/#@ operator.
      if (CurTok.hasLeadingSpace() || NextTokGetsSpace)
        Res.setFlag(Token::LeadingSpace);

      ResultToks.push_back(Res);
      MadeChange = true;
      ++i;  // Skip arg name.
      NextTokGetsSpace = false;
      continue;
    }

    // Otherwise, if this is not an argument token, just add the token to the
    // output buffer.
    IdentifierInfo *II = CurTok.getIdentifierInfo();
    int ArgNo = II ? Macro->getArgumentNum(II) : -1;
    if (ArgNo == -1) {
      // This isn't an argument, just add it.
      ResultToks.push_back(CurTok);

      if (NextTokGetsSpace) {
        ResultToks.back().setFlag(Token::LeadingSpace);
        NextTokGetsSpace = false;
      }
      continue;
    }

    // An argument is expanded somehow, the result is different than the
    // input.
    MadeChange = true;

    // If it is not the LHS/RHS of a ## operator, we must pre-expand the
    // argument and substitute the expanded tokens into the result.  This is
    // C99 6.10.3.1p1.
    const Token *ResultArgToks;

    // Only preexpand the argument if it could possibly need it.  This
    // avoids some work in common cases.
    const Token *ArgTok = ActualArgs->getUnexpArgument(ArgNo);
    if (ActualArgs->ArgNeedsPreexpansion(ArgTok, PP))
      ResultArgToks = &ActualArgs->getPreExpArgument(ArgNo, Macro, PP)[0];
    else
      ResultArgToks = ArgTok;  // Use non-preexpanded tokens.

    // If the arg token expanded into anything, append it.
    if (ResultArgToks->isNot(tok::eof)) {
      unsigned FirstResult = ResultToks.size();
      unsigned NumToks = MacroArgs::getArgLength(ResultArgToks);
      ResultToks.append(ResultArgToks, ResultArgToks+NumToks);

      // If the '##' came from expanding an argument, turn it into 'unknown'
      // to avoid pasting.
      for (unsigned i = FirstResult, e = ResultToks.size(); i != e; ++i) {
        Token &Tok = ResultToks[i];
      }

      if(ExpandLocStart.isValid()) {
        updateLocForMacroArgTokens(CurTok.getLocation(),
                                   ResultToks.begin()+FirstResult,
                                   ResultToks.end());
      }

      // If any tokens were substituted from the argument, the whitespace
      // before the first token should match the whitespace of the arg
      // identifier.
      ResultToks[FirstResult].setFlagValue(Token::LeadingSpace,
                                           CurTok.hasLeadingSpace() ||
                                           NextTokGetsSpace);
      NextTokGetsSpace = false;
    } else {
      // If this is an empty argument, and if there was whitespace before the
      // formal token, make sure the next token gets whitespace before it.
      NextTokGetsSpace = CurTok.hasLeadingSpace();
    }
    continue;

    // Okay, we have a token that is either the LHS or RHS of a paste (##)
    // argument.  It gets substituted as its non-pre-expanded tokens.
    const Token *ArgToks = ActualArgs->getUnexpArgument(ArgNo);
    unsigned NumToks = MacroArgs::getArgLength(ArgToks);
    if (NumToks) {  // Not an empty argument?
      // If this is the GNU ", ## __VA_ARGS__" extension, and we just learned
      // that __VA_ARGS__ expands to multiple tokens, avoid a pasting error when
      // the expander trys to paste ',' with the first token of the __VA_ARGS__
      // expansion.
      ResultToks.append(ArgToks, ArgToks+NumToks);

      // If the '##' came from expanding an argument, turn it into 'unknown'
      // to avoid pasting.
      for (unsigned i = ResultToks.size() - NumToks, e = ResultToks.size();
             i != e; ++i) {
        Token &Tok = ResultToks[i];
      }

      if (ExpandLocStart.isValid()) {
        updateLocForMacroArgTokens(CurTok.getLocation(),
                                   ResultToks.end()-NumToks, ResultToks.end());
      }

      // If this token (the macro argument) was supposed to get leading
      // whitespace, transfer this information onto the first token of the
      // expansion.
      //
      // Do not do this if the paste operator occurs before the macro argument,
      // as in "A ## MACROARG".  In valid code, the first token will get
      // smooshed onto the preceding one anyway (forming AMACROARG).  In
      // assembler-with-cpp mode, invalid pastes are allowed through: in this
      // case, we do not want the extra whitespace to be added.  For example,
      // we want ". ## foo" -> ".foo" not ". foo".
      if ((CurTok.hasLeadingSpace() || NextTokGetsSpace))
        ResultToks[ResultToks.size()-NumToks].setFlag(Token::LeadingSpace);

      NextTokGetsSpace = false;
      continue;
    }

    // If an empty argument is on the LHS or RHS of a paste, the standard (C99
    // 6.10.3.3p2,3) calls for a bunch of placemarker stuff to occur.  We
    // implement this by eating ## operators when a LHS or RHS expands to
    // empty.
    NextTokGetsSpace |= CurTok.hasLeadingSpace();

    // If this is on the RHS of a paste operator, we've already copied the
    // paste operator to the ResultToks list.  Remove it.
    NextTokGetsSpace |= ResultToks.back().hasLeadingSpace();
    ResultToks.pop_back();

    continue;
  }

  // If anything changed, install this as the new Tokens list.
  if (MadeChange) {
    assert(!OwnsTokens && "This would leak if we already own the token list");
    // This is deleted in the dtor.
    NumTokens = ResultToks.size();
    // The tokens will be added to Preprocessor's cache and will be removed
    // when this TokenLexer finishes lexing them.
    Tokens = PP.cacheMacroExpandedTokens(this, ResultToks);

    // The preprocessor cache of macro expanded tokens owns these tokens,not us.
    OwnsTokens = false;
  }
}

/// Lex - Lex and return a token from this macro stream.
///
void TokenLexer::Lex(Token &Tok) {
  // Lexing off the end of the macro, pop this macro off the expansion stack.
  if (isAtEnd()) {
    // If this is a macro (not a token stream), mark the macro enabled now
    // that it is no longer being expanded.
    if (Macro) Macro->EnableMacro();

    // Pop this context off the preprocessors lexer stack and get the next
    // token.  This will delete "this" so remember the PP instance var.
    Preprocessor &PPCache = PP;
    if (PP.HandleEndOfTokenLexer(Tok))
      return;

    // HandleEndOfTokenLexer may not return a token.  If it doesn't, lex
    // whatever is next.
    return PPCache.Lex(Tok);
  }

  SourceManager &SM = PP.getSourceManager();

  // If this is the first token of the expanded result, we inherit spacing
  // properties later.
  bool isFirstToken = CurToken == 0;

  // Get the next token to return.
  Tok = Tokens[CurToken++];

  bool TokenIsFromPaste = false;

  // The token's current location indicate where the token was lexed from.  We
  // need this information to compute the spelling of the token, but any
  // diagnostics for the expanded token should appear as if they came from
  // ExpansionLoc.  Pull this information together into a new SourceLocation
  // that captures all of this.
  if (ExpandLocStart.isValid() &&   // Don't do this for token streams.
      // Check that the token's location was not already set properly.
      SM.isBeforeInSLocAddrSpace(Tok.getLocation(), MacroStartSLocOffset)) {
    SourceLocation instLoc;
    if (Tok.is(tok::comment)) {
      instLoc = SM.createExpansionLoc(Tok.getLocation(),
                                      ExpandLocStart,
                                      ExpandLocEnd,
                                      Tok.getLength());
    } else {
      instLoc = getExpansionLocForMacroDefLoc(Tok.getLocation());
    }

    Tok.setLocation(instLoc);
  }

  // If this is the first token, set the lexical properties of the token to
  // match the lexical properties of the macro identifier.
  if (isFirstToken) {
    Tok.setFlagValue(Token::StartOfLine , AtStartOfLine);
    Tok.setFlagValue(Token::LeadingSpace, HasLeadingSpace);
  }

  // Handle recursive expansion!
  if (!Tok.isAnnotation() && Tok.getIdentifierInfo() != 0) {
    // Change the kind of this identifier to the appropriate token kind, e.g.
    // turning "for" into a keyword.
    IdentifierInfo *II = Tok.getIdentifierInfo();
    Tok.setKind(II->getTokenID());

    if (!DisableMacroExpansion && II->isHandleIdentifierCase())
      PP.HandleIdentifier(Tok);
  }

  // Otherwise, return a normal token.
}

/// isNextTokenLParen - If the next token lexed will pop this macro off the
/// expansion stack, return 2.  If the next unexpanded token is a '(', return
/// 1, otherwise return 0.
unsigned TokenLexer::isNextTokenLParen() const {
  // Out of tokens?
  if (isAtEnd())
    return 2;
  return Tokens[CurToken].is(tok::l_paren);
}

/// isParsingPreprocessorDirective - Return true if we are in the middle of a
/// preprocessor directive.
bool TokenLexer::isParsingPreprocessorDirective() const {
  return Tokens[NumTokens-1].is(tok::eod) && !isAtEnd();
}

/// \brief If \arg loc is a file ID and points inside the current macro
/// definition, returns the appropriate source location pointing at the
/// macro expansion source location entry, otherwise it returns an invalid
/// SourceLocation.
SourceLocation
TokenLexer::getExpansionLocForMacroDefLoc(SourceLocation loc) const {
  assert(ExpandLocStart.isValid() && MacroExpansionStart.isValid() &&
         "Not appropriate for token streams");
  assert(loc.isValid() && loc.isFileID());
  
  SourceManager &SM = PP.getSourceManager();
  assert(SM.isInSLocAddrSpace(loc, MacroDefStart, MacroDefLength) &&
         "Expected loc to come from the macro definition");

  unsigned relativeOffset = 0;
  SM.isInSLocAddrSpace(loc, MacroDefStart, MacroDefLength, &relativeOffset);
  return MacroExpansionStart.getLocWithOffset(relativeOffset);
}

/// \brief Finds the tokens that are consecutive (from the same FileID)
/// creates a single SLocEntry, and assigns SourceLocations to each token that
/// point to that SLocEntry. e.g for
///   assert(foo == bar);
/// There will be a single SLocEntry for the "foo == bar" chunk and locations
/// for the 'foo', '==', 'bar' tokens will point inside that chunk.
///
/// \arg begin_tokens will be updated to a position past all the found
/// consecutive tokens.
static void updateConsecutiveMacroArgTokens(SourceManager &SM,
                                            SourceLocation InstLoc,
                                            Token *&begin_tokens,
                                            Token * end_tokens) {
  assert(begin_tokens < end_tokens);

  SourceLocation FirstLoc = begin_tokens->getLocation();
  SourceLocation CurLoc = FirstLoc;

  // Compare the source location offset of tokens and group together tokens that
  // are close, even if their locations point to different FileIDs. e.g.
  //
  //  |bar    |  foo | cake   |  (3 tokens from 3 consecutive FileIDs)
  //  ^                    ^
  //  |bar       foo   cake|     (one SLocEntry chunk for all tokens)
  //
  // we can perform this "merge" since the token's spelling location depends
  // on the relative offset.

  Token *NextTok = begin_tokens + 1;
  for (; NextTok < end_tokens; ++NextTok) {
    SourceLocation NextLoc = NextTok->getLocation();
    if (CurLoc.isFileID() != NextLoc.isFileID())
      break; // Token from different kind of FileID.

    int RelOffs;
    if (!SM.isInSameSLocAddrSpace(CurLoc, NextLoc, &RelOffs))
      break; // Token from different local/loaded location.
    // Check that token is not before the previous token or more than 50
    // "characters" away.
    if (RelOffs < 0 || RelOffs > 50)
      break;
    CurLoc = NextLoc;
  }

  // For the consecutive tokens, find the length of the SLocEntry to contain
  // all of them.
  Token &LastConsecutiveTok = *(NextTok-1);
  int LastRelOffs = 0;
  SM.isInSameSLocAddrSpace(FirstLoc, LastConsecutiveTok.getLocation(),
                           &LastRelOffs);
  unsigned FullLength = LastRelOffs + LastConsecutiveTok.getLength();

  // Create a macro expansion SLocEntry that will "contain" all of the tokens.
  SourceLocation Expansion =
      SM.createMacroArgExpansionLoc(FirstLoc, InstLoc,FullLength);

  // Change the location of the tokens from the spelling location to the new
  // expanded location.
  for (; begin_tokens < NextTok; ++begin_tokens) {
    Token &Tok = *begin_tokens;
    int RelOffs = 0;
    SM.isInSameSLocAddrSpace(FirstLoc, Tok.getLocation(), &RelOffs);
    Tok.setLocation(Expansion.getLocWithOffset(RelOffs));
  }
}

/// \brief Creates SLocEntries and updates the locations of macro argument
/// tokens to their new expanded locations.
///
/// \param ArgIdDefLoc the location of the macro argument id inside the macro
/// definition.
/// \param Tokens the macro argument tokens to update.
void TokenLexer::updateLocForMacroArgTokens(SourceLocation ArgIdSpellLoc,
                                            Token *begin_tokens,
                                            Token *end_tokens) {
  SourceManager &SM = PP.getSourceManager();

  SourceLocation InstLoc =
      getExpansionLocForMacroDefLoc(ArgIdSpellLoc);
  
  while (begin_tokens < end_tokens) {
    // If there's only one token just create a SLocEntry for it.
    if (end_tokens - begin_tokens == 1) {
      Token &Tok = *begin_tokens;
      Tok.setLocation(SM.createMacroArgExpansionLoc(Tok.getLocation(),
                                                    InstLoc,
                                                    Tok.getLength()));
      return;
    }

    updateConsecutiveMacroArgTokens(SM, InstLoc, begin_tokens, end_tokens);
  }
}
