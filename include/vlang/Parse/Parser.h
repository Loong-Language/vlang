//===--- Parser.h - C Language Parser ---------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the Parser interface.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_PARSE_PARSER_H
#define LLVM_VLANG_PARSE_PARSER_H

#include "vlang/Parse/ParserResult.h"
#include "vlang/Basic/OperatorPrecedence.h"
#include "vlang/Basic/Specifiers.h"
#include "vlang/Lex/CodeCompletionHandler.h"
#include "vlang/Lex/Preprocessor.h"
#include "vlang/Sema/Sema.h"
#include "vlang/Sema/Scope.h"
#include <llvm/ADT/OwningPtr.h>
#include <llvm/ADT/SmallVector.h>
#include <stack>

namespace vlang {
  class DiagnosticBuilder;
  class Parser;
  class VersionTuple;

  // TODO: May need to move
  typedef SmallVector<Token, 4> CachedTokens;

/// Parser - This implements a Parser for the C family of languages.  After
/// Parsing units of the grammar, Productions are invoked to handle whatever has
/// been read.
///
class Parser : public CodeCompletionHandler {

  Preprocessor &PP;

  /// Tok - The current token we are Peeking ahead.  All Parsing methods assume
  /// that this is valid.
  Token Tok;

  // PrevTokLocation - The location of the token we Previously
  // consumed. This token is used for diagnostics where we expected to
  // see a token following another token (e.g., the ';' at the end of
  // a statement).
  SourceLocation PrevTokLocation;

  unsigned short ParenCount, BracketCount, BraceCount;
  
  /// Actions - These are the callbacks we invoke as we Parse various constructs
  /// in the file.
  Sema &Actions;

  DiagnosticsEngine &Diags;

  /// ScopeCache - Cache scopes to reduce malloc traffic.
  enum { ScopeCacheSize = 16 };
  unsigned NumCachedScopes;
  Scope *ScopeCache[ScopeCacheSize];

  /// \brief Identifiers which have been declared within a tentative Parse.
  SmallVector<IdentifierInfo *, 8> TentativelyDeclaredIdentifiers;

  OwningPtr<CommentHandler> CommentSemaHandler;

public:
  Parser(Preprocessor &PP, Sema &Actions, bool SkipFunctionBodies);
  ~Parser();

  const LangOptions &getLangOpts() const { return PP.getLangOpts(); }
  Preprocessor &getPreprocessor() const { return PP; }
  Sema &getActions() const { return Actions; }

  const Token &getCurToken() const { return Tok; }
  Scope *getCurScope() const { return Actions.getCurScope(); }

  ExprResult ExprError() { return ExprResult(true); }
  StmtResult StmtError() { return StmtResult(true); }

  ExprResult ExprError(const DiagnosticBuilder &) { return ExprError(); }
  StmtResult StmtError(const DiagnosticBuilder &) { return StmtError(); }

  ExprResult ExprEmpty() { return ExprResult(false); }

  // Parsing methods.

  /// Initialize - Warm up the Parser.
  ///
  void Initialize();

  /// ParseTopLevelDecl - Parse one top-level declaration. Returns true if
  /// the EOF was encountered.
  bool ParseTopLevelDecl();

  /// ConsumeToken - Consume the current 'peek token' and lex the next one.
  /// This does not work with all kinds of tokens: strings and specific other
  /// tokens must be consumed with custom methods below.  This returns the
  /// location of the consumed token.
  SourceLocation ConsumeToken(bool ConsumeCodeCompletionTok = false) {
    assert(!isTokenStringLiteral() && !isTokenParen() && !isTokenBracket() &&
           !isTokenBrace() &&
           "Should consume special tokens with Consume*Token");

    if (!ConsumeCodeCompletionTok && Tok.is(tok::code_completion))
      return handleUnexpectedCodeCompletionToken();

    PrevTokLocation = Tok.getLocation();
    PP.Lex(Tok);
    return PrevTokLocation;
  }

private:
  //===--------------------------------------------------------------------===//
  // Low-Level token Peeking and consumption methods.
  //

  /// isTokenParen - Return true if the cur token is '(' or ')'.
  bool isTokenParen() const {
    return Tok.getKind() == tok::l_paren || Tok.getKind() == tok::r_paren;
  }
  /// isTokenBracket - Return true if the cur token is '[' or ']'.
  bool isTokenBracket() const {
    return Tok.getKind() == tok::l_square || Tok.getKind() == tok::r_square;
  }
  /// isTokenBrace - Return true if the cur token is '{' or '}'.
  bool isTokenBrace() const {
    return Tok.getKind() == tok::l_brace || Tok.getKind() == tok::r_brace;
  }

  /// isTokenStringLiteral - True if this token is a string-literal.
  ///
  bool isTokenStringLiteral() const {
    return tok::isStringLiteral(Tok.getKind());
  }

  /// ConsumeIfMatch - Consumes token and returns true if it matches kind
  ///  otherwise returns false
  bool ConsumeIfMatch(tok::TokenKind kind){
     if( Tok.isNot(kind)){
        return false;
     }

     ConsumeToken();
     return true;
  }

  /// ConsumeAnyToken - Dispatch to the right Consume* method based on the
  /// current token type.  This should only be used in cases where the type of
  /// the token really isn't known, e.g. in error recovery.
  SourceLocation ConsumeAnyToken(bool ConsumeCodeCompletionTok = false) {
    if (isTokenParen())
      return ConsumeParen();
    else if (isTokenBracket())
      return ConsumeBracket();
    else if (isTokenBrace())
      return ConsumeBrace();
    else if (isTokenStringLiteral())
      return ConsumeStringToken();
    else
      return ConsumeToken(ConsumeCodeCompletionTok);
  }

  /// ConsumeParen - This consume method keeps the Paren count up-to-date.
  ///
  SourceLocation ConsumeParen() {
    assert(isTokenParen() && "wrong consume method");
    if (Tok.getKind() == tok::l_paren)
      ++ParenCount;
    else if (ParenCount)
      --ParenCount;       // Don't let unbalanced )'s drive the count negative.
    PrevTokLocation = Tok.getLocation();
    PP.Lex(Tok);
    return PrevTokLocation;
  }

  /// ConsumeBracket - This consume method keeps the bracket count up-to-date.
  ///
  SourceLocation ConsumeBracket() {
    assert(isTokenBracket() && "wrong consume method");
    if (Tok.getKind() == tok::l_square)
      ++BracketCount;
    else if (BracketCount)
      --BracketCount;     // Don't let unbalanced ]'s drive the count negative.

    PrevTokLocation = Tok.getLocation();
    PP.Lex(Tok);
    return PrevTokLocation;
  }

  /// ConsumeBrace - This consume method keeps the brace count up-to-date.
  ///
  SourceLocation ConsumeBrace() {
    assert(isTokenBrace() && "wrong consume method");
    if (Tok.getKind() == tok::l_brace)
      ++BraceCount;
    else if (BraceCount)
      --BraceCount;     // Don't let unbalanced }'s drive the count negative.

    PrevTokLocation = Tok.getLocation();
    PP.Lex(Tok);
    return PrevTokLocation;
  }

  /// ConsumeStringToken - Consume the current 'peek token', lexing a new one
  /// and returning the token kind.  This method is specific to strings, as it
  /// handles string literal concatenation, as Per C99 5.1.1.2, translation
  /// Phase #6.
  SourceLocation ConsumeStringToken() {
    assert(isTokenStringLiteral() &&
           "Should only consume string literals with this method");
    PrevTokLocation = Tok.getLocation();
    PP.Lex(Tok);
    return PrevTokLocation;
  }

  /// \brief Consume the current code-completion token.
  ///
  /// This routine should be called to consume the code-completion token once
  /// a code-completion action has already been invoked.
  SourceLocation ConsumeCodeCompletionToken() {
    assert(Tok.is(tok::code_completion));
    PrevTokLocation = Tok.getLocation();
    PP.Lex(Tok);
    return PrevTokLocation;
  }

  ///\ brief When we are consuming a code-completion token without having
  /// matched specific Position in the grammar, Provide code-completion results
  /// based on context.
  ///
  /// \returns the source location of the code-completion token.
  SourceLocation handleUnexpectedCodeCompletionToken();

  /// \brief Abruptly cut off Parsing; mainly used when we have reached the
  /// code-completion Point.
  void cutOffParsing() {
    PP.setCodeCompletionReached();
    // Cut off Parsing by acting as if we reached the end-of-file.
    Tok.setKind(tok::eof);
  }

  /// GetLookAheadToken - This Peeks ahead N tokens and returns that token
  /// without consuming any tokens.  LookAhead(0) returns 'Tok', LookAhead(1)
  /// returns the token after Tok, etc.
  ///
  /// Note that this differs from the Preprocessor's LookAhead method, because
  /// the Parser always has one token lexed that the Preprocessor doesn't.
  ///
  const Token &GetLookAheadToken(unsigned N) {
    if (N == 0 || Tok.is(tok::eof)) return Tok;
    return PP.LookAhead(N-1);
  }

public:
  /// NextToken - This Peeks ahead one token and returns it without
  /// consuming it.
  const Token &NextToken() {
    return PP.LookAhead(0);
  }

private:

  /// TentativeParsingAction - An object that is used as a kind of "tentative
  /// Parsing transaction". It gets instantiated to mark the token Position and
  /// after the token consumption is done, Commit() or Revert() is called to
  /// either "commit the consumed tokens" or revert to the Previously marked
  /// token Position. Example:
  ///
  ///   TentativeParsingAction TPA(*this);
  ///   ConsumeToken();
  ///   ....
  ///   TPA.Revert();
  ///
  class TentativeParsingAction {
    Parser &P;
    Token PrevTok;
    size_t PrevTentativelyDeclaredIdentifierCount;
    unsigned short PrevParenCount, PrevBracketCount, PrevBraceCount;
    bool isActive;

  public:
    explicit TentativeParsingAction(Parser& p) : P(p) {
      PrevTok = P.Tok;
      PrevTentativelyDeclaredIdentifierCount =
          P.TentativelyDeclaredIdentifiers.size();
      PrevParenCount = P.ParenCount;
      PrevBracketCount = P.BracketCount;
      PrevBraceCount = P.BraceCount;
      P.PP.EnableBacktrackAtThisPos();
      isActive = true;
    }
    void Commit() {
      assert(isActive && "Parsing action was finished!");
      P.TentativelyDeclaredIdentifiers.resize(
          PrevTentativelyDeclaredIdentifierCount);
      P.PP.CommitBacktrackedTokens();
      isActive = false;
    }
    void Revert() {
      assert(isActive && "Parsing action was finished!");
      P.PP.Backtrack();
      P.Tok = PrevTok;
      P.TentativelyDeclaredIdentifiers.resize(
          PrevTentativelyDeclaredIdentifierCount);
      P.ParenCount = PrevParenCount;
      P.BracketCount = PrevBracketCount;
      P.BraceCount = PrevBraceCount;
      isActive = false;
    }
    ~TentativeParsingAction() {
      assert(!isActive && "Forgot to call Commit or Revert!");
    }
  };

  /// ExpectAndConsume - The Parser expects that 'ExpectedTok' is next in the
  /// input.  If so, it is consumed and false is returned.
  ///
  /// If the input is malformed, this emits the specified diagnostic.  Next, if
  /// SkipToTok is specified, it calls SkipUntil(SkipToTok).  Finally, true is
  /// returned.
  bool ExpectAndConsume(tok::TokenKind ExpectedTok, unsigned Diag,
                        const char *DiagMsg = "",
                        tok::TokenKind SkipToTok = tok::unknown);

  /// \brief The Parser expects a semicolon and, if Present, will consume it.
  ///
  /// If the next token is not a semicolon, this emits the specified diagnostic,
  /// or, if there's just some closing-delimiter noise (e.g., ')' or ']') Prior
  /// to the semicolon, consumes that extra token.
  bool ExpectAndConsumeSemi(unsigned DiagID);

  /// \brief The kind of extra semi diagnostic to emit.
  enum ExtraSemiKind {
    OutsideFunction = 0,
    InsideStruct = 1,
    InstanceVariableList = 2,
    AfterMemberFunctionDefinition = 3
  };

  /// \brief Consume any extra semi-colons until the end of the line.
  void ConsumeExtraSemi(ExtraSemiKind Kind);

public:
  //===--------------------------------------------------------------------===//
  // Scope manipulation

  /// ParseScope - Introduces a new scope for Parsing. The kind of
  /// scope is determined by ScopeFlags. Objects of this type should
  /// be created on the stack to coincide with the Position where the
  /// Parser enters the new scope, and this object's constructor will
  /// create that new scope. Similarly, once the object is destroyed
  /// the Parser will exit the scope.
  class ParseScope {
    Parser *Self;
    ParseScope(const ParseScope &) LLVM_DELETED_FUNCTION;
    void operator=(const ParseScope &) LLVM_DELETED_FUNCTION;

  public:
    // ParseScope - Construct a new object to manage a scope in the
    // Parser Self where the new Scope is created with the flags
    // ScopeFlags, but only when ManageScope is true (the default). If
    // ManageScope is false, this object does nothing.
    ParseScope(Parser *Self, unsigned ScopeFlags, bool ManageScope = true)
      : Self(Self) {
      if (ManageScope)
        Self->EnterScope(ScopeFlags);
      else
        this->Self = 0;
    }

    // Exit - Exit the scope associated with this object now, rather
    // than waiting until the object is destroyed.
    void Exit() {
      if (Self) {
        Self->ExitScope();
        Self = 0;
      }
    }

    ~ParseScope() {
      Exit();
    }
  };

  /// EnterScope - Start a new scope.
  void EnterScope(unsigned ScopeFlags);

  /// ExitScope - Pop a scope off the scope stack.
  void ExitScope();

private:
  /// \brief RAII object used to modify the scope flags for the current scope.
  class ParseScopeFlags {
    Scope *CurScope;
    unsigned OldFlags;
    ParseScopeFlags(const ParseScopeFlags &) LLVM_DELETED_FUNCTION;
    void operator=(const ParseScopeFlags &) LLVM_DELETED_FUNCTION;

  public:
    ParseScopeFlags(Parser *Self, unsigned ScopeFlags, bool ManageFlags = true);
    ~ParseScopeFlags();
  };

  //===--------------------------------------------------------------------===//
  // Diagnostic Emission and Error recovery.

public:
  DiagnosticBuilder Diag(SourceLocation Loc, unsigned DiagID);
  DiagnosticBuilder Diag(const Token &Tok, unsigned DiagID);
  DiagnosticBuilder Diag(unsigned DiagID) {
    return Diag(Tok, DiagID);
  }

private:
  void SuggestParentheses(SourceLocation Loc, unsigned DK,
                          SourceRange ParenRange);

public:
  /// SkipUntil - Read tokens until we get to the specified token, then consume
  /// it (unless DontConsume is true).  Because we cannot guarantee that the
  /// token will ever occur, this skips to the next token, or to some likely
  /// good stopping Point.  If StopAtSemi is true, skipping will stop at a ';'
  /// character.
  ///
  /// If SkipUntil finds the specified token, it returns true, otherwise it
  /// returns false.
  bool SkipUntil(tok::TokenKind T, bool StopAtSemi = true,
                 bool DontConsume = false, bool StopAtCodeCompletion = false) {
    return SkipUntil(llvm::makeArrayRef(T), StopAtSemi, DontConsume,
                     StopAtCodeCompletion);
  }
  bool SkipUntil(tok::TokenKind T1, tok::TokenKind T2, bool StopAtSemi = true,
                 bool DontConsume = false, bool StopAtCodeCompletion = false) {
    tok::TokenKind TokArray[] = {T1, T2};
    return SkipUntil(TokArray, StopAtSemi, DontConsume,StopAtCodeCompletion);
  }
  bool SkipUntil(tok::TokenKind T1, tok::TokenKind T2, tok::TokenKind T3,
                 bool StopAtSemi = true, bool DontConsume = false,
                 bool StopAtCodeCompletion = false) {
    tok::TokenKind TokArray[] = {T1, T2, T3};
    return SkipUntil(TokArray, StopAtSemi, DontConsume,StopAtCodeCompletion);
  }
  bool SkipUntil(ArrayRef<tok::TokenKind> Toks, bool StopAtSemi = true,
                 bool DontConsume = false, bool StopAtCodeCompletion = false);

  /// SkipMalformedDecl - Read tokens until we get to some likely good stopping
  /// Point for skipping Past a simple-declaration.
  void SkipMalformedDecl();

private:

  bool ConsumeAndStoreFunctionPrologue(CachedTokens &Toks);
  bool ConsumeAndStoreUntil(tok::TokenKind T1,
                            CachedTokens &Toks,
                            bool StopAtSemi = true,
                            bool ConsumeFinalToken = true) {
    return ConsumeAndStoreUntil(T1, T1, Toks, StopAtSemi, ConsumeFinalToken);
  }
  bool ConsumeAndStoreUntil(tok::TokenKind T1, tok::TokenKind T2,
                            CachedTokens &Toks,
                            bool StopAtSemi = true,
                            bool ConsumeFinalToken = true);

  bool ParseDescription();

  bool ParseDesignElementDeclaration();
  bool ParseUdpDeclaration();
  bool ParsePackageDeclaration();
  bool ParseBindDirective();
  bool ParseConfigDeclaration();

  bool ParseParameterPortList();
  bool ParseParameterPortDeclaration();
  bool ParseListOfPorts();
  bool ParseListOfPortDeclarations();
  bool ParsePortDeclaration();

  // Section A.1.4
  bool ParseElaborationSystemTask();
  bool ParseModuleCommonItem();
  bool ParseModuleItem();
  bool ParseModuleOrGenerateItem();
  bool ParseModuleOrGenerateItemDeclaration();
  bool ParsePortModuleItem();
  bool ParseParameterOverride();
  bool ParseBindTargetScope();
  bool ParseBindTargetInstance();
  bool ParseBindTargetInstanceList();
  bool ParseBindInstantiation();

  // Section A.1.5 - Configuration source text
  // TODO

  // Section A.1.6 - Interface items
  bool ParseInterfaceOrGenerateItem();
  bool ParseExternTfDeclaration();
  bool ParseInterfaceItem();
  bool ParseNonPortInterfaceItem();

  // Section A.1.7 - Program items
  bool ParseProgramItem();
  bool ParseNonPortProgramItem();
  bool ParseProgramGenerateItem();

  // Section A.1.8 - Checker items
  // TODO

  // Section A.1.9 - Class items
  // TODO

  // Section A.1.10 - Constraints
  // TODO

  // Section A.1.11 - Package items
  bool ParsePackageItem();
  bool ParsePackageOrGenerateItemDeclaration();
  bool ParseAnonymousProgram();
  bool ParseAnonymousProgramItem();

  // Section A.2 - Declarations
  // Section A.2.1.1 - module Parameter declarations
  bool ParseSpecparamDeclaration();

  // Section A.2.1.2 - Port Declarations
  bool ParseRefDeclaration();

  // Section A.2.1.3 - Type declarations
  bool ParsePackageImportDeclaration();
  bool ParseImportItem();
  bool ParsePackageExportDeclaration();
  bool ParseGenvarDeclaration();
  bool ParseTypeDeclaration();

  bool ParseDataDeclarationList();
  DeclLifetime ParseLifetime();

  // Section A.2.2 - Declaration data types
  DeclTypeResult ParseDeclarationTypeInfo();

  // Section A.2.2.1 - Net/Variable types
  bool ParseCastingType();
  TypeResult ParseDataType(NetType nType);
  TypeResult ParseDataTypeOrImplicit(NetType nType);
  bool ParseEnumBaseType();
  bool ParseEnumNameDeclaration();
  bool ParseClassScope();
  bool ParseClassType();
  bool ParseIntegerType();
  bool ParseIntegerAtomType();
  bool ParseIntegerVectorType();
  bool ParseNonIntegerType();
  NetType     ParseNetType();
  SigningType ParseSigning();
  bool ParseSimpleType();
  bool ParseStructUnionMember();
  bool ParseStructUnion();
  bool ParseTypeReference();

  // Section A.2.2.2 - Strength
  // TODO

  // Section A.2.2.3 - Delays
  bool ParseDelayValue();
  bool ParseDelay3();
  // TODO

  // Section A.2.3 - Declaration lists
  bool ParseListOfIdentifiers();
  bool ParseListOfDefparamAssignments();
  bool ParseListOfGenvarAssignments();
  bool ParseListOfNetDeclAssignments();
  bool ParseListOfParamAssignments();
  bool ParseListOfSpecparamAssignments();
  bool ParseListOfTypeAssignments();
  bool ParseListOfVariableDeclAssignments();
  bool ParseListOfVirtualInterfaceDecl();

  // Section A.2.4 Declaration assignments
  bool ParseDefparamAssignment();
  bool ParseNetDeclAssignment();
  bool ParseParamAssignment();
  bool ParseSpecparamAssignment();
  bool ParseTypeAssignment();
  bool ParsePulseControlSpecparam();
  bool ParseErrorLimitValue();
  bool ParseRejectLimitValue();
  bool ParseLimitValue();
  bool ParseVariableDeclAssignment();
  bool ParseClassNew();
  bool ParseDynamicArrayNew();

  // Section A.2.5 - Declaration ranges
  DimResult ParseDimension();

  // Section A.2.6 - Function declarations
  bool ParseFunctionPrototype();
  bool ParseDpiImportExport();
  bool ParseDpiSpecString();
  bool ParseDpiFunctionImportProperty();
  bool ParseDpiTaskImportProperty();
  bool ParseDpiFunctionProto();
  bool ParseDpiTaskProto();

  // Section A.2.7 - Task declarations
  bool ParseTaskOrFunctionDeclaration();
  bool ParseTfItemDeclaration();
  bool ParseTfPortList();
  bool ParseTfPortItem();
  bool ParseTfPortDirection();
  bool ParseTfPortDeclaration();
  bool ParseTaskPrototype();

  // Section A.2.8 - Block item declarations
  bool ParseBlockItemDeclaration();
  bool ParseOverloadDeclaration();
  bool ParseOverloadOperator();
  bool ParseOverloadProtoFormals();

  // Section A.2.9 - Interface declarations
  // Section A.2.10 - Assertion declarations
  // Section A.2.11 - Covergroup declarations
  // TODO

  // Section A.3 - Primitive instances
  // Section A.3.1 - Primitive instantiaion and instances
  bool ParseGateInstantiation();
  bool ParseCmosSwitchInstance();
  bool ParseEnableGateInstance();
  bool ParseMosSwitchInstance();
  bool ParseNInputGateInstance();
  bool ParseNOutputGateInstance();
  bool ParsePassSwitchInstance();
  bool ParsePassEnableSwitchInstance();
  bool ParsePullGateInstance();

  // Section A.3.2 0 Primitive strengths
  bool ParsePulldownStrength();
  bool ParsePullupStrength();

  // Section A.3.3 Primitive terminals
  bool ParseEnableTerminal();
  bool ParseInoutTerminal();
  bool ParseInputTerminal();
  bool ParseNcontrolTerminal();
  bool ParseOutputTerminal();
  bool ParsePcontrolTerminal();

  // Section A.3.4
  bool ParseCmosSwitchtype();
  bool ParseEnableGatetype();
  bool ParseMosSwitchtype();
  bool ParseNInputGatetype();
  bool ParseNOutputGatetype();
  bool ParseEnSwitchtype();
  bool ParsePassSwitchtype();

  // TODO

  // Section A.4 - Instantiations
  // Section A.4.1.1 - Module instantiation
  bool ParseModuleInstantiation();
  bool ParseParameterValueAssignment();
  bool ParseListOfParameterAssignments();
  bool ParseOrderedParameterAssignment();
  bool ParseNamedParameterAssignment();
  bool ParseHierarchicalInstance();
  bool ParseNameOfInstance();
  bool ParseListOfPortConnections();

  // Section A.4.1.2 - Interface instantiation
  // Section A.4.1.3 - Program instantiation
  // Section A.4.1.4 - Checker instantiation
  // Section A.4.2 - Generated instantiation
  bool ParseGenerateRegion();

  // Section A.5 - UDP's
  // TODO

  //////////////////////////////////////////
  // Section A.6 - Behavioral statements
  //////////////////////////////////////////
  // Section A.6.1 - Continuous assignments
  bool ParseContinuousAssign();
  bool ParseNetAlias();
  bool ParseListOfAssignments();

  // Section A.6.2 - Procedural blocks and assignments
  bool ParseInitialConstruct();
  bool ParseAlwaysConstruct();
  bool ParseFinalConstruct();
  bool ParseBlockingOrNonblockingAssignment( bool allowBlocking, bool allowNonBlocking );
  bool ParseOperatorAssignment();
  bool ParseAssignmentOperator();

  // Section A.6.3 - Parallel and sequential blocks
  bool ParseActionBlock();
  bool ParseSeqOrParBlock();

  // Section A.6.4 - Statements
  bool ParseStatementOrNull();
  bool ParseStatement();
  bool ParseStatementItem();
  bool ParseFunctionStatement();
  bool ParseFunctionStatementOrNull();

  // Section A.6.5 - Timing controls
  bool ParseProceduralTimingControlStatement();
  bool ParseDelayOrEventControl();
  bool ParseDelayControl();
  bool ParseEventControl();
  bool ParseEventExpression();
  bool ParseProceduralTimingControl();
  bool ParseJumpStatement();
  bool ParseWaitStatement();
  bool ParseEventTrigger();

  // Section A.6.6 - Conditional Statements
  void ParseIfStatement();
  bool ParseUniquePriority();
  void ParseCondPredicate(bool requireParen);

  // Section A.6.7 - Case statement
  void ParseCaseStatement();
  bool ParseCasePatternItem();
  bool ParseCaseInsideItem();
  bool ParseRandcaseStatement();
  bool ParseRandcaseItem();

  // Section A.6.7.1 - Patterns
  // TODO

  // Section A.6.8 - Loopiing
  bool ParseLoopStatement();
  bool ParseForInitizalization();
  bool ParseForVariableDeclaration();
  bool ParseForLoop();
  bool ParseForStepAssignment();
  bool ParseLoopVariables();

  // Section A.6.10 - Assertion statements
  // Section A.6.11 - Clocking block
  // Section A.6.12 - Randsequence
  // TODO

  //////////////////////////////////////
  // Section A.7 - Specify's
  //////////////////////////////////////
  // TODO

  //////////////////////////////////////
  // Section A.8 Expressions
  //////////////////////////////////////
  // Section A.8.1 - Concatenations
  ExprResult ParseConcatenation();
  ExprResult ParseModulePathConcatenation();
  ExprResult ParseModulePathMultipleConcatenation();
  ExprResult ParseStreamingConcatenation();
  ExprResult ParseStreamOperator();
  ExprResult ParseSliceSize();
  ExprResult ParseStreamConcatenation();
  ExprResult ParseStreamExpression();
  ExprResult ParseArrayRangeExpression();
  ExprResult ParseEmptyQueue();
  bool ParseListOfExpressions(ExprVector &Exprs);

  // Section A.8.2 - Subroutine calls
  ExprResult ParseTfCall(bool parse_ident, llvm::StringRef ident);
  ExprResult ParseListOfArguments();
  ExprResult ParseMethodCall();
  ExprResult ParseMethodCallBody();
  ExprResult ParseBuiltInMethodCall();
  ExprResult ParseArrayManipulationCall();
  ExprResult ParseRandomizeCall();
  ExprResult ParseMethodCallRoot();
  ExprResult ParseArrayMethodCall();

  // Section A.8.3 - Expressions
  ExprResult ParseIncOrDecExpression();
  ExprResult ParseConditionalExpression();
  ExprResult ParseExpression( prec::Level minPrec);
  ExprResult ParseRhsExpression(ExprResult LHS, prec::Level minPrec);
  ExprResult ParseTaggedUnionExpression();
  ExprResult ParseInsideExpression();
  ExprResult ParseValueRange();
  ExprResult ParseMintypmaxExpression();
  ExprResult ParsePartSelectRange();
  ExprResult ParseIndexedRange();
  ExprResult ParseGenvarExpression();

  // Section A.8.4 - Primarys
  ExprResult  ParsePrimaryWithUnary();
  ExprResult  ParseClassQualifier();
  ExprResult  ParseTimeLiteral();
  ExprResult  ParseTimeUnit();
  ExprResult  ParseImplicitClassHandle();
  ExprResult  ParseSelectOrRange();
  ExprResult  ParseConstantCast();
  ExprResult  ParseConstantLetExpression();
  ExprResult  ParseCast();

  // Section A.8.5 - Expression left-side values
  ExprResult  ParseLvalue(bool netLvalue);

  // Section A.8.6 - Operators
  bool isUnaryOperator();
  bool isBinaryOperator();
  ExprResult  ParseIncOrDecOperator();
  ExprResult  ParseUnaryModulePathOperator();
  ExprResult  ParseBinaryModulePathOperator();

  // Section A.8.7 - Numbers
  ExprResult  ParseNumber();

  // Section A.8.8 - Strings
  // TODO

  /////////////////////////////////
  // Section A.9 - General
  /////////////////////////////////
  // Section A.9.1 - Attributes
  // Section A.9.2 - Comments
  // TODO

  // Section A.9.3 - Identifiers
  bool ParseIdentifier( llvm::StringRef *ref);
  bool ParseHierarchicalIdentifier();
  bool ParsePackageScope();

private:

  /// Returns true if the next token cannot start an expression.
  bool isNotExpressionStart();

  /// Returns true if the next token would start a Postfix-expression
  /// suffix.
  bool isPostfixExpressionSuffixStart() {
    tok::TokenKind K = Tok.getKind();
    return (K == tok::l_square || K == tok::l_paren ||
            K == tok::period || K == tok::arrow ||
            K == tok::plusplus || K == tok::minusminus);
  }

  typedef SmallVector<Expr*, 20> ExprListTy;
  typedef SmallVector<SourceLocation, 20> CommaLocsTy;

  /// ParseExpressionList - Used for C/C++ (argument-)expression-list.
  bool ParseExpressionList(SmallVectorImpl<Expr*> &Exprs,
                           SmallVectorImpl<SourceLocation> &CommaLocs,
                           void (Sema::*Completer)(Scope *S,
                                                   Expr *Data,
                                                   ArrayRef<Expr *> Args) = 0,
                           Expr *Data = 0);

  /// \brief When in code-completion, skip Parsing of the function/method body
  /// unless the body contains the code-completion Point.
  ///
  /// \returns true if the function body was skipped.
  bool trySkippingFunctionBody();

  /// TPResult - Used as the result value for functions whose Purpose is to
  /// disambiguate C++ constructs by "tentatively Parsing" them.
  /// This is a class instead of a simple enum because the implicit enum-to-bool
  /// conversions may cause subtle bugs.
  class TPResult {
    enum Result {
      TPRTrue,
      TPRFalse,
      TPRAmbiguous,
      TPRError
    };
    Result Res;
    TPResult(Result result) : Res(result) {}
  public:
    static TPResult True() { return TPRTrue; }
    static TPResult False() { return TPRFalse; }
    static TPResult Ambiguous() { return TPRAmbiguous; }
    static TPResult Error() { return TPRError; }

    bool operator==(const TPResult &RHS) const { return Res == RHS.Res; }
    bool operator!=(const TPResult &RHS) const { return Res != RHS.Res; }
  };

  /// \brief Determine whether an identifier has been tentatively declared as a
  /// non-type. Such tentative declarations should not be found to name a type
  /// during a tentative Parse, but also should not be annotated as a non-type.
  bool isTentativelyDeclared(IdentifierInfo *II);

private:

  //===--------------------------------------------------------------------===//
  // Preprocessor code-completion Pass-through
   /*
  virtual void CodeCompleteDirective(bool InConditional);
  virtual void CodeCompleteInConditionalExclusion();
  virtual void CodeCompleteMacroName(bool IsDefinition);
  virtual void CodeCompletePreprocessorExpression();
  virtual void CodeCompleteMacroArgument(IdentifierInfo *Macro,
                                         MacroInfo *MacroInfo,
                                         unsigned ArgumentIndex);
  virtual void CodeCompleteNaturalLanguage();
  */
};

}  // end namespace vlang

#endif
