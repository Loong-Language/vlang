//===--- Parser.cpp - C Language Family Parser ----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Parser interfaces.
//
//===----------------------------------------------------------------------===//

#include "vlang/Parse/Parser.h"
#include "vlang/Sema/Sema.h"
#include "RAIIObjectsForParser.h"
#include "vlang/Parse/ParseDiagnostic.h"
#include "llvm/Support/raw_ostream.h"
using namespace vlang;

#define UNIMPLEMENTED_PARSE(n) bool Parser::n(){ return false; }

namespace {
/// \brief A comment handler that passes comments found by the preprocessor
/// to the parser action.
class ActionCommentHandler : public CommentHandler {
  Sema &S;

public:
  explicit ActionCommentHandler(Sema &S) : S(S) { }

  virtual bool HandleComment(Preprocessor &PP, SourceRange Comment) {
    return false;
  }
};
} // end anonymous namespace

Parser::Parser(Preprocessor &pp, Sema &actions, bool skipFunctionBodies)
  : PP(pp), Actions(actions), Diags(PP.getDiagnostics()) {
  Tok.startToken();
  Tok.setKind(tok::eof);
  Actions.CurScope = 0;
  NumCachedScopes = 0;
  ParenCount = BracketCount = BraceCount = 0;

  CommentSemaHandler.reset(new ActionCommentHandler(actions));
  PP.addCommentHandler(CommentSemaHandler.get());

  PP.setCodeCompletionHandler(*this);
}

DiagnosticBuilder Parser::Diag(SourceLocation Loc, unsigned DiagID) {
  return Diags.Report(Loc, DiagID);
}

DiagnosticBuilder Parser::Diag(const Token &Tok, unsigned DiagID) {
  return Diag(Tok.getLocation(), DiagID);
}

/// \brief Emits a diagnostic suggesting parentheses surrounding a
/// given range.
///
/// \param Loc The location where we'll emit the diagnostic.
/// \param DK The kind of diagnostic to emit.
/// \param ParenRange Source range enclosing code that should be parenthesized.
void Parser::SuggestParentheses(SourceLocation Loc, unsigned DK,
                                SourceRange ParenRange) {
  SourceLocation EndLoc = PP.getLocForEndOfToken(ParenRange.getEnd());
  if (!ParenRange.getEnd().isFileID() || EndLoc.isInvalid()) {
    // We can't display the parentheses, so just dig the
    // warning/error and return.
    Diag(Loc, DK);
    return;
  }

  Diag(Loc, DK)
    << FixItHint::CreateInsertion(ParenRange.getBegin(), "(")
    << FixItHint::CreateInsertion(EndLoc, ")");
}

static bool IsCommonTypo(tok::TokenKind ExpectedTok, const Token &Tok) {
  switch (ExpectedTok) {
  case tok::semi:
    return Tok.is(tok::colon) || Tok.is(tok::comma); // : or , for ;
  default: return false;
  }
}

/// ExpectAndConsume - The parser expects that 'ExpectedTok' is next in the
/// input.  If so, it is consumed and false is returned.
///
/// If the input is malformed, this emits the specified diagnostic.  Next, if
/// SkipToTok is specified, it calls SkipUntil(SkipToTok).  Finally, true is
/// returned.
bool Parser::ExpectAndConsume(tok::TokenKind ExpectedTok, unsigned DiagID,
                              const char *Msg, tok::TokenKind SkipToTok) {
  if (Tok.is(ExpectedTok) || Tok.is(tok::code_completion)) {
    ConsumeAnyToken();
    return false;
  }

  // Detect common single-character typos and resume.
  if (IsCommonTypo(ExpectedTok, Tok)) {
    SourceLocation Loc = Tok.getLocation();
    Diag(Loc, DiagID)
      << Msg
      << FixItHint::CreateReplacement(SourceRange(Loc),
                                      getTokenSimpleSpelling(ExpectedTok));
    ConsumeAnyToken();

    // Pretend there wasn't a problem.
    return false;
  }

  const char *Spelling = 0;
  SourceLocation EndLoc = PP.getLocForEndOfToken(PrevTokLocation);
  if (EndLoc.isValid() &&
      (Spelling = tok::getTokenSimpleSpelling(ExpectedTok))) {
    // Show what code to insert to fix this problem.
    Diag(EndLoc, DiagID)
      << Msg
      << FixItHint::CreateInsertion(EndLoc, Spelling);
  } else
    Diag(Tok, DiagID) << Msg;

  if (SkipToTok != tok::unknown)
    SkipUntil(SkipToTok);
  return true;
}

bool Parser::ExpectAndConsumeSemi(unsigned DiagID) {
  if (Tok.is(tok::semi) || Tok.is(tok::code_completion)) {
    ConsumeToken();
    return false;
  }
  
  if ((Tok.is(tok::r_paren) || Tok.is(tok::r_square)) && 
      NextToken().is(tok::semi)) {
    Diag(Tok, diag::err_extraneous_token_before_semi)
      << PP.getSpelling(Tok)
      << FixItHint::CreateRemoval(Tok.getLocation());
    ConsumeAnyToken(); // The ')' or ']'.
    ConsumeToken(); // The ';'.
    return false;
  }
  
  return ExpectAndConsume(tok::semi, DiagID);
}

void Parser::ConsumeExtraSemi(ExtraSemiKind Kind, unsigned TST) {
  if (Tok.isNot(tok::semi)) return;

  bool HadMultipleSemis = false;
  SourceLocation StartLoc = Tok.getLocation();
  SourceLocation EndLoc = Tok.getLocation();
  ConsumeToken();

  while ((Tok.is(tok::semi) && !Tok.isAtStartOfLine())) {
    HadMultipleSemis = true;
    EndLoc = Tok.getLocation();
    ConsumeToken();
  }

//  if (Kind != AfterMemberFunctionDefinition || HadMultipleSemis)
//    Diag(StartLoc, diag::ext_extra_semi)
//        << Kind << DeclSpec::getSpecifierName((DeclSpec::TST)TST)
//        << FixItHint::CreateRemoval(SourceRange(StartLoc, EndLoc));
//  else
    // A single semicolon is valid after a member function definition.
//    Diag(StartLoc, diag::warn_extra_semi_after_mem_fn_def)
//      << FixItHint::CreateRemoval(SourceRange(StartLoc, EndLoc));
}

//===----------------------------------------------------------------------===//
// Error recovery.
//===----------------------------------------------------------------------===//

/// SkipUntil - Read tokens until we get to the specified token, then consume
/// it (unless DontConsume is true).  Because we cannot guarantee that the
/// token will ever occur, this skips to the next token, or to some likely
/// good stopping point.  If StopAtSemi is true, skipping will stop at a ';'
/// character.
///
/// If SkipUntil finds the specified token, it returns true, otherwise it
/// returns false.
bool Parser::SkipUntil(ArrayRef<tok::TokenKind> Toks, bool StopAtSemi,
                       bool DontConsume, bool StopAtCodeCompletion) {
  // We always want this function to skip at least one token if the first token
  // isn't T and if not at EOF.
  bool isFirstTokenSkipped = true;
  while (1) {
    // If we found one of the tokens, stop and return true.
    for (unsigned i = 0, NumToks = Toks.size(); i != NumToks; ++i) {
      if (Tok.is(Toks[i])) {
        if (DontConsume) {
          // Noop, don't consume the token.
        } else {
          ConsumeAnyToken();
        }
        return true;
      }
    }

    switch (Tok.getKind()) {
    case tok::eof:
      // Ran out of tokens.
      return false;
        
    case tok::code_completion:
      if (!StopAtCodeCompletion)
        ConsumeToken();
         return false;
       case tok::kw_begin:
          ConsumeToken();
          SkipUntil(tok::kw_end, false, false, StopAtCodeCompletion);
          break;
       case tok::kw_fork:
          ConsumeToken();
          SkipUntil(tok::kw_join, tok::kw_join_any, tok::kw_join_none, false, false, StopAtCodeCompletion);
          break;
       case tok::l_paren:
         // Recursively skip properly-nested parens.
         ConsumeParen();
         SkipUntil(tok::r_paren, false, false, StopAtCodeCompletion);
         break;
       case tok::l_square:
         // Recursively skip properly-nested square brackets.
         ConsumeBracket();
         SkipUntil(tok::r_square, false, false, StopAtCodeCompletion);
         break;
       case tok::l_brace:
         // Recursively skip properly-nested braces.
         ConsumeBrace();
         SkipUntil(tok::r_brace, false, false, StopAtCodeCompletion);
         break;

       // Okay, we found a ']' or '}' or ')', which we think should be balanced.
       // Since the user wasn't looking for this token (if they were, it would
       // already be handled), this isn't balanced.  If there is a LHS token at a
       // higher level, we will assume that this matches the unbalanced token
       // and return it.  Otherwise, this is a spurious RHS token, which we skip.
       case tok::r_paren:
      if (ParenCount && !isFirstTokenSkipped)
        return false;  // Matches something.
      ConsumeParen();
      break;
    case tok::r_square:
      if (BracketCount && !isFirstTokenSkipped)
        return false;  // Matches something.
      ConsumeBracket();
      break;
    case tok::r_brace:
      if (BraceCount && !isFirstTokenSkipped)
        return false;  // Matches something.
      ConsumeBrace();
      break;

    case tok::string_literal:
      ConsumeStringToken();
      break;
        
    case tok::semi:
      if (StopAtSemi)
        return false;
      // FALL THROUGH.
    default:
      // Skip this token.
      ConsumeToken();
      break;
    }
    isFirstTokenSkipped = false;
  }
}

//===----------------------------------------------------------------------===//
// Scope manipulation
//===----------------------------------------------------------------------===//

/// EnterScope - Start a new scope.
void Parser::EnterScope(unsigned ScopeFlags) {
  if (NumCachedScopes) {
    Scope *N = ScopeCache[--NumCachedScopes];
    N->Init(getCurScope(), ScopeFlags);
    Actions.CurScope = N;
  } else {
    Actions.CurScope = new Scope(getCurScope(), ScopeFlags, Diags);
  }
}

/// ExitScope - Pop a scope off the scope stack.
void Parser::ExitScope() {
  assert(getCurScope() && "Scope imbalance!");

  // Inform the actions module that this scope is going away if there are any
  // decls in it.
  //if (!getCurScope()->decl_empty())
  //  Actions.ActOnPopScope(Tok.getLocation(), getCurScope());

  Scope *OldScope = getCurScope();
  //Actions.CurScope = OldScope->getParent();

  if (NumCachedScopes == ScopeCacheSize)
    delete OldScope;
  else
    ScopeCache[NumCachedScopes++] = OldScope;
}

/// Set the flags for the current scope to ScopeFlags. If ManageFlags is false,
/// this object does nothing.
Parser::ParseScopeFlags::ParseScopeFlags(Parser *Self, unsigned ScopeFlags,
                                 bool ManageFlags)
  : CurScope(ManageFlags ? Self->getCurScope() : 0) {
  if (CurScope) {
    OldFlags = CurScope->getFlags();
    CurScope->setFlags(ScopeFlags);
  }
}

/// Restore the flags for the current scope to what they were before this
/// object overrode them.
Parser::ParseScopeFlags::~ParseScopeFlags() {
  if (CurScope)
    CurScope->setFlags(OldFlags);
}


//===----------------------------------------------------------------------===//
// C99 6.9: External Definitions.
//===----------------------------------------------------------------------===//

Parser::~Parser() {
  // If we still have scopes active, delete the scope tree.
  //delete getCurScope();
  Actions.CurScope = 0;
  
  // Free the scope cache.
  for (unsigned i = 0, e = NumCachedScopes; i != e; ++i)
    delete ScopeCache[i];

  PP.clearCodeCompletionHandler();
}

/// Initialize - Warm up the parser.
///
void Parser::Initialize() {
  // Create the translation unit scope.  Install it as the current scope.
  assert(getCurScope() == 0 && "A scope is already active?");
  EnterScope(Scope::DeclScope);
  //Actions.ActOnTranslationUnitScope(getCurScope());

  Actions.Initialize();

  // Prime the lexer look-ahead.
  ConsumeToken();
}

/// ParseTopLevelDecl - Parse one top-level declaration, return whatever the
/// action tells us to.  This returns true if the EOF was encountered.
bool Parser::ParseTopLevelDecl() {
  // Skip over the EOF token, flagging end of previous input for incremental 
  // processing
  if (PP.isIncrementalProcessingEnabled() && Tok.is(tok::eof))
    ConsumeToken();

  //Result = DeclGroupPtrTy();
  if (Tok.is(tok::eof)) {
    //if (!PP.isIncrementalProcessingEnabled())
    //  Actions.ActOnEndOfTranslationUnit();
    //else don't tell Sema that we ended parsing: more input might come.

    return true;
  }

  ParseDescription();
  return false;
}


SourceLocation Parser::handleUnexpectedCodeCompletionToken() {
  assert(Tok.is(tok::code_completion) && "Always should be called with code_completion token");
  PrevTokLocation = Tok.getLocation();

  cutOffParsing();
  return PrevTokLocation;
}

//description ::=
// module_declaration
// | udp_declaration
// | interface_declaration
// | program_declaration
// | package_declaration
// | { attribute_instance } package_item
// | { attribute_instance } bind_directive
// | config_declaration
bool Parser::ParseDescription()
{
   assert(Tok.isNot(tok::eof));

   switch( Tok.getKind()) {
   case tok::kw_program:
   case tok::kw_module:
   case tok::kw_macromodule:
   case tok::kw_interface:
      ParseDesignElementDeclaration();
      break;

   default:
      printf("ERROR: Invalid description item\n");
      //process_error();
      ConsumeToken();
      break;
   }
   return true;
}


// *_nonansi_header         ::= { attribute_instance } * [ lifetime ] module_identifier
//                              { package_import_declaration } [ parameter_port_list ] list_of_ports ;
// *_ansi_header            ::= { attribute_instance } * [ lifetime ] identifier
//                              { package_import_declaration } [ parameter_port_list ] [ list_of_port_declarations ] ;
// module_declaration       ::= *_nonansi_header [ timeunits_declaration ] { *_item }
//                              end* [ : identifier ]
//                            | *_ansi_header [ timeunits_declaration ] { non_port_*_item }
//                              end* [ : identifier ]
//                            | extern *_nonansi_header
//                            | extern *_ansi_header
// module_declaration    +=   ::= { attribute_instance } module_keyword [ lifetime ] module_identifier ( .* ) ;
//                                [ timeunits_declaration ] { module_item } endmodule [ : module_identifier ]
// interface_declaration +=   ::= { attribute_instance } interface interface_identifier ( .* ) ;
//                                [ timeunits_declaration ] { interface_item }
//                                endinterface [ : interface_identifier ]
// program_declaration   +=   ::= { attribute_instance } program program_identifier ( .* ) ;
//                                [ timeunits_declaration ] { program_item }
//                                endprogram [ : program_identifier ]
// module_keyword             ::= module | macromodule
bool Parser::ParseDesignElementDeclaration()
{
   llvm::StringRef module_name;
   llvm::StringRef module_end_name;
   bool isInterface = false;

   // TODO: Handle extern's

   // Find the design type being declared
   DesignType type;
   switch(Tok.getKind()) {
   case tok::kw_module:
   case tok::kw_macromodule:
      type = DesignType::Module;
      break;
   case tok::kw_interface:
      type = DesignType::Interface;
      break;
   case tok::kw_program:
      type = DesignType::Program;
      break;
   default:
      assert(0 && "Unknown design element type");
      return false;
      break;
   }
   ConsumeToken();

   // Check for optional lifetime
   DeclLifetime lifetime = DeclLifetime::Unknown;
   if( Tok.is(tok::kw_static) ) {
      ConsumeToken();
      lifetime = DeclLifetime::Static;
   } else if ( Tok.is(tok::kw_automatic)){
      ConsumeToken();
      lifetime = DeclLifetime::Automatic;
   }

   // Check for name of module
   if( !ParseIdentifier( &module_name ) ) {
      Diag(Tok, diag::err_expected_ident_for) << "Module";

      // Skip until semicolon or hit "#" or "(", or ";"
      SkipUntil(tok::hash, tok::l_paren, true);
   }

   // Call semantic analysis of design declaration start
   //	if( !actions->ActOnDesignDeclaration(module_name, type, lifetime ) ) {
   // TODO: Support error handling if needed
   //	}

   // TODO: support package_import_list
   //if( ParsePackageImportList() ) {
   //}

   // Parse parameters
   if( Tok.is(tok::hash) ) {
      ParseParameterPortList();
   }

   // Check for port list, either ansi or nonansi versions
   if( Tok.is(tok::l_paren) ) {
      // Determine if it is an ansi or nonansi header
      // TODO: This doesn't work for use defined interfaces
      auto nxtTok = NextToken();
      if( nxtTok.is(tok::identifier) ) {
         ParseListOfPorts();
      } else {
         ParseListOfPortDeclarations();
      }
   }

   // Module header should always end with a semicolon
   ExpectAndConsumeSemi(diag::err_expected_semi_after_decl);

   // Process the body of the module
   while( ParseModuleItem() ) { }

   switch(Tok.getKind()){
   case tok::kw_endmodule:
   case tok::kw_endinterface:
   case tok::kw_endprogram:
      ConsumeToken();
//      if( type != DesignType::Module){
//         Diag(Tok, diag::err_expected_end_design) <<  "module"
//            << FixItHint::CreateReplacement(ExpectedLoc, tok::getTokenSimpleSpelling(tok::kw_endmodule));
//      }
      break;
   default:
//      Diag(Tok, diag::err_expected_end_design) <<  "program"
//         << FixItHint::CreateInsertion(ExpectedLoc, tok::getTokenSimpleSpelling(tok::kw_endprogram));
      break;
   }

   if( ConsumeIfMatch( tok::colon ) ) {
      if( Tok.isNot(tok::identifier)) {
         Diag(Tok, diag::err_expected_matching_ident);
      } else {
         ParseIdentifier(&module_end_name);
         // TODO: Check it matches start name
      }
   }
   return true;
}
UNIMPLEMENTED_PARSE(ParseUdpDeclaration)
UNIMPLEMENTED_PARSE(ParsePackageDeclaration)
UNIMPLEMENTED_PARSE(ParseBindDirective)
UNIMPLEMENTED_PARSE(ParseConfigDeclaration)

// parameter_port_list ::=
//   # ( list_of_param_assignments { , parameter_port_declaration } )
// | # ( parameter_port_declaration { , parameter_port_declaration } )
// | #( )
bool Parser::ParseParameterPortList()
{
   assert(Tok.is(tok::hash) && "");
   ConsumeToken();

   if (ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "parameter port list")){
      SkipUntil(tok::r_paren, true);
      return false;
   }

   // TODO
   //   if( ParseListOfParamAssignments() ) {
   //      expect_comma = true;
   //   }
   do{
      if( !ParseParameterPortDeclaration()){
         break;
      }
   } while (ConsumeIfMatch(tok::comma));

   if (ExpectAndConsume(tok::r_paren, diag::err_expected_rparen)){
      SkipUntil(tok::r_paren, true);
      return false;
   }

   return true;
}

// parameter_port_declaration ::= parameter_declaration
//                              | local_parameter_declaration
//                              | data_type list_of_param_assignments
//                              | type list_of_type_assignments
UNIMPLEMENTED_PARSE(ParseParameterPortDeclaration)

// list_of_ports   ::= ( port { , port } )
// port            ::= [ port_expression ]
//                   |  . port_identifier ( [ port_expression ] )
// port_expression ::= port_reference
//                   | { port_reference { , port_reference } }
// port_reference  ::= port_identifier constant_select
bool Parser::ParseListOfPorts()
{
   llvm::StringRef ident;
   assert(Tok.is(tok::l_paren) && "");
   ConsumeParen();

   do{

      switch( Tok.getKind()){
      default:
         break;
      // TODO: Handle port expression correctly
      case tok::identifier:
         ParseIdentifier(&ident);
         break;

      case tok::period:
         ConsumeToken();
         if( Tok.isNot(tok::identifier) ) {
            Diag(Tok, diag::err_expected_ident_for) << "port";
            // TODO: Handle error
         }
         ParseIdentifier(&ident);

         if( !ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "port identifier", tok::comma) ){
            break;
         }

         // Concatenated port expression
         if( Tok.is(tok::r_brace) ) {
            Diag(Tok, diag::err_unsupported_feature) << "Concatenated port expression";
            SkipUntil(tok::r_brace, tok::comma, true, false);
         }
         // Parse
         ParseIdentifier(&ident);
         if( Tok.is(tok::l_square)) {
            auto result = ParseSelectOrRange();
         }

         ExpectAndConsume(tok::r_paren, diag::err_expected_rparen, "", tok::comma);
         break;
      }
   } while( ConsumeIfMatch(tok::comma) );

   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen, "", tok::semi);
   return true;
}

// list_of_port_declarations ::=  ( [ { attribute_instance} ansi_port_declaration { , { attribute_instance} ansi_port_declaration } ] )
// ansi_port_declaration     ::= [ port_direction ] [ net_port_type]                   port_identifier { unpacked_dimension } [ = constant_expression ]
//                             | interface_identifier [ . modport_identifier ]         port_identifier { unpacked_dimension } [ = constant_expression ]
//                             | interface [ . modport_identifier ]                    port_identifier { unpacked_dimension } [ = constant_expression ]
//                             | [ port_direction ] variable_port_type                 port_identifier { variable_dimension } [ = constant_expression ]
//                             | [ port_direction ] .                                  port_identifier ( [ expression ] )
// port_direction       ::= input | output | inout | ref
bool Parser::ParseListOfPortDeclarations()
{
   assert(Tok.is(tok::l_paren) && "");
   ConsumeParen();

   DimensionInfo *dimension = nullptr;
   DimensionKind kind = DimensionKind::Unknown;
   ExprResult *lhs = nullptr;
   ExprResult *rhs = nullptr;

   llvm::StringRef ident;
   DataType dataType = DataType::Unknown;
   PortKind portKind = PortKind::Unknown;

   do {
      bool allowAssignment      = true;
      bool allowParenExpression = false;
      switch(Tok.getKind()) {
      case tok::kw_input:
         portKind = PortKind::Input;
         ConsumeToken();
         break;
      case tok::kw_output:
         portKind = PortKind::Output;
         ConsumeToken();
         break;
      case tok::kw_inout:
         portKind = PortKind::Inout;
         ConsumeToken();
         break;
      case tok::kw_ref:
         portKind = PortKind::Ref;
         ConsumeToken();
         break;
      case tok::kw_interface:
         Diag(Tok, diag::err_unsupported_feature) << "interface";
         SkipUntil(tok::comma);
         break;

      case tok::period:
         ConsumeToken();
         allowAssignment       = false;
         allowParenExpression  = true;
         break;

         // End of empty port declaration
      case tok::r_paren:
         continue;
         break;

         // Empty port entry, go to while evaluation to consume comma
      case tok::comma:
         continue;
         break;

         // Typedef variable
      case tok::kw_var:
         Diag(Tok, diag::err_unsupported_feature) << "Typedef ports";
         SkipUntil(tok::comma);
         continue;

         // Bare identifier
      case tok::identifier:
         break;
      default:
         assert(0 && "Not Implemented\n");
         break;
      }

      // Check for implicit data type
      //dataType = ParseNetType();
      auto decl_info = ParseDeclarationTypeInfo();

      // This should never happen
      assert(decl_info.isUsable() && "Declaration Type Info is null\n");

      // Check for port_identifier
      llvm::StringRef ident;
      if( Tok.isNot(tok::identifier)){
         Diag(Tok, diag::err_expected_ident_for) << "port";
      }
      ParseIdentifier(&ident);

      // Check if a dimension is specified
      if( Tok.is(tok::l_square) ){
         ParseDimension();
      }

      // Check for assignment
      if( ConsumeIfMatch(tok::equal) ) {
         if( !allowAssignment ) {
            //printf("ERROR: Assignments are not allowed for this kind of port declaration\n");
            //process_error();
         }

         auto result = ParseExpression(prec::Assignment);
      }

      // Check for ( [ expression ] )
      if( Tok.is(tok::l_paren) ) {
         ConsumeParen();
         if( !allowParenExpression ) {
            //printf("ERROR: '(' is not allowed for this kind of port declaration\n");
            //process_error();
         }

         auto result = ParseExpression(prec::Assignment);
         if(  result.isInvalid() ) {
            //printf("ERROR: Expected expression\n");
            //process_error();
         }

         if( Tok.isNot(tok::r_paren)){
            Diag(Tok, diag::err_expected_rparen);
            SkipUntil(tok::comma, tok::r_paren, true, true);
            if(Tok.isNot(tok::r_paren)){
               continue;
            }
         }
         ConsumeParen();
      }

      // If we don't have a ',' or ')', try to search for next one
      if( Tok.isNot(tok::comma) && Tok.isNot(tok::r_paren)){
         Diag(Tok, diag::err_expected_lparen_or_comma);
         SkipUntil(tok::comma, tok::r_paren, true, true);
      }
   } while( ConsumeIfMatch( tok::comma ) );

   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen, "", tok::r_paren);
   return true;
}

// Section A.1.4
bool Parser::ParseElaborationSystemTask()
{
   return false;
}

// module_common_item ::=
//   module_or_generate_item_declaration
// | interface_instantiation -- TODO
// | program_instantiation -- TODO
// | assertion_item -- TODO
// | bind_directive -- TODO
// | continuous_assign
// | net_alias
// | initial_construct
// | final_construct
// | always_construct
// | loop_generate_construct -- TODO
// | conditional_generate_construct -- TODO
// | elaboration_system_task -- TODO
bool Parser::ParseModuleCommonItem()
{
   switch(Tok.getKind()){
   case tok::kw_assign:
      ParseContinuousAssign();
      break;
   case tok::kw_initial:
      ParseInitialConstruct();
      break;
   case tok::kw_final:
      ParseFinalConstruct();
      break;
   case tok::kw_always:
   case tok::kw_always_comb:
   case tok::kw_always_ff:
   case tok::kw_always_latch:
      ParseAlwaysConstruct();
      break;
   case tok::kw_alias:
      ParseNetAlias();
      break;
   default:
      if( !ParseModuleOrGenerateItemDeclaration() ) {
         return false;
      }
      break;
   }

   return true;
}

// module_item ::=
//   port_declaration ;
// | non_port_module_item
// non_port_module_item ::=
//   generate_region
// | module_or_generate_item
// | specify_block -- TODO
// | { attribute_instance } specparam_declaration
// | program_declaration   -- TODO
// | module_declaration    -- TODO
// | interface_declaration -- TODO
// | timeunits_declaration -- TODO

bool Parser::ParseModuleItem()
{
   if( Tok.is(tok::kw_generate) ) {
      ParseGenerateRegion();
   }else if( ParsePortDeclaration(  ) ) {
      return true;
   } else if ( ParseModuleOrGenerateItem() ) {
      return true;
   } else if ( ParseSpecparamDeclaration() ) {
      return true;
   }
   return false;
}

// module_or_generate_item ::=
// { attribute_instance } parameter_override
// | { attribute_instance } gate_instantiation -- TODO
// | { attribute_instance } udp_instantiation -- TODO
// | { attribute_instance } module_instantiation
// | { attribute_instance } module_common_item
bool Parser::ParseModuleOrGenerateItem()
{
   if( ParseParameterOverride() ) {
      return true;
   } else if ( ParseModuleInstantiation() ) {
      return true;
   } else if ( ParseModuleCommonItem() ) {
      return true;
   } else if ( ParseGateInstantiation() ) {
      return true;
   }
   return false;
}

// module_or_generate_item_declaration ::=
//   package_or_generate_item_declaration
// | genvar list_of_identifiers ;
// | clocking_declaration -- TODO
// | default clocking clocking_identifier ; -- TODO
bool Parser::ParseModuleOrGenerateItemDeclaration()
{
   if( ParsePackageOrGenerateItemDeclaration() ) {
      return true;
   } else if (ConsumeIfMatch(tok::kw_genvar) ) {
      //		if( !ParseListOfIdentifiers(false, false, false, false)) {
      //			printf("ERROR: Expected list of genvar identifiers\n");
      //			process_error);
      //		}
      //		if( !ConsumeIfMatch(tok::semicolon) ) {
      //			printf("ERROR: Expected ';' after genvar identifier list\n");
      //			process_error);
      //		}
      return true;
   }
   return false;
}
UNIMPLEMENTED_PARSE(ParsePortModuleItem)
UNIMPLEMENTED_PARSE(ParseParameterOverride)
UNIMPLEMENTED_PARSE(ParseBindTargetScope)
UNIMPLEMENTED_PARSE(ParseBindTargetInstance)
UNIMPLEMENTED_PARSE(ParseBindTargetInstanceList)
UNIMPLEMENTED_PARSE(ParseBindInstantiation)

// Section A.1.5 - Configuration source text
// TODO

// Section A.1.6 - Interface items
UNIMPLEMENTED_PARSE(ParseInterfaceOrGenerateItem)
UNIMPLEMENTED_PARSE(ParseExternTfDeclaration)
UNIMPLEMENTED_PARSE(ParseInterfaceItem)
UNIMPLEMENTED_PARSE(ParseNonPortInterfaceItem)

// Section A.1.7 - Program items
UNIMPLEMENTED_PARSE(ParseProgramItem)
UNIMPLEMENTED_PARSE(ParseNonPortProgramItem)
UNIMPLEMENTED_PARSE(ParseProgramGenerateItem)

// Section A.1.8 - Checker items
// TODO

// Section A.1.9 - Class items
// TODO

// Section A.1.10 - Constraints
// TODO

// Section A.1.11 - Package items

UNIMPLEMENTED_PARSE(ParsePackageItem)

// package_or_generate_item_declaration ::=
//   net_declaration
// | data_declaration
// | task_declaration
// | function_declaration
// | checker_declaration -- TODO
// | dpi_import_export -- TODO
// | extern_constraint_declaration -- TODO
// | class_declaration -- TODO
// | class_constructor_declaration -- TODO
// | local_parameter_declaration ; -- TODO
// | parameter_declaration ; -- TODO
// | covergroup_declaration -- TODO
// | overload_declaration -- TODO
// | assertion_item_declaration -- TODO
// | ;

bool Parser::ParsePackageOrGenerateItemDeclaration()
{
   switch( Tok.getKind() ) {
   case tok::kw_supply0:
   case tok::kw_supply1:
   case tok::kw_tri:
   case tok::kw_triand:
   case tok::kw_trior:
   case tok::kw_trireg:
   case tok::kw_tri0:
   case tok::kw_tri1:
   case tok::kw_uwire:
   case tok::kw_wire:
   case tok::kw_wand:
   case tok::kw_wor:
   case tok::kw_bit:
   case tok::kw_logic:
   case tok::kw_reg:
   case tok::kw_byte:
   case tok::kw_shortint:
   case tok::kw_int:
   case tok::kw_longint:
   case tok::kw_integer:
   case tok::kw_time:
   case tok::kw_event:
   case tok::kw_shortreal:
   case tok::kw_real:
   case tok::kw_realtime:
   case tok::kw_localparam:
   case tok::kw_parameter:
   case tok::kw_specparam:
   case tok::kw_const:
   case tok::kw_var:
      // TODO: parse data declaration
      ParseDataDeclarationList();
      break;
   case tok::kw_task:
   case tok::kw_function:
      ParseTaskOrFunctionDeclaration();
      break;
   default:
      return false;
      break;
   }

   return true;
}

UNIMPLEMENTED_PARSE(ParseAnonymousProgram)
UNIMPLEMENTED_PARSE(ParseAnonymousProgramItem)
// Section A.2 - Declarations

UNIMPLEMENTED_PARSE(ParseSpecparamDeclaration)
// Section A.2.1.2 - Port Declarations
UNIMPLEMENTED_PARSE(ParseRefDeclaration)

// TODO: Support interface port declarations
// port_declaration           ::= { attribute_instance } inout_declaration
//                              | { attribute_instance } input_declaration
//                              | { attribute_instance } output_declaration
//                              | { attribute_instance } ref_declaration
//                              | { attribute_instance } interface_port_declaration
// inout_declaration          ::= inout net_port_type list_of_port_identifiers
// input_declaration          ::= input net_port_type list_of_port_identifiers
//                              | input variable_port_type list_of_variable_identifiers
// output_declaration         ::= output net_port_type list_of_port_identifiers
//                              | output variable_port_type list_of_variable_port_identifiers
// ref_declaration            ::= ref variable_port_type list_of_port_identifiers
// interface_port_declaration ::= interface_identifier list_of_interface_identifiers
//                              | interface_identifier . modport_identifier list_of_interface_identifiers
// net_port_type              ::= [ net_type ] data_type_or_implicit
bool Parser::ParsePortDeclaration(  )
{
   bool require_net_port_type = false;
   switch( Tok.getKind() ) {
   case tok::kw_inout:
      require_net_port_type = true;
   case tok::kw_input:
   case tok::kw_output:
      ConsumeToken();
      break;
   default:
      return false;
      break;
   }

   auto netType = ParseNetType();
   ParseDataTypeOrImplicit( netType );

   if( !ParseListOfIdentifiers( ) ) {
      Diag(Tok, diag::err_expected_list_of_ident);
      SkipUntil(tok::semi);
   }

   ExpectAndConsumeSemi(diag::err_expected_semi_decl_list);

   return true;
}

// Section A.2.2.2 - Strengnths
// TODO

// Section A.2.2.3 - Delays

// delay_value ::=
//   unsigned_number
// | real_number
// | ps_identifier
// | time_literal
// | 1step
bool Parser::ParseDelayValue()
{
   switch( Tok.getKind() ) {
   case tok::numeric_constant:
   case tok::identifier:
      ConsumeToken();
      break;
   default:
      return false;
      break;
   }
   return true;
}
// TODO

// delay3 ::= # delay_value
//          | # ( mintypmax_expression [ , mintypmax_expression [ , mintypmax_expression ] ] )
// delay2 ::= # delay_value
//          | # ( mintypmax_expression [ , mintypmax_expression ] )
bool Parser::ParseDelay3()
{
   assert(Tok.is(tok::hash));
   ConsumeToken();

   if( Tok.is(tok::l_paren)){
      ConsumeParen();
      do{
         ParseMintypmaxExpression();
      } while(ConsumeIfMatch(tok::comma));
      ExpectAndConsume(tok::r_paren, diag::err_expected_rparen, "", tok::r_paren);
   } else {
      ParseDelayValue();
   }
   return true;
}

// Section A.2.3 - Declaration lists
UNIMPLEMENTED_PARSE(ParseListOfDefparamAssignments)

   // list_of_genvar_identifiers     ::= genvar_identifier { , genvar_identifier }
// list_of_interface_identifiers     ::= interface_identifier { unpacked_dimension }
//                                       { , interface_identifier { unpacked_dimension } }
// list_of_port_identifiers          ::= port_identifier { unpacked_dimension }
//                                       { , port_identifier { unpacked_dimension } }
// list_of_udp_port_identifiers      ::= port_identifier { , port_identifier }
// list_of_tf_variable_identifiers   ::= port_identifier { variable_dimension } [ = expression ]
//                                       { , port_identifier { variable_dimension } [ = expression ] }
// list_of_variable_identifiers      ::= variable_identifier { variable_dimension }
//                                       { , variable_identifier { variable_dimension } }
// list_of_variable_port_identifiers ::= port_identifier { variable_dimension } [ = constant_expression ]
//                                     { , port_identifier { variable_dimension } [ = constant_expression ] }
// variable_identifier_list          ::= variable_identifier { , variable_identifier }
// list_of_net_decl_assignments      ::= net_identifier { unpacked_dimension } [ = expression ]
//                                     { , net_identifier { unpacked_dimension } [ = expression ] }
bool Parser::ParseListOfIdentifiers(/* DeclTypeInfo *declType*/)
{
   llvm::StringRef ident;

   // Always start with an identifier
   if(Tok.isNot(tok::identifier) ) {
      return false;
   }

   do{
      // Fail if there isn't an identifier after a ','
      //  This first loop should always be an identifier because we exit early if it isn't
      if( Tok.isNot(tok::identifier)){
         Diag(Tok, diag::err_expected_ident_after_comma);
         SkipUntil(tok::comma,true, true);
         continue;
      }

      ParseIdentifier(&ident);

      // Always check for dimensions, this let the parser detect incorrect syntax and report it
      if( Tok.is(tok::l_square) ) {
         ParseDimension();
         bool foundDimension = false;
      }

      // Check for assignment expression
      if( ConsumeIfMatch(tok::equal) ) {
         auto result = ParseExpression(prec::Assignment);

         if( result.isInvalid() ) {
            // Handle error
         }
      }
   } while(ConsumeIfMatch(tok::comma));

   return true;
}

// list_of_param_assignments ::= param_assignment { , param_assignment }
// param_assignment ::= parameter_identifier { unpacked_dimension } [ = constant_param_expression ]
bool Parser::ParseListOfParamAssignments()
{
   llvm::StringRef ident;
   if( Tok.isNot(tok::identifier)){
      return false;
   }

   do {
      if( Tok.isNot(tok::identifier) ){
         Diag(Tok, diag::err_expected_ident);
         SkipUntil(tok::comma, true, false);
      }

      if(Tok.is(tok::l_square)) {
         ParseDimension();
      }

      if( ConsumeIfMatch( tok::equal ) ) {
         ParseExpression(prec::Assignment);
      }
   } while( ConsumeIfMatch( tok::comma ) );

   return true;
}

UNIMPLEMENTED_PARSE(ParseListOfSpecparamAssignments)
UNIMPLEMENTED_PARSE(ParseListOfTypeAssignments)

// list_of_variable_decl_assignments ::= variable_decl_assignment { , variable_decl_assignment }
bool Parser::ParseListOfVariableDeclAssignments()
{
   bool found_decl = false;
   while( ParseVariableDeclAssignment() ) {
      found_decl = true;
      if( !ConsumeIfMatch( tok::comma ) ) {
         break;
      }
   }
   return found_decl;
}

UNIMPLEMENTED_PARSE(ParseListOfVirtualInterfaceDecl)

// Section A.2.4 Declaration assignments
UNIMPLEMENTED_PARSE(ParseDefparamAssignment)
UNIMPLEMENTED_PARSE(ParseSpecparamAssignment)
UNIMPLEMENTED_PARSE(ParseTypeAssignment)
UNIMPLEMENTED_PARSE(ParsePulseControlSpecparam)
UNIMPLEMENTED_PARSE(ParseErrorLimitValue)
UNIMPLEMENTED_PARSE(ParseRejectLimitValue)
UNIMPLEMENTED_PARSE(ParseLimitValue)

// variable_decl_assignment ::=
//   variable_identifier { variable_dimension } [ = expression ]
// | dynamic_array_variable_identifier unsized_dimension { variable_dimension } [ = dynamic_array_new ]
// | class_variable_identifier [ = class_new ] -- TODO
bool Parser::ParseVariableDeclAssignment()
{
   llvm::StringRef var_name;

   if( Tok.isNot(tok::identifier)){
      return false;
   }

   ParseIdentifier( &var_name );

   if( Tok.is(tok::l_square)) {
       auto dimResult = ParseDimension();
       if (!dimResult.isInvalid() ) {
       }
    }

    if( ConsumeIfMatch( tok::equal ) ) {
      ParseExpression(prec::Assignment);
    }
  return true;
}

UNIMPLEMENTED_PARSE(ParseClassNew)
UNIMPLEMENTED_PARSE(ParseDynamicArrayNew)

// Section A.2.5 - Declaration ranges
// unpacked_dimension    ::= [ constant_range ]
//                         | [ constant_expression ]
// packed_dimension      ::= [ constant_range ]
//                         | [ ]
// variable_dimension    ::= unsized_dimension
//                         | unpacked_dimension
//                         | associative_dimension
//                         | queue_dimension
// unsized_dimension     ::= [ ]
// associative_dimension ::= [ data_type ] -- TODO
//                         | [ * ]
// queue_dimension       ::= [ $ [ : constant_expression ] ]
DimResult Parser::ParseDimension()
{
   bool allowErrorProcess = true;
   assert(Tok.is(tok::l_square) && "");

   ConsumeBracket();

   //DimensionInfo *info = nullptr;

   switch(Tok.getKind()){
   case tok::r_square:
      //info = new DimensionInfo(DimensionKind::Unsized, nullptr, nullptr);
      break;

      // TODO: Support type'd associative arrays
   case tok::star:
      ConsumeToken();
      //info = new DimensionInfo(DimensionKind::Associative, nullptr, nullptr);
      break;

      // TODO: Support constant expression
   case tok::dollar:
      if( ConsumeIfMatch(tok::colon) ) {
         auto result = ParseExpression(prec::Assignment);
         //info = new DimensionInfo(DimensionKind::Queue, result.get(), nullptr);
      }
      break;
   default:
      ExprResult lhsResult = ParseExpression(prec::Assignment);
      if( ConsumeIfMatch(tok::colon) ) {
         auto rhsResult = ParseExpression(prec::Assignment);

         //info = new DimensionInfo(DimensionKind::VariableRange, lhsResult.get(), rhsResult.get());
      } else {
         //info = new DimensionInfo(DimensionKind::VariableRange, lhsResult.get(), nullptr);
      }
      break;
   }

   ExpectAndConsume(tok::r_square, diag::err_expected_rsquare);

   //assert((info != nullptr) && "DimensionInfo should never be nullptr\n");

   // TODO
   return DimResult(false);
}

// Section A.2.6 - Function declarations

UNIMPLEMENTED_PARSE(ParseDpiImportExport)
UNIMPLEMENTED_PARSE(ParseDpiSpecString)
UNIMPLEMENTED_PARSE(ParseDpiFunctionImportProperty)
UNIMPLEMENTED_PARSE(ParseDpiTaskImportProperty)
UNIMPLEMENTED_PARSE(ParseDpiFunctionProto)
UNIMPLEMENTED_PARSE(ParseDpiTaskProto)

// Section A.2.7 - Task declarations
// task_declaration ::= task [ lifetime ] task_body_declaration
// function_declaration ::= function [ lifetime ] function_body_declaration

// task_body_declaration ::=
//   [ interface_identifier . | class_scope ] task_identifier ;
//     { tf_item_declaration }
//     { statement_or_null }
//   endtask [ : task_identifier ]
// | [ interface_identifier . | class_scope ] task_identifier ( [ tf_port_list ] ) ;
//     { block_item_declaration }
//     { statement_or_null }
//   endtask [ : task_identifier ]

// function_body_declaration ::=
//   function_data_type_or_implicit [ interface_identifier . | class_scope ] function_identifier ;
//     { tf_item_declaration }
//     { function_statement_or_null }
//   endfunction [ : function_identifier ]
// | function_data_type_or_implicit [ interface_identifier . | class_scope ] function_identifier ( [ tf_port_list ] ) ;
//     { block_item_declaration }
//     { function_statement_or_null }
//   endfunction [ : function_identifier ]

// function_data_type_or_implicit ::=
//   data_type_or_void
// | implicit_data_type
bool Parser::ParseTaskOrFunctionDeclaration()
{
   // Should never enter this if token isn't function or task
   assert((Tok.getKind() == tok::kw_task || Tok.getKind() == tok::kw_function) && "Token is not task or function");

   bool is_task = (Tok.getKind() == tok::kw_task);
   ConsumeToken();

   if( ConsumeIfMatch( tok::kw_static ) ) {
   } else if ( ConsumeIfMatch( tok::kw_automatic ) ) {
   }

   // Look for function_data_type_or_implicit if function
   if( !is_task ) {
      ParseDataTypeOrImplicit(NetType::Unknown);
   }

   llvm::StringRef ident;
   if( Tok.isNot(tok::identifier)){
      Diag(Tok, diag::err_expected_ident);
      // TODO: Handle error
   }

   ParseIdentifier(&ident);
   
   if( Tok.is( tok::l_paren) ) {
      //ParseTfPortList();
      ParseListOfPortDeclarations(); // TODO: Make sure list_of_port_declarations is compatabile with tf_port_list
   }

   ExpectAndConsumeSemi(diag::err_expected_semi_decl_list);

   while( ParseTfItemDeclaration() ) {}

   while(Tok.isNot(tok::kw_endtask) && Tok.isNot(tok::kw_endfunction)) {
      ParseStatementOrNull();
   }

   if( ConsumeIfMatch(tok::kw_endtask) && !is_task){
      Diag(Tok, diag::err_expected_func_end);
   }

   if( ConsumeIfMatch(tok::kw_endfunction) && is_task) {
      Diag(Tok, diag::err_expected_task_end);
   }

   if( ConsumeIfMatch( tok::colon ) ) {
      if( Tok.isNot(tok::identifier)){
         Diag(Tok, diag::err_expected_matching_ident);
         return true;
      }
      ParseIdentifier(&ident);
   }

   return true;
}

// tf_item_declaration ::=
//   block_item_declaration
// | tf_port_declaration
bool Parser::ParseTfItemDeclaration()
{
   switch(Tok.getKind()){
      case tok::kw_const:
         // If "const ref", it's a port
         if( NextToken().is(tok::kw_ref)){
            ParseTfPortDeclaration();
         } else {
            ParseBlockItemDeclaration();
         }
         break;
      case tok::kw_input:
      case tok::kw_output:
      case tok::kw_inout:
         ParseTfPortDeclaration();
         break;
      default:
         return ParseBlockItemDeclaration();
   }
   return true;
}

UNIMPLEMENTED_PARSE(ParseTfPortList)
UNIMPLEMENTED_PARSE(ParseTfPortItem)

// tf_port_direction ::= port_direction | const ref
// tf_port_declaration ::= { attribute_instance } tf_port_direction [ var ] data_type_or_implicit list_of_tf_variable_identifiers ;
bool Parser::ParseTfPortDeclaration()
{
   assert(Tok.is(tok::kw_input) || Tok.is(tok::kw_output) || Tok.is(tok::kw_inout) || Tok.is(tok::kw_const));

   switch( Tok.getKind() ) {
   case tok::kw_const:
      ConsumeToken();
      assert(Tok.is(tok::kw_ref));
      ConsumeToken();
      break;
   case tok::kw_input:
   case tok::kw_output:
   case tok::kw_inout:
      ConsumeToken();
      break;
   default:
      return false;
      break;
   }
   ParseDataTypeOrImplicit(NetType::Unknown);
   ParseListOfIdentifiers();
   ExpectAndConsumeSemi(diag::err_expected_semi_decl_list);
   return true;
}
bool Parser::ParseTaskPrototype()
{
   return false;
}

// Section A.2.8 - Block item declarations

// block_item_declaration ::=
//   { attribute_instance } data_declaration
// | { attribute_instance } local_parameter_declaration ;
// | { attribute_instance } parameter_declaration ;
// | { attribute_instance } overload_declaration
// | { attribute_instance } let_declaration
bool Parser::ParseBlockItemDeclaration()
{
   switch( Tok.getKind() ) {
   case tok::kw_supply0:
   case tok::kw_supply1:
   case tok::kw_tri:
   case tok::kw_triand:
   case tok::kw_trior:
   case tok::kw_trireg:
   case tok::kw_tri0:
   case tok::kw_tri1:
   case tok::kw_uwire:
   case tok::kw_wire:
   case tok::kw_wand:
   case tok::kw_wor:
   case tok::kw_bit:
   case tok::kw_logic:
   case tok::kw_reg:
   case tok::kw_byte:
   case tok::kw_shortint:
   case tok::kw_int:
   case tok::kw_longint:
   case tok::kw_integer:
   case tok::kw_time:
   case tok::kw_event:
   case tok::kw_shortreal:
   case tok::kw_real:
   case tok::kw_realtime:
   case tok::kw_localparam:
   case tok::kw_parameter:
   case tok::kw_specparam:
   case tok::kw_const:
   case tok::kw_var:
      // TODO: parse data declaration
      ParseDataDeclarationList();
      return true;
      break;

      // identifier identifier - is a typedef/struct/class declaration
   case tok::identifier:
      if(NextToken().is(tok::identifier)) {
         ParseDataDeclarationList();
         return true;
      }
      return false;
      break;
   default:
      break;
   }

   return false;
}
UNIMPLEMENTED_PARSE(ParseOverloadDeclaration)
UNIMPLEMENTED_PARSE(ParseOverloadOperator)
UNIMPLEMENTED_PARSE(ParseOverloadProtoFormals)

// Section A.2.9 - Interface declarations
// Section A.2.10 - Assertion declarations
// Section A.2.11 - Covergroup declarations
// TODO

// Section A.3 - Primitive instances
// gate_instantiation ::=
//   cmos_switchtype [delay3] cmos_switch_instance { , cmos_switch_instance } ;
// | enable_gatetype [drive_strength] [delay3] enable_gate_instance { , enable_gate_instance } ;
// | mos_switchtype [delay3] mos_switch_instance { , mos_switch_instance } ;
// | n_input_gatetype [drive_strength] [delay2] n_input_gate_instance { , n_input_gate_instance } ;
// | n_output_gatetype [drive_strength] [delay2] n_output_gate_instance { , n_output_gate_instance } ;
// | pass_en_switchtype [delay2] pass_enable_switch_instance { , pass_enable_switch_instance } ;
// | pass_switchtype pass_switch_instance { , pass_switch_instance } ;
// | pulldown [pulldown_strength] pull_gate_instance { , pull_gate_instance } ;
// | pullup [pullup_strength] pull_gate_instance { , pull_gate_instance } ;

// cmos_switch_instance ::= [ name_of_instance ] ( output_terminal , input_terminal , ncontrol_terminal , pcontrol_terminal )
// enable_gate_instance ::= [ name_of_instance ] ( output_terminal , input_terminal , enable_terminal )
// mos_switch_instance ::= [ name_of_instance ] ( output_terminal , input_terminal , enable_terminal )
// n_input_gate_instance ::= [ name_of_instance ] ( output_terminal , input_terminal { , input_terminal } )
// n_output_gate_instance ::= [ name_of_instance ] ( output_terminal { , output_terminal } , input_terminal )
// pass_switch_instance ::= [ name_of_instance ] ( inout_terminal , inout_terminal )
// pass_enable_switch_instance ::= [ name_of_instance ] ( inout_terminal , inout_terminal , enable_terminal )
// pull_gate_instance ::= [ name_of_instance ] ( output_terminal )

// TODO: pullup/pulldown
bool Parser::ParseGateInstantiation()
{
   switch(Tok.getKind()){
   case tok::kw_buf:   case tok::kw_bufif0:   case tok::kw_bufif1:
   case tok::kw_not:   case tok::kw_notif0:   case tok::kw_notif1:
   case tok::kw_tran:  case tok::kw_tranif0:  case tok::kw_tranif1:
   case tok::kw_rtran: case tok::kw_rtranif0: case tok::kw_rtranif1:
   case tok::kw_cmos:  case tok::kw_rcmos:
   case tok::kw_nmos:  case tok::kw_rnmos:
   case tok::kw_and:   case tok::kw_or:       case tok::kw_xor:   
   case tok::kw_nand:  case tok::kw_nor:      case tok::kw_xnor:
      ConsumeToken();
      break;
   case tok::kw_pulldown: case tok::kw_pullup:
      ConsumeToken();
      break;
   default:
      return false;
   }

   llvm::StringRef ident;

   // TODO: Check for drive strength
   if( Tok.is(tok::l_paren) ){
   }

   // Parse delay3 value
   if(Tok.is(tok::hash) ){
      ParseDelay3();
   }

   do{
      // Parse instance name
      if( Tok.is(tok::identifier) ){
         ParseIdentifier(&ident);
      }

      if( Tok.isNot(tok::l_paren) ){
         Diag(Tok, diag::err_expected_lparen_after) << "gate instantiation";
         SkipUntil(tok::semi);
         break;
      }
      ConsumeParen();

      do{
         ParseExpression(prec::Assignment);
      } while(ConsumeIfMatch(tok::comma));

      ExpectAndConsume(tok::r_paren, diag::err_expected_rparen, "", tok::semi);
   } while(ConsumeIfMatch(tok::comma));

   ExpectAndConsumeSemi(diag::err_expected_semi_after_decl);
   return true;
}

// Section A.4 - Instantiations
// Section A.4.1.1 - Module instantiation
// module_instantiation ::= module_identifier [ parameter_value_assignment ] hierarchical_instance { , hierarchical_instance } ;
bool Parser::ParseModuleInstantiation()
{
   bool require_ident = false;
   llvm::StringRef ident;

   if( Tok.isNot(tok::identifier)){
      return false;
   }
   ParseIdentifier(&ident);

   do {
      if( ParseHierarchicalInstance() ) {
         require_ident = true;
      } else if ( require_ident ) {
         // TODO: Error handling
      }
   } while ( ConsumeIfMatch( tok::comma ) );

   if( require_ident && !ConsumeIfMatch( tok::semi ) ) {
      // TODO: Error handling
   }
   return true;
}
UNIMPLEMENTED_PARSE(ParseParameterValueAssignment)
UNIMPLEMENTED_PARSE(ParseListOfParameterAssignments)
UNIMPLEMENTED_PARSE(ParseOrderedParameterAssignment)
UNIMPLEMENTED_PARSE(ParseNamedParameterAssignment)

//  hierarchical_instance ::= name_of_instance ( [ list_of_port_connections ] )
bool Parser::ParseHierarchicalInstance()
{
   llvm::StringRef ident;
   if( Tok.isNot(tok::identifier)){
      return false;
   }
  ParseIdentifier( &ident );
  ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "module instance name");
  ParseListOfPortConnections();
  ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   return true;
}
bool Parser::ParseNameOfInstance()
{
   return false;
}

// list_of_port_connections ::=
//      ordered_port_connection { , ordered_port_connection }
//    | named_port_connection { , named_port_connection }
// ordered_port_connection ::= { attribute_instance } [ expression ]
// named_port_connection ::=
//      { attribute_instance } . port_identifier [ ( [ expression ] ) ]
//    | { attribute_instance } .*   // TODO
bool Parser::ParseListOfPortConnections()
{
   llvm::StringRef ident;

   // Named port connection
   bool namedPortConnection = Tok.is(tok::period);

   do {

      if( Tok.isNot(tok::period) ){
         if(namedPortConnection) {
            Diag(Tok, diag::err_cant_mix_port_connection);
         }

         ParseExpression(prec::Assignment);
         continue;
      }

      // Consume .
      ConsumeToken();
      if( !namedPortConnection) {
         Diag(Tok, diag::err_cant_mix_port_connection);
      }

      // Consume token
      if(Tok.isNot(tok::identifier)){
         Diag(Tok, diag::err_expected_ident);
         SkipUntil(tok::comma, tok::r_paren, true, true);
         continue;
      }

      ParseIdentifier(&ident);

      ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "port identifier");
      ParseExpression(prec::Assignment);
      ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   } while( ConsumeIfMatch( tok::comma ) );

   return true;
}

// Section A.4.1.2 - Interface instantiation
// Section A.4.1.3 - Program instantiation
// Section A.4.1.4 - Checker instantiation
// Section A.4.2 - Generated instantiation
bool Parser::ParseGenerateRegion()
{
   if( !ConsumeIfMatch(tok::kw_generate) ) {
      return false;
   }

   while( ParseModuleOrGenerateItem() ||
      ParseInterfaceOrGenerateItem() ||
      0 // TODO ParseCheckerOrGenerateItem()
      ) {}
   return true;
}
// Section A.5 - UDP's
// TODO

//////////////////////////////////////
// Section A.7 - Specify's
//////////////////////////////////////
// TODO

/////////////////////////////////
// Section A.9 - General
/////////////////////////////////
// Section A.9.1 - Attributes
// Section A.9.2 - Comments
// TODO

// Section A.9.3 - Identifiers
// hierarchical_identifier ::= [ $root . ] { identifier constant_bit_select . } identifier
// select ::= [ { . member_identifier bit_select } . member_identifier ] bit_select [ [ part_select_range ] ]
bool Parser::ParseHierarchicalIdentifier()
{
   llvm::StringRef ident;
   bool found_ident = false;
   bool require_ident = false;
   bool found_period = false;

   // TODO: check for $root
#if 0
   if( ConsumeIfMatch( tok::sys_root ) ) {
      if( !ConsumeIfMatch( tok::period ) ) {
         printf( "ERROR: Expected '.' after $root\n" );
         process_error();
      }
   }
#endif

   do{
      if( Tok.isNot(tok::identifier)){
         break;
      }
      ParseIdentifier( &ident );
      found_ident = true;
      found_period = false;

      // Check to see if we should look for a constant_bit_select or it is
      //   the end of the identifier, in which case ignore
      // TODO: This isn't right, it should check based on the identifier type
      //       not based on the '.' or not
      if( Tok.is( tok::l_square ) ) {
         ParseSelectOrRange();
      }
   } while( ConsumeIfMatch(tok::period));

   if( !found_ident && require_ident ) {
      Diag(Tok, diag::err_expected_ident);
   }
   return found_ident;
}
bool Parser::ParsePackageScope()
{
   return false;
}

bool Parser::ParseIdentifier(llvm::StringRef *ref){
   assert(Tok.is(tok::identifier));
   ConsumeToken();
   return true;
}