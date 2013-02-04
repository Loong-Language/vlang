//#include <cstdio>
#include <cstdlib>
#include <string>

#include "Tokens.h"
#include "Lexer.h"
#include "IdentifierTable.h"

#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Twine.h>
#include <llvm/Support/SMLoc.h>

using namespace vlang;

Lexer::Lexer( const llvm::SourceMgr *s, int buff)
{
  srcMgr = s;
  curBuf = srcMgr->getMemoryBuffer( buff );
  curPtr = curBuf->getBufferStart();
  tokStart = 0;
  tokenIdentTable = new IdentifierTable();
  tokenIdentTable->AddKeywords();
}

// TODO: Handle escaped identifiers
int Lexer::lexIdentifier()
{
  bool escaped_identifier = (*curPtr == '\\');
  bool system_task = (*curPtr == '$');
  if( isalpha( *curPtr ) || *curPtr == '`' || *curPtr == '$' || *curPtr == '\\' || *curPtr == '_') {   // identifier: [a-zA-Z_][a-zA-Z0-9_]*
    curPtr++;
    if( escaped_identifier) {
      while( !isspace( *curPtr) ) {
        ++curPtr;
      }
    } else {
      while( isalnum( *curPtr ) || *curPtr == '_' ){
        ++curPtr;
      }
    }

	curTok.kind = tokenIdentTable->lookupToken( llvm::StringRef( tokStart, curPtr - tokStart ) );
	if( curTok.kind != TokenKind::identifier ) {
		return 1;
	}

	if( preTok.kind == TokenKind::cd_define ) {
      curTok.kind = TokenKind::define_name;
    } else {
      curTok.kind = system_task ? TokenKind::systask : TokenKind::identifier;
    }
    return 1;
  }
  return 0;
}

// TODO: Handle escaped identifiers
void Lexer::lexLine()
{
  while( *curPtr != 0 && *curPtr != '\n' && *curPtr != '\r' )
    curPtr++;
}


// TODO: Handle non-decimal constants
// TODO: Handle X/Z values
void Lexer::lexConstant()
{
  curTok.kind = TokenKind::numeric_constant;
  while( isdigit( *curPtr ) ||
         *curPtr == 'a'     ||
         *curPtr == 'A'     ||
         *curPtr == 'b'     ||
         *curPtr == 'B'     ||
         *curPtr == 'c'     ||
         *curPtr == 'C'     ||
         *curPtr == 'd'     ||
         *curPtr == 'D'     ||
         *curPtr == 'e'     ||
         *curPtr == 'E'     ||
         *curPtr == 'f'     ||
         *curPtr == 'F'     ||
         *curPtr == '_'     ||
         *curPtr == '.'     ||
         *curPtr == 'Z'     ||
         *curPtr == 'z'     ||
         *curPtr == 'X'     ||
         *curPtr == 'x'     ||
         *curPtr == '?')
  {

	  // Need special handling when parsing the number if it has Z/z/X/x/?  
	  if( *curPtr == 'Z' || *curPtr == 'z' || *curPtr == 'X' || *curPtr == 'x' || *curPtr == '?' ){
		  curTok.kind = TokenKind::numeric_constant_xz;
	  }
	  ++curPtr;
  }

  curTok.tokenString = llvm::StringRef( tokStart, curPtr - tokStart );

}

int Lexer::lexWhiteSpace()
{
  bool hasNewLine = false;
  // Skip white spaces
  while( isspace( *curPtr ) ) {
    if( *curPtr == '\n' || *curPtr == '\r' ) {
      hasNewLine = true;
    }
    curPtr++;
    tokStart = curPtr;
    curTok.kind = TokenKind::whitespace;
  }

  if( hasNewLine )
    curTok.kind = TokenKind::newline;
  return ( curTok.kind == TokenKind::whitespace || curTok.kind == TokenKind::newline);
}

void Lexer::lexToken()
{
  bool previousNewLine = (curTok.kind == TokenKind::newline);
  if( curTok.kind != TokenKind::whitespace ) {
    preTok = curTok;
  }
  curTok.kind = TokenKind::unknown;
  bool is_signed;

  if( lexWhiteSpace() ) return;

  tokStart = curPtr;
  if( *curPtr == 0 ) {
    curTok.kind = TokenKind::eof;
    return;
  }

  if( preTok.kind == TokenKind::define_name && !previousNewLine) {
    lexLine();
    curTok.kind = TokenKind::define_value;
    return;
  }

  switch(preTok.kind){
    case TokenKind::base_signed_hex:
    case TokenKind::base_hex:
      return lexConstant();
      break;
    default:
      break;
  }
  
  if( lexIdentifier() ) {
    if( curTok.kind == TokenKind::cd_ifdef ) {
      lexLine();
    }
    return;
  }

  if( isdigit(*curPtr) ) {
    return lexConstant();
  }

  // c++ style comment
  if( *curPtr == '/' && curPtr[1] == '*') {
    while( *curPtr != 0 && !( curPtr[0] == '*' && curPtr[1] == '/' ) ) {
      curPtr++;
    }
    if( *curPtr == 0 ) {
      curTok.kind = TokenKind::eof;
    } else {
      curPtr += 2;
      curTok.kind = TokenKind::comment_block;
    }
    return;

    // c style comment
  } else if (*curPtr == '/' && curPtr[1] == '/') {
    lexLine();
    if( *curPtr == 0 ) {
      curTok.kind = TokenKind::eof;
    } else {
      curTok.kind = TokenKind::comment;
    }
    return;
  }

  curPtr++;
  while( !isalnum( *curPtr ) && !isspace( *curPtr)) {
    curPtr++;
  }


  do {
    for( curTok.kind = TokenKind::unknown; curTok.kind < TokenKind::NUM_TOKENS; curTok.kind = TokenKind( (int)curTok.kind + 1 ) ) {
      if( llvm::StringRef( tokStart, curPtr - tokStart ) == tok::getTokenName( curTok.kind ) ) break;
    }
    if( curPtr == tokStart) {
      curTok.kind = TokenKind::unknown;
      break;
    }

    if( curTok.kind == TokenKind::NUM_TOKENS){
      curPtr--;
    }
  } while( curTok.kind == TokenKind::NUM_TOKENS );

  switch( curTok.kind ) {
    case TokenKind::quote:
      while( *curPtr != '\"' )
        curPtr++;
      curPtr++;
      curTok.kind = TokenKind::string_literal;
      break;

    case TokenKind::apostrophe:
      is_signed = ( *curPtr == 's' ) || ( *curPtr == 'S' );
      if( is_signed ) {
        curPtr++;
      }

      switch( *curPtr ) {
        case 'b':
        case 'B':
          curTok.kind = is_signed ? TokenKind::base_signed_binary : TokenKind::base_binary;
          curPtr++;
          push_token();
          lexConstant();
          break;
        case 'd':
        case 'D':
          curTok.kind = is_signed ? TokenKind::base_signed_decimal : TokenKind::base_decimal;
          curPtr++;
          push_token();
          lexConstant();
          break;
        case 'o':
        case 'O':
          curTok.kind = is_signed ? TokenKind::base_signed_oct : TokenKind::base_oct;
          curPtr++;
          push_token();
          lexConstant();
          break;
        case 'h':
        case 'H':
          curTok.kind = is_signed ? TokenKind::base_signed_hex : TokenKind::base_hex;
          curPtr++;
          push_token();
          lexConstant();
          break;
        default:
          break;
      }
      break;
    case TokenKind::l_paren_star:
      while( curPtr[0] != '*' && curPtr[1] != ')' ) {
        curPtr++;
      }
      curPtr++;
      curPtr++;
      curTok.kind = TokenKind::attribute;
      break;
    default:
      break;
  }

  // Check for end of file
  if( *curPtr == 0 ) {
    curTok.kind = TokenKind::eof;
  }

  return;
}

void Lexer::push_token(){
  curTok.tokenString = llvm::StringRef( tokStart, curPtr - tokStart );
  tokens.push_back( curTok );
  tokStart = curPtr;
}

int Lexer::tokenize()
{
  curTok.kind = TokenKind::unknown;
  bool found = true;
  do {
    lexToken();
    push_token();
    if( found ){
      llvm::SMLoc loc = llvm::SMLoc::getFromPointer( curTok.tokenString.data() );
      const llvm::StringRef ref = tok::getTokenDebugName( curTok.kind );
      //srcMgr->PrintMessage( loc,llvm::SourceMgr::DK_Note,  llvm::Twine( "Lexed token ", ref ));
    }
  } while( curTok.kind != TokenKind::eof );
  itrToken = tokens.begin();
  skipWhiteSpaceOrComment();
  return 0;
}

void Lexer::skipWhiteSpaceOrComment(bool reverse )
{
  bool foundWhitespace = true;
  while( ( (itrToken != tokens.end() && !reverse) ||
	       (itrToken != tokens.begin() && reverse) ) && foundWhitespace) {
    // TODO: Remove attributes from here
    // TODO: Remove compiler directives from here
    switch( itrToken->kind ){
      case TokenKind::whitespace:
      case TokenKind::newline:
      case TokenKind::comment:
      case TokenKind::comment_block:
      case TokenKind::attribute:
      case TokenKind::cd_ifdef:
      case TokenKind::cd_else:
      case TokenKind::cd_endif:
		  if( reverse) {
			  itrToken--;
		  } else {
			  itrToken++;
		  }
        break;
      default:
        foundWhitespace = false;
        break;
    }
  }
}

Token* Lexer::getNextToken( int pop_token, int skipWhiteSpace )
{
  Token *token;

  if( itrToken == tokens.end() ) {
    return NULL;
  } else {
    itrToken++;
    if( skipWhiteSpace ) {
      skipWhiteSpaceOrComment();
      if( itrToken == tokens.end() )
        return NULL;
    } else {
      if( itrToken == tokens.end() )
        return NULL;

      itrToken++;
    }
  }
  token = &( *itrToken );
  if( !pop_token ) itrToken--;
  return token;
}

Token* Lexer::searchForTokenAfter( TokenKind kind )
{
  std::vector<Token>::iterator itrTokenPrev = itrToken;
  while( itrToken != tokens.end()) {
    if( itrToken->kind == kind) {
      break;
    }
    itrToken++;
  }
  if( itrToken == tokens.end()) {
    return NULL;
  }

  Token *rToken = getNextToken(0, 1);
  itrToken = itrTokenPrev;

  return rToken;
}

bool Lexer::checkTokenBetween( const TokenKind insideKind, const TokenKind finishKind, const TokenKind exitKind )
{
  int inside_count = countTokensBetween(insideKind, finishKind, exitKind);
  return (inside_count != 0);
}

int Lexer::countTokensBetween( const TokenKind insideKind, const TokenKind finishKind, const TokenKind exitKind){
  int current_depth = 0;
  int inside_count = 0;
  std::vector<Token>::iterator itrTokenPrev = itrToken;

  while( itrToken++ != tokens.end()) {
    if( current_depth == 0){
      if( itrToken->kind == insideKind) {
        inside_count++;
      } else if (itrToken->kind == finishKind) {
        break;
      }
    }

    if( itrToken->kind == exitKind ){
      inside_count = -1;
      break;
    }

    // TODO: Assert if we go negative
    switch(itrToken->kind) {
      case TokenKind::l_paren:
      case TokenKind::l_brace:
      case TokenKind::l_square:
        current_depth++;
        break;
      case TokenKind::r_paren:
      case TokenKind::r_brace:
      case TokenKind::r_square:
        current_depth--;
        break;
      default:
        break;
    }
  }

  itrToken = itrTokenPrev;
  return inside_count;
}

Token* Lexer::getPreviousToken()
{
  Token *token;

  if( itrToken == tokens.begin() )
    return NULL;
  
  itrToken--;
  skipWhiteSpaceOrComment(true);
  token = &( *itrToken );
  return token;
}

Token* Lexer::getCurToken()
{
  return & ( *itrToken );
}

