#ifndef VERILOG_LEXER_H_INCLUDED
#define VERILOG_LEXER_H_INCLUDED

#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include "Tokens.h"

namespace llvm
{
class MemoryBuffer;
class SourceMgr;
}

namespace vlang {
	class IdentifierTable;

	class Lexer {
	private:
		std::vector<Token> tokens;
		std::vector<Token>::iterator  itrToken;

		const char *curPtr;
		const char *tokStart;

		Token curTok;
		Token preTok;

		const llvm::SourceMgr *srcMgr;
		const llvm::MemoryBuffer *curBuf;

		IdentifierTable *tokenIdentTable;

		int lexIdentifier();
		void lexConstant();
		int lexWhiteSpace();
		void lexLine();
		void lexToken();
		void push_token();
   
	public:
		Lexer( const llvm::SourceMgr *s, int buff );
		int tokenize();
		void skipWhiteSpaceOrComment(bool reverse = false);
		Token* getNextToken( int pop_token = 1, int skipWhiteSpace = 1);
		Token* searchForTokenAfter( TokenKind kind );
		bool checkTokenBetween( const TokenKind insideKind, const TokenKind finishKind, const TokenKind exitKind );
		int    countTokensBetween( const TokenKind insideKind, const TokenKind finishKind, const TokenKind exitKind);
		Token* getPreviousToken( );
		Token* getCurToken();
	};
}
#endif

