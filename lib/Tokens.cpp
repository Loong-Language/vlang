#include <cassert>
#include "llvm/Support/SMLoc.h"
#include "Tokens.h"

namespace vlang {
	namespace tok {
		static const char * const TokNames[] = {
           #define TOK(X) "",
           #define KEYWORD(X, Y) #X,
	       #define PUNCTUATOR(X, Y) Y,
	       #define CDKEYWORD(X, Y) Y,
	       #include "Tokens.def"
		   0
		};

		static const char * const TokDebugNames[] = {
	       #define TOK(X) #X,
	       #define KEYWORD(X, Y) "keyword " #X,
	       #define PUNCTUATOR(X, Y) #X,
	       #define CDKEYWORD(X, Y) "cd_" #X,
	       #include "Tokens.def"
		   0
		};

		static const char * const alphanum =
			"0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

		static const char * const verilogVal = "0123456789_xXzZ";

		const char *getTokenName( vlang::TokenKind Kind ) {
			assert( Kind < vlang::TokenKind::NUM_TOKENS );
			return TokNames[(int)Kind];
		}

		const char *getTokenDebugName( vlang::TokenKind Kind ) {
			assert( Kind < vlang::TokenKind::NUM_TOKENS );
			return TokDebugNames[(int)Kind];
		}

		const char *getAlphaNumConst() {
			return alphanum;
		}
	};

	const char *getTokenString( Token *t ) {
		return t->tokenString.slice( 0, t->tokenString.size() - 1 ).str().c_str();
	}

	llvm::SMRange getTokenRange( Token *t ) {
		llvm::SMLoc loc = llvm::SMLoc::getFromPointer( t->tokenString.data() );
		llvm::SMLoc end = llvm::SMLoc::getFromPointer( t->tokenString.data() + t->tokenString.size() );
		return llvm::SMRange(loc, end);
	}

	llvm::SMRange getTokenRange( Token *t0, Token *t1){
		llvm::SMLoc loc = llvm::SMLoc::getFromPointer( t0->tokenString.data() );
		llvm::SMLoc end = llvm::SMLoc::getFromPointer( t1->tokenString.data() + t1->tokenString.size() );
		return llvm::SMRange(loc, end);
	}
};