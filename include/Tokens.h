//===--- TokenKinds.h - Enum values for C Token Kinds -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines the TokenKind enum and support functions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VERILOG_TOKEN_H
#define LLVM_VERILOG_TOKEN_H

#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>

namespace vlang {

	/// TokenKind - This provides a simple uniform namespace for tokens from all C
	/// languages.
	enum class TokenKind : int {
#define TOK(X) X,
#include "Tokens.def"
		NUM_TOKENS
	};

	namespace tok {
		/// \brief Determines the name of a token as used within the front end.
		///
		/// The name of a token will be an internal name (such as "l_square")
		/// and should not be used as part of diagnostic messages.
		const char *getTokenName( vlang::TokenKind Kind );
		const char *getTokenDebugName( vlang::TokenKind Kind );
		const char *getAlphaNumConst();
	}

	struct Token {
		TokenKind kind;
		llvm::StringRef tokenString;
	};

	const char *getTokenString( Token *t );

	llvm::SMRange getTokenRange( Token *t);
	llvm::SMRange getTokenRange( Token *t0, Token *t1);
};
#endif

