#ifndef VERILOG_SEMANTIC_H
#define VERILOG_SEMANTIC_H

#include <vector>
#include <utility>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>
#include "Ast.h"
#include "Declaration.h"
#include "Type.h"
#include "ParserResult.h"

namespace vlang {

	typedef llvm::StringRef Identifier;
	enum class TokenKind : int;

	enum class HeaderType : char {
		Ansi,
		NonAnsi
	};

	enum class DesignType : char{
		Unknown,
		Program,
		Module,
		Interface,
		Checker,
		Package,
		Primitive,
		Config
	};

	enum class PortKind : char {
		Unknown,
		Input,
		Output,
		Inout,
		Ref,
		GenericInterface
	};

	class Scope;

	/// @class	Semantic
	///
	/// @brief	Describes the semantic actions to take during parsing
	class Semantic{
	public:
		Semantic();

		bool ActOnDesignDeclaration(Identifier &ident, DesignType type, DeclLifetime lifetime);
		bool ActOnPortListEntry(Identifier &ident);

		bool ActOnAnsiPortDeclaration(Identifier &ident, PortKind dir, DeclTypeInfo type, Expr *val);
		bool ActOnAnsiParameterDeclaration(Identifier &ident, DeclTypeInfo type, Expr *val);

		bool ActOnDataDeclaration( const llvm::SMRange srcRange, llvm::StringRef ident, DeclTypeInfo *t, Expr *assignExpr);

		ExprResult ActOnUnaryOperation(const llvm::SMLoc opTokenSrc, TokenKind opTokenKind, Expr* RHS);
		ExprResult ActOnBinaryOperation( const llvm::SMRange opTokenSrc, TokenKind opTokenKind, Expr *LHS, Expr *RHS);
		ExprResult ActOnConditionalOperation( const llvm::SMRange questionTokenSrc, const llvm::SMRange colonTokenSrc, Expr *CondExpr, Expr *LHS, Expr *RHS);
		ExprResult ActOnConcatenation( const llvm::SMLoc LBrace, const llvm::SMLoc RBrace, ExprVector &Exprs, std::vector<llvm::SMLoc> &CommaLoc);
		ExprResult ActOnMultipleConcatenation( const llvm::SMLoc LBrace, const llvm::SMLoc RBrace, Expr *Width, Expr *Inside);

		ExprResult ActOnTaskFuncCall(const llvm::SMRange callIdent, const llvm::SMLoc LBrace, const llvm::SMLoc RBrace, ExprVector &Exprs, std::vector<llvm::SMLoc> &CommaLoc );

		ExprResult ActOnNumericLiteral( const llvm::SMRange srcRange, int bitwidth, int radix, llvm::StringRef num);
	private:
		Scope *currentScope;
	};

}
#endif