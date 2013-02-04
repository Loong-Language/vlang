
#include "Namespace.h"
#include "Semantic.h"

using namespace vlang;

Semantic::Semantic(){
	currentScope = new Scope();
}

bool Semantic::ActOnDataDeclaration( const llvm::SMRange srcRange, llvm::StringRef ident, DeclTypeInfo *t, Expr *assignExpr){
	return true;
}

ExprResult Semantic::ActOnBinaryOperation( const llvm::SMRange opTokenSrc,
										  TokenKind opTokenKind,
										  Expr *LHS, Expr *RHS)
{
	return ExprResult(false);
}
ExprResult Semantic::ActOnConditionalOperation( const llvm::SMRange questionTokenSrc,
											   const llvm::SMRange colonTokenSrc,
											   Expr *CondExpr, Expr *LHS, Expr *RHS)
{
	return ExprResult(false);
}

ExprResult Semantic::ActOnConcatenation( const llvm::SMLoc LBrace, const llvm::SMLoc RBrace,
										ExprVector &Exprs, std::vector<llvm::SMLoc> &CommaLoc)
{
	return ExprResult(false);
}
ExprResult Semantic::ActOnMultipleConcatenation( const llvm::SMLoc LBrace, const llvm::SMLoc RBrace,
												Expr *Width, Expr *Inside)
{
	return ExprResult(false);
}
