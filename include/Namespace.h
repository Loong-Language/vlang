#ifndef VERILOG_SYMBOL_TABLE_H
#define VERILOG_SYMBOL_TABLE_H

#include <list>
#include "llvm/ADT/StringRef.h"
#include "Type.h"

namespace vlang {
	class Decl;
	class TypeInfo;
	class Expr;

	class Scope{
	public:
		enum ScopeKind {
			Unknown,
			Defintition,
			Package,
			Compilation,
			Macro,
			Module,
			Block,
			Port,
			Attribute
		};

		Scope() : name(llvm::StringRef()), kind(ScopeKind::Unknown), enclosingNamespace(nullptr) { }
		Scope(llvm::StringRef n, ScopeKind k, Scope *p) : name(n) , kind(k), enclosingNamespace(p) {}
		Scope(llvm::StringRef n, ScopeKind k) : name(n), kind(k), enclosingNamespace(nullptr) {}

		llvm::StringRef getNamespaceName();
	
		void    setEnclosingNamespace(Scope *n);
		Scope*  getEnclosingNamespace();

	private:
		llvm::StringRef name;
		ScopeKind   kind;
		Scope       *enclosingNamespace;
		std::list<TypeInfo*> typeDecls;


		Decl *getSymbolLocal(llvm::StringRef n);
	};
}

#endif