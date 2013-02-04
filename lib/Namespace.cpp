
#include <algorithm>

#include "Namespace.h"
#include "Declaration.h"
#include "Type.h"

using namespace vlang;

llvm::StringRef Scope::getNamespaceName(){
	return name;
}

void Scope::setEnclosingNamespace(Scope *n){
	// TODO: ERROR if already defined
	enclosingNamespace = n;
}

Scope* Scope::getEnclosingNamespace(){
	return enclosingNamespace;
}
