#include <cstdio>
#include <cstdlib>
#include <cassert>

#include "llvm/Support/CommandLine.h"

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
#include "Tokens.h"
#include "Parser.h"

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

static llvm::cl::list<std::string> InputFilenames(llvm::cl::Positional, llvm::cl::OneOrMore,
											llvm::cl::desc("<input bitcode files>"));
int main( int argc, char *argv[] )
{
	llvm::cl::ParseCommandLineOptions(argc, argv, " Vlang Parser\n");

	if( InputFilenames.size() == 0 ){
		printf("ERROR: Expected at least on input\n");
		exit(1);
	}

	vlang::Parser *p = new vlang::Parser();
	for( int i = 0; i < InputFilenames.size(); i++){
		p->ParseFile(InputFilenames[i].c_str() );
	}

	printf("FINISHED parsing\n");
    return 0;
}


