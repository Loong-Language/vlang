//===--- main.cpp - Vlang testing f ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <cstdio>
#include <cstdlib>
#include <cassert>

#include "llvm/Support/CommandLine.h"

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
#include "vlang/Lex/Preprocessor.h"
#include "vlang/Lex/Lexer.h"
#include "vlang/Basic/Diagnostic.h"
#include "vlang/Basic/DiagnosticOptions.h"
#include "vlang/Frontend/TextDiagnosticPrinter.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/SourceManager.h"
#include "vlang/Basic/TargetInfo.h"
#include "vlang/Basic/TargetOptions.h"
#include "vlang/Lex/PreprocessorOptions.h"
#include "vlang/Lex/HeaderSearchOptions.h"
#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Parse/Parser.h"
#include "vlang/Sema/Sema.h"
#include <llvm/Support/system_error.h>
#include <llvm/Support/raw_ostream.h>
#include "vlang/Frontend/Utils.h"
#include "vlang/Basic/TokenKinds.h"

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
using namespace llvm;
using namespace vlang;

static cl::list<std::string> InputFilenames(cl::Positional, cl::OneOrMore,
											cl::desc("<input bitcode files>"));

static cl::list<std::string> HeaderSearchPaths("I", cl::NormalFormatting, cl::ZeroOrMore,
                                 cl::desc("Path to Headers"));

int main( int argc, char *argv[] )
{
	cl::ParseCommandLineOptions(argc, argv, " Vlang Parser\n");

	if( InputFilenames.size() == 0 ){
		printf("ERROR: Expected at least on input\n");
		exit(1);
	}


   std::string errString;
   for (auto file : InputFilenames) {
      FileSystemOptions FileMgrOpts;
      FileManager       FileMgr(FileMgrOpts);

      IntrusiveRefCntPtr<DiagnosticIDs> DiagID(new DiagnosticIDs());
      LangOptions LangOpts;
      HeaderSearchOptions HeadSearch;
      TextDiagnosticPrinter *DiagPrinter = new TextDiagnosticPrinter(llvm::errs(), new DiagnosticOptions());
      DiagnosticsEngine Diags(DiagID, new DiagnosticOptions, DiagPrinter);
      SourceManager SourceMgr(Diags,FileMgr);
      IntrusiveRefCntPtr<TargetOptions> TargetOpts(new TargetOptions);
      IntrusiveRefCntPtr<TargetInfo> Target;

      auto buf = FileMgr.getBufferForFile(file.c_str(), &errString);
      SourceMgr.createMainFileIDForMemBuffer(buf);
      
      // Add search paths for `include
      for( auto header : HeaderSearchPaths){
         HeadSearch.AddPath(header.c_str(), frontend::Quoted, true);
      }

      HeaderSearch HeaderInfo(&HeadSearch, FileMgr, Diags, LangOpts);
      PreprocessorOptions PPopts;
      Preprocessor PP(&PPopts, Diags, LangOpts, SourceMgr, HeaderInfo,0, false, false);

      InitializePreprocessor(PP, PPopts, HeadSearch);

      DiagPrinter->BeginSourceFile(LangOpts, &PP);
      PP.EnterMainSourceFile();
      Parser P(PP, Sema(PP, TU_Complete, nullptr),false);
      P.Initialize();
      while(!P.ParseTopLevelDecl()){}
      printf("\nFINISHED parsing\n");
   }

    return 0;
}


