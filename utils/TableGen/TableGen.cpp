 //===- TableGen.cpp - Top-Level TableGen implementation for Vlang ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the main function for Vlang's TableGen.
//
//===----------------------------------------------------------------------===//

#include "TableGenBackends.h" // Declares all backends.
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Signals.h"
#include "llvm/TableGen/Error.h"
#include "llvm/TableGen/Main.h"
#include "llvm/TableGen/Record.h"

using namespace llvm;
using namespace vlang;

enum ActionType {
  GenVlangDiagsDefs,
  GenVlangDiagGroups,
  GenVlangDiagsIndexName
};

namespace {
  cl::opt<ActionType>
  Action(cl::desc("Action to perform:"),
         cl::values(clEnumValN(GenVlangDiagsDefs, "gen-vlang-diags-defs",
                               "Generate Vlang diagnostics definitions"),
                    clEnumValN(GenVlangDiagGroups, "gen-vlang-diag-groups",
                               "Generate Vlang diagnostic groups"),
                    clEnumValN(GenVlangDiagsIndexName,
                               "gen-vlang-diags-index-name",
                               "Generate Vlang diagnostic name index"),
                    clEnumValEnd));

  cl::opt<std::string>
  VlangComponent("vlang-component",
                 cl::desc("Only use warnings from specified component"),
                 cl::value_desc("component"), cl::Hidden);

bool VlangTableGenMain(raw_ostream &OS, RecordKeeper &Records) {
  switch (Action) {
  case GenVlangDiagsDefs:
    EmitVlangDiagsDefs(Records, OS, VlangComponent);
    break;
  case GenVlangDiagGroups:
    EmitVlangDiagGroups(Records, OS);
    break;
  case GenVlangDiagsIndexName:
    EmitVlangDiagsIndexName(Records, OS);
    break;
  }

  return false;
}
}

int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);
  cl::ParseCommandLineOptions(argc, argv);

  return TableGenMain(argv[0], &VlangTableGenMain);
}
