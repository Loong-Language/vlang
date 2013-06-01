//===- TableGenBackends.h - Declarations for Vlang TableGen Backends ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declarations for all of the Vlang TableGen
// backends. A "TableGen backend" is just a function. See
// "$LLVM_ROOT/utils/TableGen/TableGenBackends.h" for more info.
//
//===----------------------------------------------------------------------===//

#include <string>

namespace llvm {
  class raw_ostream;
  class RecordKeeper;
}

using llvm::raw_ostream;
using llvm::RecordKeeper;

namespace vlang {

void EmitVlangDiagsDefs(RecordKeeper &Records, raw_ostream &OS,
                        const std::string &Component);
void EmitVlangDiagGroups(RecordKeeper &Records, raw_ostream &OS);
void EmitVlangDiagsIndexName(RecordKeeper &Records, raw_ostream &OS);

void EmitOptParser(RecordKeeper &Records, raw_ostream &OS, bool GenDefs);

} // end namespace vlang
