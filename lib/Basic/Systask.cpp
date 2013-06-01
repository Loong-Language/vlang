//===--- Systasks.cpp - Systask function implementation -------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements various things for systask functions.
//
//===----------------------------------------------------------------------===//

#include "vlang/Basic/Systask.h"
#include "vlang/Basic/IdentifierTable.h"
#include "vlang/Basic/LangOptions.h"
#include "vlang/Basic/TargetInfo.h"
#include "llvm/ADT/SmallVector.h"
using namespace vlang;

static const Systask::Info SystaskInfo[] = {
  { "not a systask function", 0, 0, ALL_LANGUAGES },
#define SYSTASK(ID, TYPE, ATTRS, LANG) { "$" #ID, TYPE, ATTRS, LANG },
#include "vlang/Basic/Systask.def"
};

const Systask::Info &Systask::Context::GetRecord(unsigned ID) const {
  if (ID < Systask::FirstTSSystask)
    return SystaskInfo[ID];
  assert(ID - Systask::FirstTSSystask < NumTSRecords && "Invalid systask ID!");
  return TSRecords[ID - Systask::FirstTSSystask];
}

Systask::Context::Context() {
  // Get the target specific systasks from the target.
  TSRecords = 0;
  NumTSRecords = 0;
}

void Systask::Context::InitializeTarget() {
  assert(NumTSRecords == 0 && "Already initialized target?");
}

/// InitializeSystasks - Mark the identifiers for all the systasks with their
/// appropriate systask ID # and mark any non-portable systask identifiers as
/// such.
void Systask::Context::InitializeSystasks(IdentifierTable &Table,
                                          const LangOptions& LangOpts) {
  // Step #1: mark all target-independent systasks with their ID's.
  for (unsigned i = Systask::NotSystask+1; i != Systask::FirstTSSystask; ++i)
    Table.get(SystaskInfo[i].Name).setSystaskID(i);

  // Step #2: Register target-specific systasks.
  for (unsigned i = 0, e = NumTSRecords; i != e; ++i)
    Table.get(TSRecords[i].Name).setSystaskID(i+Systask::FirstTSSystask);
}

void
Systask::Context::GetSystaskNames(SmallVectorImpl<const char *> &Names) {
  // Final all target-independent names
  for (unsigned i = Systask::NotSystask+1; i != Systask::FirstTSSystask; ++i)
    Names.push_back(SystaskInfo[i].Name);

  // Find target-specific names.
  for (unsigned i = 0, e = NumTSRecords; i != e; ++i)
    Names.push_back(TSRecords[i].Name);
}

void Systask::Context::ForgetSystask(unsigned ID, IdentifierTable &Table) {
  Table.get(GetRecord(ID).Name).setSystaskID(0);
}

bool
Systask::Context::isPrintfLike(unsigned ID, unsigned &FormatIdx,
                               bool &HasVAListArg) {
  const char *Printf = strpbrk(GetRecord(ID).Attributes, "pP");
  if (!Printf)
    return false;

  HasVAListArg = (*Printf == 'P');

  ++Printf;
  assert(*Printf == ':' && "p or P specifier must have be followed by a ':'");
  ++Printf;

  assert(strchr(Printf, ':') && "printf specifier must end with a ':'");
  FormatIdx = strtol(Printf, 0, 10);
  return true;
}

// FIXME: Refactor with isPrintfLike.
bool
Systask::Context::isScanfLike(unsigned ID, unsigned &FormatIdx,
                              bool &HasVAListArg) {
  const char *Scanf = strpbrk(GetRecord(ID).Attributes, "sS");
  if (!Scanf)
    return false;

  HasVAListArg = (*Scanf == 'S');

  ++Scanf;
  assert(*Scanf == ':' && "s or S specifier must have be followed by a ':'");
  ++Scanf;

  assert(strchr(Scanf, ':') && "printf specifier must end with a ':'");
  FormatIdx = strtol(Scanf, 0, 10);
  return true;
}

