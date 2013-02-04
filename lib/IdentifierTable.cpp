//===--- IdentifierTable.cpp - Hash table for identifier lookup -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the IdentifierInfo, IdentifierVisitor, and
// IdentifierTable interfaces.
//
//===----------------------------------------------------------------------===//

#include "IdentifierTable.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/ErrorHandling.h"
#include <cstdio>

using namespace vlang;
//===----------------------------------------------------------------------===//
// IdentifierInfo Implementation
//===----------------------------------------------------------------------===//

IdentifierInfo::IdentifierInfo() {
  TokenID = (int)TokenKind::identifier;
  IsSystemVerilogKeyword = false;
  Entry = 0;
}

//===----------------------------------------------------------------------===//
// IdentifierTable Implementation
//===----------------------------------------------------------------------===//

IdentifierTable::IdentifierTable( )
  : HashTable(256) // Start with space for 256 identifiers.
  {

  // Populate the identifier table with info about keywords for the current
  // language.
  AddKeywords();
}

//===----------------------------------------------------------------------===//
// Language Keyword Implementation
//===----------------------------------------------------------------------===//

// Constants for TokenKinds.def
namespace {
  enum {
    KEYV1995 = 0x1,
    KEYV2001 = 0x2,
    KEYSV    = 0x4,
    KEYALL   = 0x7
  };
}

/// AddKeyword - This method is used to associate a token ID with specific
/// identifiers because they are language keywords.  This causes the lexer to
/// automatically map matching identifiers to specialized token codes.
static void AddKeyword(llvm::StringRef Keyword,
                       TokenKind TokenCode, unsigned Flags,
                       IdentifierTable &Table) {
  unsigned AddResult = 0;
  if ( (Flags & KEYALL) != 0) AddResult = 2;

  // Don't add this keyword if disabled in this language.
  if (AddResult == 0) return;

  IdentifierInfo &Info =
      Table.get(Keyword, AddResult == 3 ? TokenKind::identifier : TokenCode);
  Info.setIsSystemVerilogKeyword(false);
}

/// AddKeywords - Add all keywords to the symbol table.
///
void IdentifierTable::AddKeywords() {
  // Add keywords and tokens for the current language.
#define KEYWORD(NAME, FLAGS) \
  AddKeyword(llvm::StringRef(#NAME), TokenKind::kw_ ## NAME,  \
             FLAGS, *this);
#define CDKEYWORD(NAME, TEXT) \
  AddKeyword(llvm::StringRef(TEXT), TokenKind::cd_ ## NAME, \
	          KEYALL, *this);
#include "Tokens.def"
}

//===----------------------------------------------------------------------===//
// Stats Implementation
//===----------------------------------------------------------------------===//

/// PrintStats - Print statistics about how well the identifier table is doing
/// at hashing identifiers.
void IdentifierTable::PrintStats() const {
  unsigned NumBuckets = HashTable.getNumBuckets();
  unsigned NumIdentifiers = HashTable.getNumItems();
  unsigned NumEmptyBuckets = NumBuckets-NumIdentifiers;
  unsigned AverageIdentifierSize = 0;
  unsigned MaxIdentifierLength = 0;

  // TODO: Figure out maximum times an identifier had to probe for -stats.
  for (llvm::StringMap<IdentifierInfo*, llvm::BumpPtrAllocator>::const_iterator
       I = HashTable.begin(), E = HashTable.end(); I != E; ++I) {
    unsigned IdLen = I->getKeyLength();
    AverageIdentifierSize += IdLen;
    if (MaxIdentifierLength < IdLen)
      MaxIdentifierLength = IdLen;
  }

  fprintf(stderr, "\n*** Identifier Table Stats:\n");
  fprintf(stderr, "# Identifiers:   %d\n", NumIdentifiers);
  fprintf(stderr, "# Empty Buckets: %d\n", NumEmptyBuckets);
  fprintf(stderr, "Hash density (#identifiers per bucket): %f\n",
          NumIdentifiers/(double)NumBuckets);
  fprintf(stderr, "Ave identifier length: %f\n",
          (AverageIdentifierSize/(double)NumIdentifiers));
  fprintf(stderr, "Max identifier length: %d\n", MaxIdentifierLength);

  // Compute statistics about the memory allocated for identifiers.
  HashTable.getAllocator().PrintStats();
}

