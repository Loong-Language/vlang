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

#include "vlang/Basic/IdentifierTable.h"
#include "vlang/Basic/CharInfo.h"
#include "vlang/Basic/LangOptions.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cstdio>

using namespace vlang;

//===----------------------------------------------------------------------===//
// IdentifierInfo Implementation
//===----------------------------------------------------------------------===//

IdentifierInfo::IdentifierInfo() {
	TokenID = tok::identifier;
	SystaskID = 0;
	HasMacro = false;
	HadMacro = false;
	IsExtension = false;
	NeedsHandleIdentifier = false;
	FETokenInfo = 0;
	Entry = 0;
}

//===----------------------------------------------------------------------===//
// IdentifierTable Implementation
//===----------------------------------------------------------------------===//

IdentifierIterator::~IdentifierIterator() { }

IdentifierInfoLookup::~IdentifierInfoLookup() {}

namespace {
	/// \brief A simple identifier lookup iterator that represents an
	/// empty sequence of identifiers.
	class EmptyLookupIterator : public IdentifierIterator
	{
	public:
		virtual StringRef Next() { return StringRef(); }
	};
}

IdentifierIterator *IdentifierInfoLookup::getIdentifiers() {
	return new EmptyLookupIterator();
}

ExternalIdentifierLookup::~ExternalIdentifierLookup() {}

IdentifierTable::IdentifierTable(const LangOptions &LangOpts,
								 IdentifierInfoLookup* externalLookup)
								 : HashTable(8192), // Start with space for 8K identifiers.
								 ExternalLookup(externalLookup) {

  // Populate the identifier table with info about keywords for the current
  // language.
  AddKeywords(LangOpts);

}

//===----------------------------------------------------------------------===//
// Language Keyword Implementation
//===----------------------------------------------------------------------===//

// Constants for TokenKinds.def
namespace {
	enum {
		KEYV2001  = 0x01,
		KEYV2005  = 0x02,
		KEYSV2005 = 0x04,
		KEYSV2009 = 0x08,
		KEYSV2012 = 0x10,
		KEYALL = 0xffff // Because KEYNOMS is used to exclude.
	};
}

/// AddKeyword - This method is used to associate a token ID with specific
/// identifiers because they are language keywords.  This causes the lexer to
/// automatically map matching identifiers to specialized token codes.
///
/// The C90/C99/CPP/CPP0x flags are set to 3 if the token is a keyword in a
/// future language standard, set to 2 if the token should be enabled in the
/// specified language, set to 1 if it is an extension in the specified
/// language, and set to 0 if disabled in the specified language.
static void AddKeyword(StringRef Keyword,
					   tok::TokenKind TokenCode, unsigned Flags,
					   const LangOptions &LangOpts, IdentifierTable &Table) {
	unsigned AddResult = 0;
	if (Flags == KEYALL) AddResult = 2;
	else if (LangOpts.V2001  && Flags == KEYV2001 ) AddResult = 2;
	else if (LangOpts.V2005  && Flags == KEYV2005 ) AddResult = 2;
	else if (LangOpts.SV2005 && Flags == KEYSV2005) AddResult = 2;
	else if (LangOpts.SV2009 && Flags == KEYSV2009) AddResult = 2;
	else if (LangOpts.SV2012 && Flags == KEYSV2012) AddResult = 2;

	// Don't add this keyword if disabled in this language.
	if (AddResult == 0) return;

	IdentifierInfo &Info = Table.get(Keyword, TokenCode);
}

/// AddKeywords - Add all keywords to the symbol table.
///
void IdentifierTable::AddKeywords(const LangOptions &LangOpts) {
	// Add keywords and tokens for the current language.
#define KEYWORD(NAME, FLAGS) \
	AddKeyword(StringRef(#NAME), tok::kw_ ## NAME,  \
	FLAGS, LangOpts, *this);
#define ALIAS(NAME, TOK, FLAGS) \
	AddKeyword(StringRef(NAME), tok::kw_ ## TOK,  \
	FLAGS, LangOpts, *this);
#define TESTING_KEYWORD(NAME, FLAGS)
#define PPKEYWORD(NAME, FLAGS) \
   AddKeyword(StringRef("`" #NAME), tok::pp_ ## NAME, \
   FLAGS, LangOpts, *this);
#include "vlang/Basic/TokenKinds.def"

	if (LangOpts.ParseUnknownAnytype)
		AddKeyword("__unknown_anytype", tok::kw___unknown_anytype, KEYALL,
		LangOpts, *this);
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

/// Interpreting the given string using the normal CamelCase
/// conventions, determine whether the given string starts with the
/// given "word", which is assumed to end in a lowercase letter.
static bool startsWithWord(StringRef name, StringRef word) {
	if (name.size() < word.size()) return false;
	return ((name.size() == word.size() || !isLowercase(name[word.size()])) &&
		name.startswith(word));
}


const char *vlang::getOperatorSpelling(OverloadedOperatorKind Operator) {
	switch (Operator) {
	case OO_None:
	case NUM_OVERLOADED_OPERATORS:
		return 0;

#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
	case OO_##Name: return Spelling;
#include "vlang/Basic/OperatorKinds.def"
	}

	llvm_unreachable("Invalid OverloadedOperatorKind!");
}
