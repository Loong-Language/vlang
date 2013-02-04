
//===--- IdentifierTable.h - Hash table for identifier lookup ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the IdentifierInfo, IdentifierTable, and Selector
// interfaces.
//
//===----------------------------------------------------------------------===//

#ifndef VERILOG_IDENTIFIERTABLE_H
#define VERILOG_IDENTIFIERTABLE_H

#include "Tokens.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <cassert>
#include <cctype>
#include <string>

namespace llvm {
  template <typename T> struct DenseMapInfo;
}

namespace vlang {
	class IdentifierInfo;
	class IdentifierTable;

	/// IdentifierInfo - One of these records is kept for each identifier that
		/// is lexed.
	class IdentifierInfo {
	  unsigned TokenID            : 9; // Front-end token ID or tok::identifier.
	  bool IsSystemVerilogKeyword : 1;
	  // 22 bit(s) left in 32-bit word.
  
	  llvm::StringMapEntry<IdentifierInfo*> *Entry;

	  IdentifierInfo(const IdentifierInfo&);  // NONCOPYABLE.
	  void operator=(const IdentifierInfo&);  // NONASSIGNABLE.

	  friend class IdentifierTable;
  
	public:
	  IdentifierInfo();

	  /// isStr - Return true if this is the identifier for the specified string.
	  /// This is intended to be used for string literals only: II->isStr("foo").
	  template <std::size_t StrLen>
	  bool isStr(const char (&Str)[StrLen]) const {
		return getLength() == StrLen-1 && !memcmp(getNameStart(), Str, StrLen-1);
	  }

	  /// getNameStart - Return the beginning of the actual string for this
	  /// identifier.  The returned string is properly null terminated.
	  ///
	  const char *getNameStart() const {
		if (Entry) return Entry->getKeyData();
		// FIXME: This is gross. It would be best not to embed specific details
		// of the PTH file format here.
		// The 'this' pointer really points to a
		// std::pair<IdentifierInfo, const char*>, where internal pointer
		// points to the external string data.
		typedef std::pair<IdentifierInfo, const char*> actualtype;
		return ((const actualtype*) this)->second;
	  }

	  /// getLength - Efficiently return the length of this identifier info.
	  ///
	  unsigned getLength() const {
		if (Entry) return Entry->getKeyLength();
		// FIXME: This is gross. It would be best not to embed specific details
		// of the PTH file format here.
		// The 'this' pointer really points to a
		// std::pair<IdentifierInfo, const char*>, where internal pointer
		// points to the external string data.
		typedef std::pair<IdentifierInfo, const char*> actualtype;
		const char* p = ((const actualtype*) this)->second - 2;
		return (((unsigned) p[0]) | (((unsigned) p[1]) << 8)) - 1;
	  }

	  /// getName - Return the actual identifier string.
	  llvm::StringRef getName() const {
		return llvm::StringRef(getNameStart(), getLength());
	  }

	  /// getTokenID - If this is a source-language token (e.g. 'for'), this API
	  /// can be used to cause the lexer to map identifiers to source-language
	  /// tokens.
	  TokenKind getTokenID() const { return (TokenKind)TokenID; }

	  bool isSystemVerilogKeyword() const { return IsSystemVerilogKeyword; }
	  void setIsSystemVerilogKeyword(bool Val) {  IsSystemVerilogKeyword = Val; }
  
	private:
	};

	/// IdentifierTable - This table implements an efficient mapping from strings to
	/// IdentifierInfo nodes.  It has no other purpose, but this is an
	/// extremely performance-critical piece of the code, as each occurrence of
	/// every identifier goes through here when lexed.
	class IdentifierTable {
	  // Shark shows that using MallocAllocator is *much* slower than using this
	  // BumpPtrAllocator!
	  typedef llvm::StringMap<IdentifierInfo*, llvm::BumpPtrAllocator> HashTableTy;
	  HashTableTy HashTable;

	public:
	  /// IdentifierTable ctor - Create the identifier table, populating it with
	  /// info about the language keywords for the language specified by LangOpts.
	  IdentifierTable();
  
	  llvm::BumpPtrAllocator& getAllocator() {
		return HashTable.getAllocator();
	  }

	  /// get - Return the identifier token info for the specified named identifier.
	  ///
	  IdentifierInfo &get(llvm::StringRef Name) {
		llvm::StringMapEntry<IdentifierInfo*> &Entry =
		  HashTable.GetOrCreateValue(Name);

		IdentifierInfo *II = Entry.getValue();
		if (II) return *II;

		// Lookups failed, make a new IdentifierInfo.
		void *Mem = getAllocator().Allocate<IdentifierInfo>();
		II = new (Mem) IdentifierInfo();
		Entry.setValue(II);

		// Make sure getName() knows how to find the IdentifierInfo
		// contents.
		II->Entry = &Entry;

		return *II;
	  }

	  IdentifierInfo &get(llvm::StringRef Name, TokenKind TokenCode) {
		IdentifierInfo &II = get(Name);
		II.TokenID = (int)TokenCode;
		assert(II.TokenID == (unsigned) TokenCode && "TokenCode too large");
		return II;
	  }

	  TokenKind lookupToken(llvm::StringRef s) {
		  IdentifierInfo *II =  HashTable.lookup(s);
		  if (II) return II->getTokenID();

		  return TokenKind::identifier;
	  }

	  typedef HashTableTy::const_iterator iterator;
	  typedef HashTableTy::const_iterator const_iterator;

	  iterator begin() const { return HashTable.begin(); }
	  iterator end() const   { return HashTable.end(); }
	  unsigned size() const { return HashTable.size(); }

	  /// PrintStats - Print some statistics to stderr that indicate how well the
	  /// hashing is doing.
	  void PrintStats() const;

	  void AddKeywords();
	};
};
#endif
