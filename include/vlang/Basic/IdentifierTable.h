//===--- IdentifierTable.h - Hash table for identifier lookup ---*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines the vlang::IdentifierInfo, vlang::IdentifierTable, and
/// vlang::Selector interfaces.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_BASIC_IDENTIFIERTABLE_H
#define LLVM_VLANG_BASIC_IDENTIFIERTABLE_H

#include "vlang/Basic/LLVM.h"
#include "vlang/Basic/OperatorKinds.h"
#include "vlang/Basic/TokenKinds.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <cassert>
#include <string>

namespace llvm {
	template <typename T> struct DenseMapInfo;
}

namespace vlang {
	class LangOptions;
	class IdentifierInfo;
	class IdentifierTable;
	class SourceLocation;
	class MultiKeywordSelector; // private class used by Selector
	class DeclarationName;      // AST class that stores declaration names

	/// \brief A simple pair of identifier info and location.
	typedef std::pair<IdentifierInfo*, SourceLocation> IdentifierLocPair;


	/// One of these records is kept for each identifier that
	/// is lexed.  This contains information about whether the token was \#define'd,
	/// is a language keyword, or if it is a front-end token of some sort (e.g. a
	/// variable or function name).  The preprocessor keeps this information in a
	/// set, and all tok::identifier tokens have a pointer to one of these.
	class IdentifierInfo {
		unsigned TokenID            : 9; // Front-end token ID or tok::identifier.
		unsigned SystaskID          :11; // Builtin ID
		bool HasMacro               : 1; // True if there is a `define for this.
		bool HadMacro               : 1; // True if there was a `define for this.
		bool IsExtension            : 1; // True if identifier is a lang extension.
		bool NeedsHandleIdentifier  : 1; // See "RecomputeNeedsHandleIdentifier".
		// definition loaded from an AST file.
		// called.
		// stored externally.
		// 32-bit word is filled.

		void *FETokenInfo;               // Managed by the language front-end.
		llvm::StringMapEntry<IdentifierInfo*> *Entry;

		IdentifierInfo(const IdentifierInfo&) LLVM_DELETED_FUNCTION;
		void operator=(const IdentifierInfo&) LLVM_DELETED_FUNCTION;

		friend class IdentifierTable;

	public:
		IdentifierInfo();


		/// \brief Return true if this is the identifier for the specified string.
		///
		/// This is intended to be used for string literals only: II->isStr("foo").
		template <std::size_t StrLen>
		bool isStr(const char (&Str)[StrLen]) const {
			return getLength() == StrLen-1 && !memcmp(getNameStart(), Str, StrLen-1);
		}

		/// \brief Return the beginning of the actual null-terminated string for this
		/// identifier.
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

		/// \brief Efficiently return the length of this identifier info.
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

		/// \brief Return the actual identifier string.
		StringRef getName() const {
			return StringRef(getNameStart(), getLength());
		}

		/// \brief Return true if this identifier is \`defined to some other value.
		bool hasMacroDefinition() const {
			return HasMacro;
		}
		void setHasMacroDefinition(bool Val) {
			if (HasMacro == Val) return;

			HasMacro = Val;
			if (Val) {
				NeedsHandleIdentifier = 1;
				HadMacro = true;
			} else {
				RecomputeNeedsHandleIdentifier();
			}
		}
		/// \brief Returns true if this identifier was \#defined to some value at any
		/// moment. In this case there should be an entry for the identifier in the
		/// macro history table in Preprocessor.
		bool hadMacroDefinition() const {
			return HadMacro;
		}

		/// getTokenID - If this is a source-language token (e.g. 'for'), this API
		/// can be used to cause the lexer to map identifiers to source-language
		/// tokens.
		tok::TokenKind getTokenID() const { return (tok::TokenKind)TokenID; }

		/// getBuiltinID - Return a value indicating whether this is a builtin
		/// function.  0 is not-built-in.  1 is builtin-for-some-nonprimary-target.
		/// 2+ are specific builtin functions.
		unsigned getSystaskID() const  { return SystaskID; }
		void setSystaskID(unsigned ID) { SystaskID = ID;   }

		/// get/setExtension - Initialize information about whether or not this
		/// language token is an extension.  This controls extension warnings, and is
		/// only valid if a custom token ID is set.
		bool isExtensionToken() const { return IsExtension; }
		void setIsExtensionToken(bool Val) {
			IsExtension = Val;
			if (Val)
				NeedsHandleIdentifier = 1;
			else
				RecomputeNeedsHandleIdentifier();
		}

		/// getFETokenInfo/setFETokenInfo - The language front-end is allowed to
		/// associate arbitrary metadata with this token.
		template<typename T>
		T *getFETokenInfo() const { return static_cast<T*>(FETokenInfo); }
		void setFETokenInfo(void *T) { FETokenInfo = T; }

		/// isHandleIdentifierCase - Return true if the Preprocessor::HandleIdentifier
		/// must be called on a token of this identifier.  If this returns false, we
		/// know that HandleIdentifier will not affect the token.
		bool isHandleIdentifierCase() const { return NeedsHandleIdentifier; }


	private:
		/// RecomputeNeedsHandleIdentifier - The Preprocessor::HandleIdentifier does
		/// several special (but rare) things to identifiers of various sorts.  For
		/// example, it changes the "for" keyword token from tok::identifier to
		/// tok::for.
		///
		/// This method is very tied to the definition of HandleIdentifier.  Any
		/// change to it should be reflected here.
		void RecomputeNeedsHandleIdentifier() {
			NeedsHandleIdentifier =
				( hasMacroDefinition() | isExtensionToken() );
		}
	};

	/// \brief An iterator that walks over all of the known identifiers
	/// in the lookup table.
	///
	/// Since this iterator uses an abstract interface via virtual
	/// functions, it uses an object-oriented interface rather than the
	/// more standard C++ STL iterator interface. In this OO-style
	/// iteration, the single function \c Next() provides dereference,
	/// advance, and end-of-sequence checking in a single
	/// operation. Subclasses of this iterator type will provide the
	/// actual functionality.
	class IdentifierIterator {
	private:
		IdentifierIterator(const IdentifierIterator &) LLVM_DELETED_FUNCTION;
		void operator=(const IdentifierIterator &) LLVM_DELETED_FUNCTION;

	protected:
		IdentifierIterator() { }

	public:
		virtual ~IdentifierIterator();

		/// \brief Retrieve the next string in the identifier table and
		/// advances the iterator for the following string.
		///
		/// \returns The next string in the identifier table. If there is
		/// no such string, returns an empty \c StringRef.
		virtual StringRef Next() = 0;
	};

	/// IdentifierInfoLookup - An abstract class used by IdentifierTable that
	///  provides an interface for performing lookups from strings
	/// (const char *) to IdentiferInfo objects.
	class IdentifierInfoLookup {
	public:
		virtual ~IdentifierInfoLookup();

		/// get - Return the identifier token info for the specified named identifier.
		///  Unlike the version in IdentifierTable, this returns a pointer instead
		///  of a reference.  If the pointer is NULL then the IdentifierInfo cannot
		///  be found.
		virtual IdentifierInfo* get(StringRef Name) = 0;

		/// \brief Retrieve an iterator into the set of all identifiers
		/// known to this identifier lookup source.
		///
		/// This routine provides access to all of the identifiers known to
		/// the identifier lookup, allowing access to the contents of the
		/// identifiers without introducing the overhead of constructing
		/// IdentifierInfo objects for each.
		///
		/// \returns A new iterator into the set of known identifiers. The
		/// caller is responsible for deleting this iterator.
		virtual IdentifierIterator *getIdentifiers();
	};

	/// \brief An abstract class used to resolve numerical identifier
	/// references (meaningful only to some external source) into
	/// IdentifierInfo pointers.
	class ExternalIdentifierLookup {
	public:
		virtual ~ExternalIdentifierLookup();

		/// \brief Return the identifier associated with the given ID number.
		///
		/// The ID 0 is associated with the NULL identifier.
		virtual IdentifierInfo *GetIdentifier(unsigned ID) = 0;
	};

	/// \brief Implements an efficient mapping from strings to IdentifierInfo nodes.
	///
	/// This has no other purpose, but this is an extremely performance-critical
	/// piece of the code, as each occurrence of every identifier goes through
	/// here when lexed.
	class IdentifierTable {
		// Shark shows that using MallocAllocator is *much* slower than using this
		// BumpPtrAllocator!
		typedef llvm::StringMap<IdentifierInfo*, llvm::BumpPtrAllocator> HashTableTy;
		HashTableTy HashTable;

		IdentifierInfoLookup* ExternalLookup;

	public:
		/// \brief Create the identifier table, populating it with info about the
		/// language keywords for the language specified by \p LangOpts.
		IdentifierTable(const LangOptions &LangOpts,
			IdentifierInfoLookup* externalLookup = 0);

		/// \brief Set the external identifier lookup mechanism.
		void setExternalIdentifierLookup(IdentifierInfoLookup *IILookup) {
			ExternalLookup = IILookup;
		}

		/// \brief Retrieve the external identifier lookup object, if any.
		IdentifierInfoLookup *getExternalIdentifierLookup() const {
			return ExternalLookup;
		}

		llvm::BumpPtrAllocator& getAllocator() {
			return HashTable.getAllocator();
		}

		/// \brief Return the identifier token info for the specified named
		/// identifier.
		IdentifierInfo &get(StringRef Name) {
			llvm::StringMapEntry<IdentifierInfo*> &Entry =
				HashTable.GetOrCreateValue(Name);

			IdentifierInfo *II = Entry.getValue();
			if (II) return *II;

			// No entry; if we have an external lookup, look there first.
			if (ExternalLookup) {
				II = ExternalLookup->get(Name);
				if (II) {
					// Cache in the StringMap for subsequent lookups.
					Entry.setValue(II);
					return *II;
				}
			}

			// Lookups failed, make a new IdentifierInfo.
			void *Mem = getAllocator().Allocate<IdentifierInfo>();
			II = new (Mem) IdentifierInfo();
			Entry.setValue(II);

			// Make sure getName() knows how to find the IdentifierInfo
			// contents.
			II->Entry = &Entry;

			return *II;
		}

		IdentifierInfo &get(StringRef Name, tok::TokenKind TokenCode) {
			IdentifierInfo &II = get(Name);
			II.TokenID = TokenCode;
			assert(II.TokenID == (unsigned) TokenCode && "TokenCode too large");
			return II;
		}

		/// \brief Gets an IdentifierInfo for the given name without consulting
		///        external sources.
		///
		/// This is a version of get() meant for external sources that want to
		/// introduce or modify an identifier. If they called get(), they would
		/// likely end up in a recursion.
		IdentifierInfo &getOwn(StringRef Name) {
			llvm::StringMapEntry<IdentifierInfo*> &Entry =
				HashTable.GetOrCreateValue(Name);

			IdentifierInfo *II = Entry.getValue();
			if (!II) {

				// Lookups failed, make a new IdentifierInfo.
				void *Mem = getAllocator().Allocate<IdentifierInfo>();
				II = new (Mem) IdentifierInfo();
				Entry.setValue(II);

				// Make sure getName() knows how to find the IdentifierInfo
				// contents.
				II->Entry = &Entry;
			}

			return *II;
		}

		typedef HashTableTy::const_iterator iterator;
		typedef HashTableTy::const_iterator const_iterator;

		iterator begin() const { return HashTable.begin(); }
		iterator end() const   { return HashTable.end(); }
		unsigned size() const { return HashTable.size(); }

		/// \brief Print some statistics to stderr that indicate how well the
		/// hashing is doing.
		void PrintStats() const;

		void AddKeywords(const LangOptions &LangOpts);
	};

}  // end namespace vlang

namespace llvm {
	// Provide PointerLikeTypeTraits for IdentifierInfo pointers, which
	// are not guaranteed to be 8-byte aligned.
	template<>
	class PointerLikeTypeTraits<vlang::IdentifierInfo*> {
	public:
		static inline void *getAsVoidPointer(vlang::IdentifierInfo* P) {
			return P;
		}
		static inline vlang::IdentifierInfo *getFromVoidPointer(void *P) {
			return static_cast<vlang::IdentifierInfo*>(P);
		}
		enum { NumLowBitsAvailable = 1 };
	};

	template<>
	class PointerLikeTypeTraits<const vlang::IdentifierInfo*> {
	public:
		static inline const void *getAsVoidPointer(const vlang::IdentifierInfo* P) {
			return P;
		}
		static inline const vlang::IdentifierInfo *getFromVoidPointer(const void *P) {
			return static_cast<const vlang::IdentifierInfo*>(P);
		}
		enum { NumLowBitsAvailable = 1 };
	};

}  // end namespace llvm
#endif
