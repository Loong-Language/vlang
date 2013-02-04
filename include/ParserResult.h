#ifndef VERILOG_PARSER_RESULT_H
#define VERILOG_PARSER_RESULT_H

#include <vector>

#include <llvm/Support/PointerLikeTypeTraits.h>

namespace vlang {

	class DimensionInfo;
	class TypeInfo;
	class DeclTypeInfo;
	class DeclStmt;
	class Expr;

	typedef std::vector<Expr*> ExprVector;

	template<typename PtrTy>
	class ParseResult {
		// A pointer whose low bit is 1 if this result is invalid, 0
		// otherwise.
		uintptr_t PtrWithInvalid;
		typedef llvm::PointerLikeTypeTraits<PtrTy> PtrTraits;
	public:
		ParseResult(bool Invalid = false)
			: PtrWithInvalid(static_cast<uintptr_t>(Invalid)) { }

		ParseResult(PtrTy V) {
			void *VP = PtrTraits::getAsVoidPointer(V);
			PtrWithInvalid = reinterpret_cast<uintptr_t>(VP);
			assert((PtrWithInvalid & 0x01) == 0 && "Badly aligned pointer");
		}

		// These two overloads prevent void* -> bool conversions.
		ParseResult(const void *);
		ParseResult(volatile void *);

		bool isInvalid() const { return PtrWithInvalid & 0x01; }
		bool isUsable() const { return PtrWithInvalid > 0x01; }

		PtrTy get() const {
			void *VP = reinterpret_cast<void *>(PtrWithInvalid & ~0x01);
			return PtrTraits::getFromVoidPointer(VP);
		}
		// FIXME: Replace with get.
		PtrTy take() const { return get(); }
		PtrTy release() const { return get(); }
		template <typename T> T *takeAs() { return static_cast<T*>(get()); }

		void set(PtrTy V) {
			void *VP = PtrTraits::getAsVoidPointer(V);
			PtrWithInvalid = reinterpret_cast<uintptr_t>(VP);
			assert((PtrWithInvalid & 0x01) == 0 && "Badly aligned pointer");
		}

		const ParseResult &operator=(PtrTy RHS) {
			void *VP = PtrTraits::getAsVoidPointer(RHS);
			PtrWithInvalid = reinterpret_cast<uintptr_t>(VP);
			assert((PtrWithInvalid & 0x01) == 0 && "Badly aligned pointer");
			return *this;
		}
	};

	typedef ParseResult<Expr*>            ExprResult;
	typedef ParseResult<DimensionInfo*>   DimResult;
	typedef ParseResult<TypeInfo*>        TypeResult;
	typedef ParseResult<DeclTypeInfo*>    DeclTypeResult;
	typedef ParseResult<DeclStmt*>        DeclStmtResult;
};
#endif