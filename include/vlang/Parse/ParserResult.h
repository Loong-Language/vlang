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
   class Stmt;

   enum class HeaderType : char {
      Ansi,
      NonAnsi
   };

   enum class DesignType : char{
      Unknown,
      Program,
      Module,
      Interface,
      Checker,
      Package,
      Primitive,
      Config
   };

   enum class PortKind : char {
      Unknown,
      Input,
      Output,
      Inout,
      Ref,
      GenericInterface
   };

   enum class SigningType : char {
      Unknown,
      Signed,
      Unsigned
   };

   enum class DeclType : char{
      Unknown,
      Design,
      Port,
      Net,
      Variable
   };

   enum class NetType : char{
      Unknown,
      Supply0,
      Supply1,
      Tri,
      TriAnd,
      TriOr,
      TriReg,
      Tri0,
      Tri1,
      Uwire,
      Wire,
      Wand,
      Wor
   };

   enum class DataType : char{
      Unknown,
      Implicit,
      Bit,
      Logic,
      Reg,
      Byte,
      ShortInt,
      Int,
      LongInt,
      Integer,
      Time,
      Event,
      Shortreal,
      Real,
      RealTime,
   };

   enum class DeclLifetime : char{
      Unknown,
      Static,
      Automatic
   };

   enum class DimensionKind {
      Unknown,               // For unknown, constructor will determine if it is constant/variable and range/expression
      ConstantRange,
      ConstantExpression,
      VariableRange,
      VariableExpression,
      Unsized,
      Associative,
      Queue
   };

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

		bool isInvalid()   const { return PtrWithInvalid & 0x01; }
		bool isValid()     const { return !isInvalid();          }
		bool isUsable()    const { return PtrWithInvalid > 0x01; }
		bool isNotUsable() const { return !isUsable();           }

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
   typedef ParseResult<Stmt*>            StmtResult;
};
#endif