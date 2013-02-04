#ifndef VERILOG_TYPE_H
#define VERILOG_TYPE_H

#include <vector>
#include <utility>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>

namespace vlang {
	class Expr;

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
		RealTime
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

	/// @class	DimensionInfo
	///
	/// @brief	Contains the dimension kind, and the max/min expressions inside if they exist.
	class DimensionInfo {
	public:

		/// @fn	DimensionInfo::DimensionInfo(DimensionKind k, ExprResult *lhs, ExprResult *rhs)
		///
		/// @brief	Constructor.
		///
		/// @param	k		The DimensionKind to process.
		/// @param [in,out]	lhs	The lhs of the range or if this is not a range it's
		/// 				the expression inside the dimension
		/// @param [in,out]	rhs	The rhs of the range
		DimensionInfo(DimensionKind k, Expr *lhs, Expr *rhs) : dimension(lhs,rhs), kind(k){}

		/// @fn	DimensionInfo::DimensionInfo(ExprResult *lhs, ExprResult *rhs);
		///
		/// @brief	Constructor.
		///
		/// @param [in,out]	lhs	The lhs of the range or if this is not a range it's
		/// 				the expression inside the dimension
		/// @param [in,out]	rhs	The rhs of the range
		DimensionInfo(Expr *lhs, Expr *rhs);

		~DimensionInfo();

		DimensionKind getKind() { return kind; }
		Expr    *getLhs() { return dimension.first;  }
		Expr    *getRhs() { return dimension.second; }

	private:

		/// @typedef	std::pair<Expr*,Expr*> DeclarationDimension
		///
		/// @brief	Defines an alias representing the declaration dimension. Implemented using pair's of
		/// 	ExprResult, if the second ExprResult is not nullptr, then the dimension is a range.
		typedef std::pair<Expr*,Expr*> DimensionPair;

		DimensionKind kind;
		DimensionPair dimension;
	};

	bool operator==( DimensionInfo &lhs, DimensionInfo &rhs);
	bool operator!=( DimensionInfo &lhs, DimensionInfo &rhs);

	/// @class	TypeInfo
	///
	/// @brief	TypeInfo contains the type information for a declaration.  It does not include any
	/// 		modifies (ie const, or array'd) done in the declaration.
	class TypeInfo {
	public:
		TypeInfo( DataType d, NetType n, SigningType s, DimensionInfo *dim) :
			dataType(d), netType(n), signType(s), dimension(*dim) {}
		TypeInfo( DataType d, NetType n, SigningType s) :
			dataType(d), netType(n), signType(s), dimension(DimensionKind::Unknown, nullptr, nullptr){}
		~TypeInfo();

		DimensionInfo &getDimension()   { return dimension;   }
		DataType       getDataType()    { return dataType;    }
		NetType        getNetType()     { return netType;     }
		SigningType    getSigning()     { return signType;    }
		llvm::SMRange  getSourceRange() { return sourceRange; }

		void           setSourceRange(llvm::SMRange s) { sourceRange = s; }
	private:
		DimensionInfo            dimension;
		DataType                 dataType;
		NetType                  netType;
		SigningType              signType;
		llvm::SMRange            sourceRange;
	};

	// TODO: Figure out why we can't use const here
	bool operator==(TypeInfo &lhs, TypeInfo &rhs);
	bool operator!=(TypeInfo &lhs, TypeInfo &rhs);

	/// @typedef	std::vector<DimensionInfo> DimensionList
	///
	/// @brief	Defines an alias representing list of declaration dimensions.
	typedef std::vector<DimensionInfo> DimensionInfoList;

	/// @class	DeclTypeInfo
	///
	/// @brief	Full type information of the declaration, including any modifiers (ie const), and
	/// 		array dimensions.
	class DeclTypeInfo {
	public:
		DeclTypeInfo();
		DeclTypeInfo(DeclTypeInfo &d);
		DeclTypeInfo(TypeInfo *t) : type(t) {};

		int  getArrayDimensionSize();
		void addArrayDimension(DimensionInfo dim);
		DimensionInfo getArrayDimension(int i);

	private:
		DimensionInfoList arrayDimension;
		TypeInfo      *type;

		// Properties of the Declaration
		bool isConst;
		DeclLifetime lifetime;
	};
};
#endif