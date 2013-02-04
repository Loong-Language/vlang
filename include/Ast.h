#ifndef VERILOG_AST_H
#define VERILOG_AST_H

#include <vector>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SMLoc.h>

#include "ParserResult.h"

namespace vlang {

	enum class BinaryOp {
		Unknown,
		NonBlockingAssign,
		BlockingAssign,
		AddAssign, SubAssign, DivAssign, MultAssign, ModAssign,  // Arithmetic Assignment
		AndAssign, OrAssign, XorAssign,                          // Bit Logical Assignment
		LogicShiftLeftAssign, LogicShiftRightAssign,             // Logical Shift Assignment
		ArithShiftLeftAssign, ArithShiftRightAssign,             // Arithmetic Shift Assignment
		Add, Sub, Mult, Div, Power, Mod,                         // Arithmetic
		BitAnd, BitOr, BitXor, BotXNor,                          // Bit Logical
		LogicShiftLeft, LogicShiftRight,                         // Logical Shift
		ArithShiftLeft, ArithShiftRight,                         // Arithmetic Shift
		LogicAnd, LogicOr,  /* TODO: -> <->  */                  // Logical
		Less, LessEqual, Greater, GreaterEqual,                  // Relational
		CaseEqual, CaseNotEqual,                                 // Case Equality
		Equal, NotEqual,                                         // Logical Equality
		EqualWild, NotEqualWild                                  // Wildcard Equality
	};

	class Stmt {
	public:
		enum class StmtClass {
			NoStmtClass,
			AssignStmt,
			AliasStmt,
			InitialStmt,
			AlwaysStmt,
			FinalStmt,
			ProcedrualDeAssignStmt,
			ProceduralForceStmt,
			ProceduralReleaseStmt,
			SequentialBlockStmt,
			ParallelBlockStmt,
			CaseStmt,
			IfElseStmt,
			TaskFuncCallStmt,
			DisableStmt,
			EvenTriggerStmt,
			ForeverStmt,
			WhileStmt,         // Covers while and do while
			ForStmt,
			ForeachStmt,
			JumpStmt,
			DelayCtrlStmt,
			EventCtrlStmt,
			RepeatStmt,
			WaitStmt,
			DeclStmt,
			ConcatExpr,
			RangeExpr,
			SystemTaskFuncCallStmt,
			IncOrDecExpr,
			ConditionalExpr,
			SignalRefExpr,
			BinaryOpExpr,
			UnaryOpExpr,
			IntegerLiteralExpr,
			StringLiteralExpr
		};

		Stmt( StmtClass SC, llvm::SMRange range) : stmtClass(SC), sourceRange(range) {}
		virtual ~Stmt()  {
			for( auto expr : children) {
				if( expr != nullptr ) {
					delete expr;
					expr = nullptr;
				}
			}
		};
		void insert(Stmt* child);
	private:
		llvm::SMRange sourceRange;
		StmtClass stmtClass;
		std::vector<Stmt*> children;
	};

	class Expr : public Stmt {
	public:
		Expr(StmtClass SC, llvm::SMRange range ) : Stmt(SC, range) {}
		virtual ~Expr() {};

		virtual bool isConstant() = 0;

	private:
	};

	class ConcatExpr : public Expr {
	public:
	};

	class MultipleConcatExpr : public Expr {
	public:
	};

	// Constant integer number
	// TODO: Add support for Z/X
	class IntegerLiteralExpr : public Expr {
	public:
		IntegerLiteralExpr(llvm::SMRange range, int width, int radix, llvm::StringRef str)
			: Expr(StmtClass::IntegerLiteralExpr, range), val(width, str, radix) {}
		~IntegerLiteralExpr(){}
		bool isConstant() { return true; }
	private:
		llvm::APInt val;
	};

	class RangeExpr : public Expr {
	public:
		enum RangeKind {
			SingleRange,
			MsbLsbRange,
			AddRange,
			SubRange
		};
		RangeExpr(llvm::SMRange range, RangeKind k, Expr *l, Expr *r) :
			Expr(StmtClass::RangeExpr, range), kind(k) {
				insert(l);
				insert(r);
		}
		~RangeExpr() {}
	private:
		RangeKind kind;
	};

	class AssignStmt : public Stmt {
	public:
		enum AssignKind {
			Blocking,
			NonBlocking,
			Assign,
			ProceduralContinuousAssign
		};

		AssignStmt(llvm::SMRange range, AssignKind k, Expr *rValue, Expr *lValue) :
			Stmt(StmtClass::AssignStmt, range), kind(k) {
				insert(rValue);
				insert(lValue);
		}
		~AssignStmt(){}
	private:
		AssignKind kind;
	};

};



#endif