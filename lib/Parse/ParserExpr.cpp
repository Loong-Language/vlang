#include <stdio.h>
#include <assert.h>
#include <limits>

#include "vlang/Parse/Parser.h"
#include "vlang/Basic/TokenKinds.h"
#include "vlang/Parse/ParseDiagnostic.h"
#include "vlang/Sema/Sema.h"
#include "vlang/Basic/OperatorPrecedence.h"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/ADT/Twine.h>

#define UNIMPLMENETED_PARSE_EXPR(n) ExprResult Parser::n (){  return ExprResult(true); }

using namespace vlang;
using namespace prec;

//////////////////////////////////////
// Section A.8 Expressions
//////////////////////////////////////
// Section A.8.1 - Concatenations

// concatenation ::= { expression { , expression } }
// multiple_concatenation ::= { expression concatenation }
// NOTE: In a multiple_concatenation, it shall be illegal for the multiplier not to be a constant_expression unless the type of
//    the concatenation is string.
ExprResult Parser::ParseConcatenation( )
{
    assert( Tok.is(tok::l_brace) && "'{' Required to enter ParseConcatenation\n");
	ConsumeBrace();
	
	ExprVector ExprList;

	if( !ParseListOfExpressions(ExprList) ) {
      Diag(Tok, diag::err_expected_expression);
      SkipUntil(tok::comma, tok::r_brace, false, true);
		return ExprResult(false);
	}

   // Multiple concat
   if( Tok.is(tok::l_brace) ) {

      if( ExprList.size() != 1){
         Diag(Tok, diag::err_expected_one_expr_for) << "multiple concatenation";
      }
		auto internalConcat = ParseConcatenation();

		//result = sema->ActOnMultipleConcatenation(lBraceLoc.Start, rBraceLoc.Start, ExprList.front(), internalConcat.release());
      return ExprResult(false);
    // Otherwise it is a normal concatenation
	}

   ExpectAndConsume(tok::r_brace, diag::err_expected_rparen, "", tok::r_brace);
   // TODO: Handle error correctly
   //result = sema->ActOnConcatenation(lBraceLoc.Start, rBraceLoc.Start, ExprList, CommaList);

   return ExprResult(false);
}
UNIMPLMENETED_PARSE_EXPR(ParseStreamingConcatenation)
UNIMPLMENETED_PARSE_EXPR(ParseStreamOperator)
UNIMPLMENETED_PARSE_EXPR(ParseSliceSize)
UNIMPLMENETED_PARSE_EXPR(ParseStreamConcatenation)
UNIMPLMENETED_PARSE_EXPR(ParseStreamExpression)
UNIMPLMENETED_PARSE_EXPR(ParseArrayRangeExpression)
UNIMPLMENETED_PARSE_EXPR(ParseEmptyQueue)

// expression { , expression }
bool Parser::ParseListOfExpressions(ExprVector &Exprs){
	while(1) {
		ExprResult Expr = ParseExpression(prec::Assignment);
		if( Expr.isInvalid() ) {
			return false;
		}

		Exprs.push_back(Expr.release());
		if( Tok.isNot(tok::comma) ) {
			return true;
		}

      ConsumeToken();
	}
}

// Section A.8.2 - Subroutine calls

// tf_call ::= ps_or_hierarchical_tf_identifier { attribute_instance } [ ( list_of_arguments ) ]
ExprResult  Parser::ParseTfCall(bool parse_ident, llvm::StringRef ident)
{
   if( parse_ident) {
      if( !ParseHierarchicalIdentifier() ) {
         return ExprResult(true);
      }
   }

   if( Tok.is(tok::l_paren)) {
      ConsumeParen();
		if( Tok.isNot(tok::r_paren) ) {
			auto listResult = ParseListOfArguments();
		}

      ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   }
	return ExprResult(false);
}

// list_of_arguments ::=
//   [ expression ] { , [ expression ] } { , . identifier ( [ expression ] ) }
// | . identifier ( [ expression ] ) { , . identifier ( [ expression ] ) }      -- TODO
ExprResult Parser::ParseListOfArguments()
{
	auto result = ExprResult(false);
	bool firstLoop = true;

	do{
		// Seen an empty expressions
		if( !firstLoop && result.isInvalid()) {
		}

		// Parse optional expression
		auto argumentResult = ParseExpression(prec::Concatenation);
		if( !argumentResult.isInvalid() ) {
		}
	} while( ConsumeIfMatch(tok::comma) );

	return result;
}

UNIMPLMENETED_PARSE_EXPR(ParseMethodCall)
UNIMPLMENETED_PARSE_EXPR(ParseMethodCallBody)
UNIMPLMENETED_PARSE_EXPR(ParseBuiltInMethodCall)
UNIMPLMENETED_PARSE_EXPR(ParseArrayManipulationCall)
UNIMPLMENETED_PARSE_EXPR(ParseRandomizeCall)
UNIMPLMENETED_PARSE_EXPR(ParseMethodCallRoot)
UNIMPLMENETED_PARSE_EXPR(ParseArrayMethodCall)

// Section A.8.3 - Expressions
UNIMPLMENETED_PARSE_EXPR(ParseIncOrDecExpression)

// expression ::=
//   primary
// | unary_operator { attribute_instance } primary
// | inc_or_dec_expression
// | ( operator_assignment )
// | expression binary_operator { attribute_instance } expression
// | conditional_expression
// | inside_expression
// | tagged_union_expression

// constant_expression ::=
//   constant_primary
// | unary_operator { attribute_instance } constant_primary
// | constant_expression binary_operator { attribute_instance } constant_expression
// | constant_expression ? { attribute_instance } constant_expression : constant_expression

// conditional_expression ::= cond_predicate ? { attribute_instance } expression : expression
ExprResult Parser::ParseExpression(prec::Level minPrec)
{

	// TODO: Check for unary operators
	// Check for primary
	ExprResult LHS = ParsePrimaryWithUnary();
	if( LHS.isInvalid()) {
		return LHS;
	}

	return ParseRhsExpression(LHS, minPrec);

}

ExprResult Parser::ParseRhsExpression(ExprResult LHS, prec::Level minPrec){
	//while cur token is a binary operator with precedence >/= min_prec:prec,
	//		assoc = precedence and associativity of current token
	//   if assoc is left:
	//     next_min_prec = prec + 1
	//   else:
	//     next_min_prec = prec
	//	rhs = compute_expr(next_min_prec)
	//	result = compute operator(result, rhs)
	auto nextTokPrec = getBinOpPrecedence(Tok.getKind());

	while(1){
		// Finish if precedence is less then MinPrec
		//   Should never get a ":" here
		if( nextTokPrec < minPrec || Tok.is(tok::colon) || Tok.is(tok::r_brace) || Tok.is(tok::l_brace))
			return LHS;

		// Grab the token value
		auto opTokenKind = Tok.getKind();
		ConsumeAnyToken();

		ExprResult ConditionalMiddle(true);
		if( nextTokPrec == prec::Conditional) {
         // LHS is select for condition, now we need to parse actual conditional
         ConditionalMiddle = ParseExpression(prec::Conditional);
                
         if( ExpectAndConsume(tok::colon, diag::err_expected_ternery_colon)){
            SkipUntil(tok::colon, true, true);
            // Handle semicolon case
         }
		}

		// Parse another leaf here for the RHS of the operator.
		ExprResult RHS =  ParsePrimaryWithUnary();

		// Remember the precedence of this operator and get the precedence of the
		// operator immediately to the right of the RHS.
		auto currentPrec = nextTokPrec;
		nextTokPrec = getBinOpPrecedence(Tok.getKind());
		bool isRightAssoc = false;

		// Get the precedence of the operator to the right of the RHS.  If it binds
		// more tightly with RHS than we do, evaluate it completely first.
		if( (currentPrec < nextTokPrec) ||
			((currentPrec == nextTokPrec) && isRightAssoc)) {
				RHS = ParseRhsExpression(RHS,static_cast<prec::Level>(static_cast<int>(currentPrec) + !isRightAssoc));
				nextTokPrec = getBinOpPrecedence(Tok.getKind());
		}
		assert(nextTokPrec <= currentPrec && "Recursion didn't work!");

		if( !LHS.isInvalid() ) {
			if(ConditionalMiddle.isInvalid() ) {
				//LHS = sema->ActOnBinaryOperation(llvm::SMRange(), opTokenKind, LHS.get(), RHS.get());
			} else {
				//LHS = sema->ActOnConditionalOperation(llvm::SMRange(), llvm::SMRange(), LHS.get(), ConditionalMiddle.get(), RHS.get());
			}
		}
	}

	return LHS;
}


UNIMPLMENETED_PARSE_EXPR(ParseTaggedUnionExpression)
UNIMPLMENETED_PARSE_EXPR(ParseInsideExpression)
UNIMPLMENETED_PARSE_EXPR(ParseValueRange)

// mintypmax_expression ::=
//   expression
// | expression : expression : expression
ExprResult Parser::ParseMintypmaxExpression()
{
	auto result = ParseExpression(prec::Assignment);

	if( !result.isInvalid() && ConsumeIfMatch( tok::colon ) ) {
		auto localResult = ParseExpression(prec::Assignment);
        // TODO: Handle bad expression

        ExpectAndConsume(tok::colon, diag::err_expected_colon);
        // TODO: Handle missing colon

		localResult = ParseExpression(prec::Assignment);
        // TODO: Handle bad expression
	}
	return result;
}
UNIMPLMENETED_PARSE_EXPR(ParsePartSelectRange)
UNIMPLMENETED_PARSE_EXPR(ParseIndexedRange)
UNIMPLMENETED_PARSE_EXPR(ParseGenvarExpression)

// Section A.8.4 - Primarys

	// primary ::=
//   primary_literal
// | [ class_qualifier | package_scope ] hierarchical_identifier select
// | empty_queue
// | concatenation [ [ range_expression ] ]
// | multiple_concatenation [ [ range_expression ] ]
// | function_subroutine_call   --> subroutine_call
// | let_expression
// | ( mintypmax_expression )
// | cast
// | assignment_pattern_expression
// | streaming_concatenation
// | sequence_method_call
// | this
// | $
// | null

// constant_primary ::=
//   primary_literal
// | ps_parameter_identifier constant_select
// | specparam_identifier [ [ constant_range_expression ] ]
// | genvar_identifier
// | [ package_scope | class_scope ] enum_identifier
// | constant_concatenation [ [ constant_range_expression ] ]
// | constant_multiple_concatenation [ [ constant_range_expression ] ]
// | constant_function_call
// | constant_let_expression
// | ( constant_mintypmax_expression )
// | constant_cast
// | constant_assignment_pattern_expression
// | type_reference

ExprResult Parser::ParsePrimaryWithUnary()
{

	// First Check for a possible unary operator
	auto unaryOperator = tok::unknown;
   while( isUnaryOperator() ){
		unaryOperator = Tok.getKind();
		ConsumeToken();
	}

	ExprResult primary(true);
	switch(Tok.getKind() ){
		case tok::l_brace:
			primary = ParseConcatenation();
			if( Tok.is(tok::l_square) ) {
				ParseDimension();
			}
         return primary;

		case tok::identifier:
         ParseHierarchicalIdentifier();

         // Check if it is a function/subroutine call
         //  May still be a subroutine if '(' is missing
         // tf_call ::= ps_or_hierarchical_tf_identifier { attribute_instance } [ ( list_of_arguments ) ]
         if( Tok.is(tok::l_paren) ) {
            ConsumeParen();
            ParseListOfArguments();
                    
            ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
         } else {
            ParseSelectOrRange();
         }
         return ExprResult(false);
 
		case tok::l_paren:
            ConsumeParen();
            primary = ParseMintypmaxExpression();
            // TODO: Handle primary not valid
            
            ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
			return primary;

      // primary_literal ::= number | time_literal | unbased_unsized_literal | string_literal
      // time_literal    ::= unsigned_number time_unit
      //                   | fixed_point_number time_unit
      case tok::string_literal:
         ConsumeStringToken();
         return ExprResult(false);
      case tok::base_binary:  case tok::base_signed_binary:
      case tok::base_decimal: case tok::base_signed_decimal:
      case tok::base_hex:     case tok::base_signed_hex:
      case tok::base_oct:     case tok::base_signed_oct:
      case tok::numeric_constant: case tok::numeric_constant_xz:
         ParseNumber();
         // TODO: Handle time literal
         return ExprResult(false);
		default:
         // TODO: Handle error
			break;
	}

	// Invalid
	return ExprResult(true);
}
UNIMPLMENETED_PARSE_EXPR(ParseClassQualifier)

UNIMPLMENETED_PARSE_EXPR(ParseTimeLiteral)
UNIMPLMENETED_PARSE_EXPR(ParseTimeUnit)
UNIMPLMENETED_PARSE_EXPR(ParseImplicitClassHandle)

// bit_select ::= { [ expression ] }
// part_select_range ::= constant_range | indexed_range
// indexed_range ::=
//   expression +: constant_expression
// | expression -: constant_expression
// constant_range ::= constant_expression : constant_expression
// range_expression ::= expression | part_select_range
// select ::= [ { . member_identifier bit_select } . member_identifier ] bit_select [ [ part_select_range ] ]
ExprResult  Parser::ParseSelectOrRange(  )
{
    if( Tok.isNot(tok::l_square)){
        return ExprResult(true);
    }
    
    ConsumeBracket();

    // Get first expression value
    auto resultLeft = ParseExpression(prec::Assignment);
    
    if(ConsumeIfMatch(tok::colon)){
        auto resultRight = ParseExpression(prec::Assignment);
    }
    

    ExpectAndConsume(tok::r_square, diag::err_expected_rparen);
    // TODO: Handle missing ']'

	return ExprResult(false);
}

UNIMPLMENETED_PARSE_EXPR(ParseConstantCast)
UNIMPLMENETED_PARSE_EXPR(ParseConstantLetExpression)
UNIMPLMENETED_PARSE_EXPR(ParseCast)

// Section A.8.5 - Expression left-side values

// net_lvalue      ::=  ps_or_hierarchical_net_identifier constant_select
//                   | { net_lvalue { , net_lvalue } }
//                   | [ assignment_pattern_expression_type ] assignment_pattern_net_lvalue
//
// variable_lvalue ::= [ implicit_class_handle . | package_scope ] hierarchical_variable_identifier select
//                   | { variable_lvalue { , variable_lvalue } }
//                   | [ assignment_pattern_expression_type ] assignment_pattern_variable_lvalue
//                   | streaming_concatenation
//
// hierarchical_variable_identifier ::= hierarchical_identifier
// ps_or_hierarchical_net_identifier ::= [ package_scope ] net_identifier | hierarchical_net_identifier
ExprResult  Parser::ParseLvalue(bool net_lvalue)
{
	llvm::StringRef ident;

	// Handles
	if( Tok.is(tok::l_brace)){
        ConsumeBrace();
        do {
            ParseLvalue(true);
		} while ( ConsumeIfMatch(tok::comma));

        ExpectAndConsume(tok::r_brace, diag::err_expected_rparen);
	} else {
		if( !ParseHierarchicalIdentifier( ) ) {
			return ExprResult(true);
		}

		auto result = ParseSelectOrRange();
	}

	return ExprResult(false);
}

// Section A.8.6 - Operators

// unary_operator ::= + | - | ! | ~ | & | ~& | | | ~| | ^ | ~^ | ^~
bool  Parser::isUnaryOperator()
{
	switch( Tok.getKind() ) {
	case tok::plus:
	case tok::minus:
	case tok::exclaim:
	case tok::tilde:
   case tok::amp:
	case tok::tildeamp:
	case tok::pipe:
	case tok::tildepipe:
	case tok::caret:
	case tok::tildecaret:
		return true;
		break;
	default:
		break;
	}
	return false;
}

// binary_operator ::=
//   + | - | * | / | % | == | != | === | !== | ==? | !=? | && | || | **
// | < | <= | > | >= | & | | | ^ | ^~ | ~^ | >> | << | >>> | <<<
// | -> | <->
// ~& | ~|
bool Parser::isBinaryOperator()
{
	switch( Tok.getKind() ) {
	case tok::plus:
	case tok::minus:
	case tok::star:
	case tok::slash:
	case tok::percent:
	case tok::equalequal:
	case tok::exclaimequal:
	case tok::equalequalequal:
	case tok::exclaimequalequal:
	case tok::equalequalquestion:
	case tok::exclaimequalquestion:
	case tok::ampamp:
	case tok::pipepipe:
	case tok::starstar:
	case tok::less:
	case tok::lessequal:
	case tok::greater:
	case tok::greaterequal:
	case tok::amp:
	case tok::pipe:
	case tok::caret:
	case tok::tildecaret:
	case tok::carettilde:
	case tok::greatergreater:
	case tok::lessless:
	case tok::tildeamp:
	case tok::tildepipe:
		return true;
		break;
	default:
		break;
	}
	return false;
}
UNIMPLMENETED_PARSE_EXPR(ParseIncOrDecOperator)

// Section A.8.7 - Numbers
// number ::=
//     integral_number
//   | real_number
// integral_number ::=
//     decimal_number
//   | octal_number
//   | binary_number
//   | hex_number
// decimal_number ::=
//     unsigned_number
//   | [ size ] decimal_base unsigned_number
//   | [ size ] decimal_base x_digit { _ }
//   | [ size ] decimal_base z_digit { _ }
// binary_number ::= [ size ] binary_base binary_value
// octal_number ::= [ size ] octal_base octal_value
// hex_number ::= [ size ] hex_base hex_value
// sign ::= + | -
// size ::= non_zero_unsigned_number
// non_zero_unsigned_number ::= non_zero_decimal_digit { _ | decimal_digit}
// real_number ::=
//     fixed_point_number
//   | unsigned_number [ . unsigned_number ] exp [ sign ] unsigned_number
// fixed_point_number ::= unsigned_number . unsigned_number
// exp ::= e | E
// unsigned_number ::= decimal_digit { _ | decimal_digit }
// binary_value ::= binary_digit { _ | binary_digit }
// octal_value ::= octal_digit { _ | octal_digit }
// hex_value ::= hex_digit { _ | hex_digit }
// decimal_base ::= '[s|S]d | '[s|S]D
// binary_base ::= '[s|S]b | '[s|S]B
// octal_base ::= '[s|S]o | '[s|S]O
// hex_base ::= '[s|S]h | '[s|S]H
// non_zero_decimal_digit ::= 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
// decimal_digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
// binary_digit ::= x_digit | z_digit | 0 | 1
// octal_digit ::= x_digit | z_digit | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
// hex_digit ::= x_digit | z_digit | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | b | c | d | e | f | A | B | C | D | E | F
// x_digit ::= x | X
// z_digit ::= z | Z | ?
// unbased_unsized_literal ::= '0 | '1 | 'z_or_x

ExprResult  Parser::ParseNumber()
{

   auto baseToken = Tok;

   // Check to see if the number is the [ size ]
	if( Tok.is(tok::numeric_constant ) ) {
      baseToken = NextToken();
	}

	// Check to see if we have a base
	int  radix = 10;
	bool allow_sign = false;
	bool has_base   = true;
	switch( baseToken.getKind() ) {
	case tok::base_signed_binary:
		allow_sign = true;
	case tok::base_binary:
		radix     = 2;
		break;

	case tok::base_signed_decimal:
		allow_sign = true;
	case tok::base_decimal:
		radix = 10;
		break;

	case tok::base_signed_oct:
		allow_sign = true;
	case tok::base_oct:
		radix = 8;
		break;

	case tok::base_signed_hex:
		allow_sign = true;
	case tok::base_hex:
		radix = 16;
		break;
	default:
		has_base = false;
		break;
	}

	// Get bitwidth
	int bitwidth = 32;
	if( has_base ) {

        // Consume the bitwidth
		if( Tok.is(tok::numeric_constant) ){
            //auto arbitraty_int = new llvm::APInt(32, token->tokenString, 10 ) ;
			//bitwidth = arbitraty_int->getLimitedValue(std::numeric_limits<int>::max());
			ConsumeToken();
		}

		// Consume the base
		ConsumeToken();
	}

	//IntegerLiteralExpr *literalExpr = nullptr;
	switch( Tok.getKind() ) {
	case tok::numeric_constant_xz:
		ConsumeToken();
		return ExprResult(false);
		break;
	case tok::numeric_constant:
		//literalExpr = new IntegerLiteralExpr(getTokenRange(baseToken, token), bitwidth, radix, Tok.);
		ConsumeToken();
		//return ExprResult(literalExpr);
        return ExprResult(false);
		break;

    // If an identifier has Z|z|X|x|Z|z then it could be okay
	case tok::identifier:
      ConsumeToken();
      return ExprResult(false);
	default:
		break;
	}

	return ExprResult(true);
}

// Section A.8.8 - Strings
// TODO
