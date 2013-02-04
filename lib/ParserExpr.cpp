#include <stdio.h>
#include <assert.h>
#include <limits>

#include "Parser.h"
#include "Lexer.h"
#include "Tokens.h"
#include "Ast.h"
#include "Semantic.h"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/ADT/Twine.h>

#define UNIMPLMENETED_PARSE_EXPR(n) ExprResult Parser::n (){  return ExprResult(true); }

using namespace vlang;

Precedence getOpPrecedence(TokenKind kind, bool unaryOp, bool allowAssign){
	switch(kind){
	default: return Precedence::Unknown;

	case TokenKind::less_equal:
		return (allowAssign) ? Precedence::Assignment : Precedence::Relational;
		break;

	case TokenKind::plus:
	case TokenKind::minus:
		return (unaryOp) ? Precedence::Unary : Precedence::Additive;
		break;

	case TokenKind::bit_and:
		return (unaryOp) ? Precedence::Unary : Precedence::BitAnd;
		break;

	case TokenKind::bit_xor:
	case TokenKind::invert_xor:
	case TokenKind::invert_xor2:
		return (unaryOp) ? Precedence::Unary : Precedence::BitXor;
		break;

	case TokenKind::bit_or:
		return (unaryOp) ? Precedence::Unary : Precedence::BitOr;
		break;

	case TokenKind::logic_not:
	case TokenKind::invert:
	case TokenKind::invert_and:
	case TokenKind::invert_or:
	case TokenKind::plus_plus:
	case TokenKind::minus_minus:
		return Precedence::Unary;
		break;

	case TokenKind::star_star:
		return Precedence::Power;
		break;

	case TokenKind::multiply:
	case TokenKind::divide:
	case TokenKind::modulo:
		return Precedence::Multiplicative;
		break;

	case TokenKind::less_less:
	case TokenKind::greater_greater:
	case TokenKind::less_less_less:
	case TokenKind::greater_greater_greater:
		return Precedence::Shift;
		break;

	case TokenKind::less:
	case TokenKind::greater:
	case TokenKind::greater_equal:
	case TokenKind::kw_inside:
	case TokenKind::kw_dist:
		return Precedence::Relational;
		break;

	case TokenKind::equal_equal:
	case TokenKind::invert_equal:
	case TokenKind::equal_equal_equal:
	case TokenKind::invert_equal_equal:
	case TokenKind::equal_equal_question:
	case TokenKind::invert_equal_question:
		return Precedence::Equality;
		break;

	case TokenKind::and_and:
		return Precedence::LogicalAnd;
		break;

	case TokenKind::or_or:
		return Precedence::LogicalOr;
		break;

	case TokenKind::conditional:
	case TokenKind::colon:
		return Precedence::Conditional;
		break;

	case TokenKind::implication:
	case TokenKind::less_minus_greater:
		return Precedence::Implication;
		break;

	case TokenKind::equal:
	case TokenKind::plus_equal:
	case TokenKind::minus_equal:
	case TokenKind::multiply_equal:
	case TokenKind::divide_equal:
	case TokenKind::modulo_equal:
	case TokenKind::xor_equal:
	case TokenKind::or_equal:
	case TokenKind::less_less_equal:
	case TokenKind::greater_greater_equal:
	case TokenKind::less_less_less_equal:
	case TokenKind::greater_greater_greater_equal:
	case TokenKind::colon_equal:
	case TokenKind::colon_divide:
		return Precedence::Assignment;
		break;

	case TokenKind::l_brace:
	case TokenKind::r_brace:
		return Precedence::Assignment;
		break;
	}
}

//////////////////////////////////////
// Section A.8 Expressions
//////////////////////////////////////
// Section A.8.1 - Concatenations

// concatenation ::= { expression { , expression } }
// multiple_concatenation ::= { expression concatenation }
// NOTE: In a multiple_concatenation, it shall be illegal for the multiplier not to be a constant_expression unless the type of
//    the concatenation is string.
ExprResult Parser::parse_concatenation( )
{
    assert( check_for_token(TokenKind::l_brace) && "'{' Required to enter parse_concatenation\n");
	
	auto lBraceLoc = consume_token();
	
	ExprVector ExprList;
	std::vector<llvm::SMLoc> CommaList;
	
	if( !parse_list_of_expressions(ExprList, CommaList) ) {
		printf( "ERROR: Expected expression in concatenation\n" );
		process_error();
		return ExprResult(true);
	}

	// Check for multiple concat
	if( ExprList.size() == 1 && check_for_token(TokenKind::r_brace) ) {
		auto internalConcat = parse_concatenation();
		if( !check_for_token(TokenKind::r_brace) ) {
			printf( "ERROR: Expected '}' to close concatenation\n" );
			process_error();
		}

		auto rBraceLoc = consume_token();
		return sema->ActOnMultipleConcatenation(lBraceLoc.Start, rBraceLoc.Start, ExprList.front(), internalConcat.release());

    // Otherwise it is a normal concatenation
	} else {
		if( !check_for_token(TokenKind::r_brace) ) {
			printf( "ERROR: Expected '}' to close concatenation\n" );
			process_error();
		}

		auto rBraceLoc = consume_token();

		return sema->ActOnConcatenation(lBraceLoc.Start, rBraceLoc.Start, ExprList, CommaList);
	}


	assert(0 && "Should never hit default return in parse_concatenation\n");
	return ExprResult(true);
}
UNIMPLMENETED_PARSE_EXPR(parse_streaming_concatenation)
UNIMPLMENETED_PARSE_EXPR(parse_stream_operator)
UNIMPLMENETED_PARSE_EXPR(parse_slice_size)
UNIMPLMENETED_PARSE_EXPR(parse_stream_concatenation)
UNIMPLMENETED_PARSE_EXPR(parse_stream_expression)
UNIMPLMENETED_PARSE_EXPR(parse_array_range_expression)
UNIMPLMENETED_PARSE_EXPR(parse_empty_queue)

// expression { , expression }
bool Parser::parse_list_of_expressions(ExprVector &Exprs, std::vector<llvm::SMLoc> &CommaLocs){
	while(1) {
		ExprResult Expr = parse_expression(Precedence::Assignment);
		if( Expr.isInvalid() ) {
			return false;
		}

		Exprs.push_back(Expr.release());
		if( !check_for_token(TokenKind::comma) ) {
			return true;
		}

		CommaLocs.push_back(getTokenRange(token).Start);
		consume_token();
	}
}

// Section A.8.2 - Subroutine calls

// tf_call ::= ps_or_hierarchical_tf_identifier { attribute_instance } [ ( list_of_arguments ) ]
ExprResult  Parser::parse_tf_call()
{
	//if( !parse_hierarchical_identifier() ) {
	//	printf( "ERROR: Expected task identifier\n" );
	//	process_error();
	//}

	auto result = ExprResult(false);

	if( check_for_token_and_consume( TokenKind::l_paren ) ) {
		if( !check_for_token(TokenKind::r_paren) ) {
			auto listResult = parse_list_of_arguments();
		}

		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' to close task/function call\n" );
			process_error();
		}
	}
	return result;
}

// system_tf_call ::=
//   system_tf_identifier [ ( list_of_arguments ) ]
// | system_tf_identifier ( data_type [ , expression ] )
ExprResult  Parser::parse_system_tf_call()
{
	assert((token->kind == TokenKind::systask) && "Systask token is required\n");
	consume_token();
	auto result = ExprResult(false);

	if( check_for_token_and_consume( TokenKind::l_paren ) ) {
		auto listResult = parse_list_of_arguments();
		if( listResult.isInvalid() ) {
			printf( "ERROR: Expected some arguments to display call\n" );
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected '(' to close display call\n" );
			process_error();
			if( !listResult.isInvalid() ) { 	}
		}
	}
	return result;
}

// subroutine_call ::=
//   tf_call
// | system_tf_call
// | method_call
// | [ std:: ] randomize_call -- TODO
ExprResult  Parser::parse_subroutine_call()
{
	if( token->kind == TokenKind::systask ) {
		return parse_system_tf_call();
	}
	
    auto result = parse_method_call();

	if( !result.isInvalid() ) {
		return result;
	}

	return parse_tf_call();
}

// list_of_arguments ::=
//   [ expression ] { , [ expression ] } { , . identifier ( [ expression ] ) }
// | . identifier ( [ expression ] ) { , . identifier ( [ expression ] ) }      -- TODO
ExprResult Parser::parse_list_of_arguments()
{
	auto result = ExprResult(false);
	bool firstLoop = true;

	do{
		// Seen an empty expressions
		if( !firstLoop && result.isInvalid()) {
		}

		// Parse optional expression
		auto argumentResult = parse_expression(Precedence::Concatenation);
		if( !argumentResult.isInvalid() ) {
		}
	} while( check_for_token_and_consume(TokenKind::comma) );

	return result;
}

UNIMPLMENETED_PARSE_EXPR(parse_method_call)
UNIMPLMENETED_PARSE_EXPR(parse_method_call_body)
UNIMPLMENETED_PARSE_EXPR(parse_built_in_method_call)
UNIMPLMENETED_PARSE_EXPR(parse_array_manipulation_call)
UNIMPLMENETED_PARSE_EXPR(parse_randomize_call)
UNIMPLMENETED_PARSE_EXPR(parse_method_call_root)
UNIMPLMENETED_PARSE_EXPR(parse_array_method_call)

// Section A.8.3 - Expressions
UNIMPLMENETED_PARSE_EXPR(parse_inc_or_dec_expression)

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
ExprResult Parser::parse_expression(Precedence minPrec)
{

	// TODO: Check for unary operators
	// Check for primary
	ExprResult LHS = parse_primary_with_unary();
	if( LHS.isInvalid()) {
		printf("ERROR: Expected a primary in front of expression\n");
		process_error();
		return LHS;
	}

	return parse_rhs_expression(LHS, minPrec);

}

ExprResult Parser::parse_rhs_expression(ExprResult LHS, Precedence minPrec){
	//while cur token is a binary operator with precedence >= min_prec:prec,
	//		assoc = precedence and associativity of current token
	//   if assoc is left:
	//     next_min_prec = prec + 1
	//   else:
	//     next_min_prec = prec
	//	rhs = compute_expr(next_min_prec)
	//	result = compute operator(result, rhs)
	Precedence nextTokPrec = getOpPrecedence(token->kind, false, false);

	while(1){
		// Finish if precedence is less then MinPrec
		if( nextTokPrec < minPrec)
			return LHS;

		// Grab the token value
		TokenKind opTokenKind = token->kind;
		consume_token();

		ExprResult ConditionalMiddle(true);
		if( nextTokPrec == Precedence::Conditional) {
			if( check_for_token_and_consume(TokenKind::colon) ) {
				// LHS is select for condition, now we need to parse actual conditional
				ConditionalMiddle = parse_expression(Precedence::Conditional);
				if( !check_for_token_and_consume(TokenKind::colon) ) {
					printf("Expected ':' after conditional middle\n");
					process_error();
				}
			}
		}

		// Parse another leaf here for the RHS of the operator.
		ExprResult RHS =  parse_primary_with_unary();

		// Remember the precedence of this operator and get the precedence of the
		// operator immediately to the right of the RHS.
		Precedence currentPrec = nextTokPrec;
		nextTokPrec = getOpPrecedence(token->kind, false, false);
		bool isRightAssoc = false;

		// Get the precedence of the operator to the right of the RHS.  If it binds
		// more tightly with RHS than we do, evaluate it completely first.
		if( (currentPrec < nextTokPrec) ||
			((currentPrec == nextTokPrec) && isRightAssoc)) {
				RHS = parse_rhs_expression(RHS,static_cast<Precedence>(static_cast<int>(currentPrec) + !isRightAssoc));
				nextTokPrec = getOpPrecedence(token->kind, false, false);
		}
		assert(nextTokPrec <= currentPrec && "Recursion didn't work!");

		if( !LHS.isInvalid() ) {
			if(ConditionalMiddle.isInvalid() ) {
				LHS = sema->ActOnBinaryOperation(llvm::SMRange(), opTokenKind, LHS.get(), RHS.get());
			} else {
				LHS = sema->ActOnConditionalOperation(llvm::SMRange(), llvm::SMRange(), LHS.get(), ConditionalMiddle.get(), RHS.get());
			}
		}
	}

	return LHS;
}


UNIMPLMENETED_PARSE_EXPR(parse_tagged_union_expression)
UNIMPLMENETED_PARSE_EXPR(parse_inside_expression)
UNIMPLMENETED_PARSE_EXPR(parse_value_range)

// mintypmax_expression ::=
//   expression
// | expression : expression : expression
ExprResult Parser::parse_mintypmax_expression()
{
	auto result = parse_expression(Precedence::Assignment);

	if( !result.isInvalid() && check_for_token_and_consume( TokenKind::colon ) ) {
		auto localResult = parse_expression(Precedence::Assignment);
		if( localResult.isInvalid() ) {
			printf( "ERROR: Expected expression\n" );
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::colon ) ) {
			printf( "ERROR: Expected ':' between constant expressions\n" );
			process_error();
		}

		localResult = parse_expression(Precedence::Assignment);
		if( localResult.isInvalid() ) {
			printf( "ERROR: Expected expression\n" );
			process_error();
		}
	}
	return result;
}
UNIMPLMENETED_PARSE_EXPR(parse_part_select_range)
UNIMPLMENETED_PARSE_EXPR(parse_indexed_range)
UNIMPLMENETED_PARSE_EXPR(parse_genvar_expression)

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

ExprResult Parser::parse_primary_with_unary()
{

	// First Check for a possible unary operator
	auto unaryOperator = TokenKind::unknown;
	if( is_unary_operator() ) {
		unaryOperator = token->kind;
		consume_token();
	}

	ExprResult primary(true);
	switch(token->kind ){
		case TokenKind::l_brace:
			primary = parse_concatenation();
			if( check_for_token(TokenKind::l_square) ) {
				parse_dimension();
			}
			break;
		case TokenKind::identifier:
			if ( parse_hierarchical_identifier() ) {
				// Check if it is a function/subroutine call
				//  May still be a subroutine if '(' is missing
				// tf_call ::= ps_or_hierarchical_tf_identifier { attribute_instance } [ ( list_of_arguments ) ]
				if( check_for_token_and_consume(TokenKind::l_paren)) {
					if( !parse_list_of_arguments().isInvalid() ) {}
					if( !check_for_token_and_consume(TokenKind::r_paren)) {
						printf("ERROR: Expected ')' to close task/function\n");
						process_error();
					}
				} else {
					if( !parse_select_or_range().isInvalid() ) { }
				}
				return ExprResult(false);
			} else {
				printf("ERROR: Expected hierarchical identifier\n");
				return ExprResult(true);
			}
			break;
		case TokenKind::systask:
			return parse_system_tf_call();
			break;
 
		case TokenKind::l_paren:
			consume_token();
			result = parse_mintypmax_expression();
			if( result.isInvalid() ) {
				printf( "ERROR: Expected mintypmax expression\n" );
				process_error();
			}
			if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
				printf( "ERROR: Expected ')' to close subexpression\n" );
				process_error();
			}
			return result;
			break;
		default:
			return parse_primary_literal();
			break;
	}

	assert(0 && "Should never hit default return in parse_primary\n");

	// Invalid
	return ExprResult(true);
}
UNIMPLMENETED_PARSE_EXPR(parse_class_qualifier)

// primary_literal ::= number | time_literal | unbased_unsized_literal (TODO) | string_literal
// time_literal ::=
//    unsigned_number time_unit
//  | fixed_point_number time_unit
ExprResult  Parser::parse_primary_literal()
{
	if( check_for_token_and_consume(TokenKind::string_literal) ) {
		return ExprResult(false);
	}

	auto result = parse_number();
	if( !result.isInvalid() ){
		return result;
	}

	result = parse_time_literal();
	if( !result.isInvalid() ) {
		return result;
	}

	return ExprResult(true);
}
UNIMPLMENETED_PARSE_EXPR(parse_time_literal)
UNIMPLMENETED_PARSE_EXPR(parse_time_unit)
UNIMPLMENETED_PARSE_EXPR(parse_implicit_class_handle)

// bit_select ::= { [ expression ] }
// part_select_range ::= constant_range | indexed_range
// indexed_range ::=
//   expression +: constant_expression
// | expression -: constant_expression
// constant_range ::= constant_expression : constant_expression
// range_expression ::= expression | part_select_range
// select ::= [ { . member_identifier bit_select } . member_identifier ] bit_select [ [ part_select_range ] ]
ExprResult  Parser::parse_select_or_range(  )
{
	if( !check_for_token_and_consume( TokenKind::l_square ) ) {
		return ExprResult(true);
	}

	auto result = ExprResult();

	// constant_range
	if( lexer->checkTokenBetween( TokenKind::colon, TokenKind::r_square, TokenKind::semicolon ) ) {

		result = parse_expression(Precedence::Assignment);

		if( !check_for_token_and_consume( TokenKind::colon ) ) {
			printf( "ERROR: Expected ';' inbetween upper/lower values of range\n" );
			process_error();
		}

		auto parseResult = parse_expression(Precedence::Assignment);

    // bit_select
	} else {
		result = parse_expression(Precedence::Assignment);
		if( result.isInvalid() ) {
			printf( "ERROR: Expected expression in bit select\n" );
			process_error();
		}
	}

	if( !check_for_token_and_consume( TokenKind::r_square ) ) {
		printf( "ERROR: Expected ']' to close bit/rage select\n" );
		process_error();
	}

	return result;
}

UNIMPLMENETED_PARSE_EXPR(parse_constant_cast)
UNIMPLMENETED_PARSE_EXPR(parse_constant_let_expression)
UNIMPLMENETED_PARSE_EXPR(parse_cast)

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
ExprResult  Parser::parse_lvalue(bool net_lvalue)
{
	llvm::StringRef ident;

	// Handles
	if( check_for_token_and_consume( TokenKind::l_brace ) ) {
		do {
			if( parse_lvalue(true).isInvalid() ) {
				printf( "ERROR: Expected net variable for assign\n" );
				process_error();
			}
		} while ( check_for_token_and_consume( TokenKind::comma ) );
		if( !check_for_token_and_consume( TokenKind::r_brace ) ) {
			printf( "ERROR: Expected '}' to close left hand side variable\n" );
			process_error();
		}
	} else {
		if( !parse_hierarchical_identifier( ) ) {
			return ExprResult(true);
		}

		auto result = parse_select_or_range();
	}

	return ExprResult(false);
}

// Section A.8.6 - Operators

// unary_operator ::= + | - | ! | ~ | & | ~& | | | ~| | ^ | ~^ | ^~
bool  Parser::is_unary_operator()
{
	switch( token->kind ) {
	case TokenKind::plus:
	case TokenKind::minus:
	case TokenKind::logic_not:
	case TokenKind::invert:
	case TokenKind::bit_and:
	case TokenKind::invert_and:
	case TokenKind::bit_or:
	case TokenKind::invert_or:
	case TokenKind::bit_xor:
	case TokenKind::invert_xor:
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
bool Parser::is_binary_operator()
{
	switch( token->kind ) {
	case TokenKind::plus:
	case TokenKind::minus:
	case TokenKind::multiply:
	case TokenKind::divide:
	case TokenKind::modulo:
	case TokenKind::equal_equal:
	case TokenKind::invert_equal:
	case TokenKind::equal_equal_equal:
	case TokenKind::invert_equal_equal:
	case TokenKind::equal_equal_question:
	case TokenKind::invert_equal_question:
	case TokenKind::and_and:
	case TokenKind::or_or:
	case TokenKind::star_star:
	case TokenKind::less:
	case TokenKind::less_equal:
	case TokenKind::greater:
	case TokenKind::greater_equal:
	case TokenKind::bit_and:
	case TokenKind::bit_or:
	case TokenKind::bit_xor:
	case TokenKind::invert_xor:
	case TokenKind::invert_xor2:
	case TokenKind::greater_greater:
	case TokenKind::less_less:
	case TokenKind::invert_and:
	case TokenKind::invert_or:
		return true;
		break;
	default:
		break;
	}
	return false;
}
UNIMPLMENETED_PARSE_EXPR(parse_inc_or_dec_operator)

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

ExprResult  Parser::parse_number()
{
	Token *baseToken = token;
	bool exp_constant = false;

	// Check to see if the number is the [ size ]
	if( token->kind == TokenKind::numeric_constant ) {
		baseToken = get_next_token();
	}

	// Check to see if we have a base
	int  radix = 10;
	bool allow_sign = false;
	bool has_base   = true;
	switch( baseToken->kind ) {
	case TokenKind::base_signed_binary:
		allow_sign = true;
	case TokenKind::base_binary:
		radix     = 2;
		break;

	case TokenKind::base_signed_decimal:
		allow_sign = true;
	case TokenKind::base_decimal:
		radix = 10;
		break;

	case TokenKind::base_signed_oct:
		allow_sign = true;
	case TokenKind::base_oct:
		radix = 8;
		break;

	case TokenKind::base_signed_hex:
		allow_sign = true;
	case TokenKind::base_hex:
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
		if( token->kind == TokenKind::numeric_constant ) {
			auto arbitraty_int = new llvm::APInt(32, token->tokenString, 10 ) ;
			bitwidth = arbitraty_int->getLimitedValue(std::numeric_limits<int>::max());
			consume_token();
		}

		// Consume the base
		consume_token();
	}

	IntegerLiteralExpr *literalExpr = nullptr;
	switch( token->kind ) {
	case TokenKind::numeric_constant_xz:
		consume_token();
		return ExprResult(false);
		break;
	case TokenKind::numeric_constant:
		literalExpr = new IntegerLiteralExpr(getTokenRange(baseToken, token), bitwidth, radix, token->tokenString);
		consume_token();
		return ExprResult(literalExpr);
		break;

    // If an identifier has Z|z|X|x|Z|z then it could be okay
	case TokenKind::identifier:
		if( exp_constant ) {
			consume_token();
			return ExprResult(false);
		}
	default:
		break;
	}

	if( exp_constant ) {
		printf( "ERROR: Expected constant after size and base\n" );
		process_error();
	}
	return ExprResult(true);
}

// Section A.8.8 - Strings
// TODO
