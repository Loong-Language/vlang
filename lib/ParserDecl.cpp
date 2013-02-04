

#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/ADT/Twine.h>


#include "Parser.h"
#include "Lexer.h"
#include "Tokens.h"
#include "Namespace.h"
#include "Declaration.h"
#include "Semantic.h"

using namespace vlang;

// Parses one of these and stops at commas
//                                                                       data_type_or_implicit
// var                                                                   data_type_or_implicit
// net_type [ drive_strength | charge_strength ] [ vectored | scalared ] data_type_or_implicit [ delay3 ]
// [ const ] [ var ] [ lifetime ]                                        data_type_or_implicit

DeclTypeResult Parser::parse_declaration_type_info(){

	auto dataType = DataType::Unknown;
	auto netType  = NetType::Unknown;

	auto declarationTokenStart = token;

	bool isConst  = check_for_token_and_consume(TokenKind::kw_const);
	bool isVar    = check_for_token_and_consume(TokenKind::kw_var);
	auto lifetime = parse_lifetime();

	// TODO: Handle cases where the order is incorrect
	netType = parse_net_type();
	if( netType != NetType::Unknown) {
		// Check for strength
		
		// Check for vectored/scalared
	}

	auto rType = parse_data_type_or_implicit(netType);

	// TODO: Check for delay3
	if( rType.isInvalid()) {
		printf("ERROR: Couldn't parse data type\n");
		process_error();
	}
	
	// Expect an identifier
	if( !check_for_token(TokenKind::identifier) ) {
		printf("ERROR: Expected identifier after data type\n");
		process_error();
	}

	// If the next token is an identifier, then the current one is a
	//   undeclared type, this is only allowed for a few types
	if( check_for_next_token(TokenKind::identifier) ){
		printf("ERROR: User defined types are not supported\n");
		process_error();
	}

	// Failed to find a type, so it's an implicit, with 0 dimension

	// HACK: Testing token range
	auto type = rType.get();
	llvm::SMRange r = type->getSourceRange();
	llvm::ArrayRef<llvm::SMRange> Ranges = llvm::ArrayRef<llvm::SMRange>( r );
	DeclTypeInfo *declType = new DeclTypeInfo(rType.get());

	return DeclTypeResult(declType);
}

// data_type_or_implicit ::=   data_type
//                         |   implicit_data_type
// implicit_data_type    ::= [ signing ] { packed_dimension }
TypeResult Parser::parse_data_type_or_implicit(NetType nType)
{
	auto tokenStart = token;
	auto rType = parse_data_type(nType);
	if( rType.isUsable()) {
		return rType;
	}

	// Parse implicit data type
	SigningType sType = parse_signing();

	if( check_for_token(TokenKind::l_square)){
		auto dimResult = parse_dimension();
		auto rType = new TypeInfo(DataType::Implicit, nType, sType, dimResult.get() );
		rType->setSourceRange(getTokenRange(tokenStart, token));
		return TypeResult(rType);
	}

	auto *type = new TypeInfo(DataType::Implicit, nType, sType);
	return TypeResult(type);
}

// net_declaration ::=   net_type [ drive_strength | charge_strength ] [ vectored | scalared ]
//                                                      data_type_or_implicit [ delay3 ] list_of_net_decl_assignments ;
// data_declaration ::=  [ const ] [ var ] [ lifetime ] data_type_or_implicit list_of_variable_decl_assignments ;
//                    |  type_declaration              -- TODO
//                    |  package_import_declaration    -- TODO
//                    |  virtual_interface_declaration -- TODO
// list_of_*_decl_assignments ::= *_decl_assignment { , *_decl_assignment }
// net_decl_assignment ::= net_identifier { unpacked_dimension } [ = expression ]
// variable_decl_assignment ::= variable_identifier { variable_dimension } [ = expression ]
//                            | dynamic_array_variable_identifier unsized_dimension { variable_dimension } [ = dynamic_array_new ]
//                            | class_variable_identifier [ = class_new ]
// parameter_declaration ::=   parameter data_type_or_implicit list_of_param_assignments
//                         | parameter type list_of_type_assignments
bool Parser::parse_data_declaration_list(){
	auto declTypeResult = parse_declaration_type_info();

    // Failed to get declaration type
	if( declTypeResult.isInvalid()) {
		return false;
	}

	// Parse list_of_*_decl_assignments
	llvm::StringRef ident;
	do {
        Token *tokenStart = token;
		if( !parse_identifier(&ident)) {
			printf("ERROR: Expected identifier in declaration\n");
			process_error();
		}

		// Parse array dimensions
		while( check_for_token(TokenKind::l_square)) {
			auto dimResult = parse_dimension();
			if( dimResult.isUsable() ) {
				declTypeResult.get()->addArrayDimension(*dimResult.get());
			}
		}

		// Parse initial assignment if it exists
		ExprResult expr(true);
		if( check_for_token_and_consume(TokenKind::equal)) {
			expr = parse_expression(Precedence::Assignment);

			// TODO: Goto ',' or ';' on error
		}

		if(expr.isUsable()) {
			sema->ActOnDataDeclaration(getTokenRange(tokenStart, token), ident, declTypeResult.get(), expr.release() );
		} else {
			sema->ActOnDataDeclaration(getTokenRange(tokenStart, token), ident, declTypeResult.get(), nullptr );
		}
	} while( check_for_token_and_consume(TokenKind::comma));
	
	if( !check_for_token_and_consume(TokenKind::semicolon)) {
		printf("ERROR: Expected ';'\n");
		process_error();
	}

	return true;
}

bool Parser::parse_package_import_declaration()
{
	return false;
}
bool Parser::parse_import_item()
{
	return false;
}
bool Parser::parse_package_export_declaration()
{
	return false;
}
bool Parser::parse_genvar_declaration()
{
	return false;
}

bool Parser::parse_type_declaration()
{
	return false;
}

DeclLifetime Parser::parse_lifetime(){
	if( check_for_token_and_consume ( TokenKind::kw_static ) ) {
		return DeclLifetime::Static;
	} else if ( check_for_token_and_consume( TokenKind::kw_automatic ) ) {
		return DeclLifetime::Automatic;
	}

	return DeclLifetime::Unknown;
}

// Section A.2.2 - Declaration data types
// Section A.2.2.1 - Net/Variable types
bool Parser::parse_casting_type()
{
	return false;
}

// data_type ::=
//   integer_vector_type [ signing ] { packed_dimension }
// | integer_atom_type [ signing ]
// | non_integer_type
// | struct_union [ packed [ signing ] ] { struct_union_member { struct_union_member } } -- TODO
//   { packed_dimension }
// | enum [ enum_base_type ] { enum_name_declaration { , enum_name_declaration } } -- TODO
//   { packed_dimension }
// | string -- TODO
// | chandle -- TODO
// | virtual [ interface ] interface_identifier -- TODO
// | [ class_scope | package_scope ] type_identifier { packed_dimension } -- TODO
// | class_type -- TODO
// | event
// | ps_covergroup_identifier -- TODO
// | type_reference -- TODO
TypeResult Parser::parse_data_type(NetType nType)
{
	DataType  dType = DataType::Unknown;

	switch( token->kind ) {
	case TokenKind::kw_bit:
		dType = DataType::Bit;
		break;
	case TokenKind::kw_logic:
		dType = DataType::Logic;
		break;
	case TokenKind::kw_reg:
		dType = DataType::Reg;
		break;
	case TokenKind::kw_byte:
		dType = DataType::Byte;
		break;
	case TokenKind::kw_shortint:
		dType = DataType::ShortInt;
		break;
	case TokenKind::kw_int:
		dType = DataType::Int;
		break;
	case TokenKind::kw_longint:
		dType = DataType::LongInt;
		break;
	case TokenKind::kw_integer:
		dType = DataType::Integer;
		break;
	case TokenKind::kw_time:
		dType = DataType::Time;
		break;
	case TokenKind::kw_event:
		dType = DataType::Event;
		break;
	case TokenKind::kw_shortreal:
		dType = DataType::Shortreal;
		break;
	case TokenKind::kw_real:
		dType = DataType::Real;
		break;
	case TokenKind::kw_realtime:
		dType = DataType::RealTime;
		break;
	default:
		break;
	}

	// Return failure if data type is unknown.  Implicit nets will be
	//   handled by caller
	if(dType == DataType::Unknown ) {
		return TypeResult(true);
	}

	// Consume the type token
	consume_token();

	// Get the signing type
	SigningType sType = parse_signing();
	
	// Get the dimension if it exists
	DimResult   dResult;
	if( check_for_token(TokenKind::l_square) ){
		dResult = parse_dimension();
	}

	// TODO: Add error checking for multiple dimensions

    // Construct TypeInfo object
	TypeInfo *rType = nullptr;
	if( dResult.isUsable() ) {
		auto dim = dResult.get();
		TypeInfo *rType = new TypeInfo(dType, nType, sType, dResult.get());
	} else {
		TypeInfo *rType = new TypeInfo(dType, nType, sType);
	}

	return TypeResult(rType);
}

bool Parser::parse_enum_base_type()
{
	return false;
}
bool Parser::parse_enum_name_declaration()
{
	return false;
}
bool Parser::parse_class_scope()
{
	return false;
}
bool Parser::parse_class_type()
{
	return false;
}

// net_type ::= supply0 | supply1 | tri | triand | trior | trireg| tri0 | tri1 | uwire| wire | wand | wor
NetType Parser::parse_net_type()
{
	NetType rType = NetType::Unknown;

	switch( token->kind ) {
	case TokenKind::kw_supply0:
		rType = NetType::Supply0;
		break;
	case TokenKind::kw_supply1:
		rType = NetType::Supply1;
		break;
	case TokenKind::kw_tri:
		rType = NetType::Tri;
		break;
	case TokenKind::kw_triand:
		rType = NetType::TriAnd;
		break;
	case TokenKind::kw_trior:
		rType = NetType::TriOr;
		break;
	case TokenKind::kw_trireg:
		rType = NetType::TriReg;
		break;
	case TokenKind::kw_tri0:
		rType = NetType::Tri0;
		break;
	case TokenKind::kw_tri1:
		rType = NetType::Tri1;
		break;
	case TokenKind::kw_uwire:
		rType = NetType::Uwire;
		break;
	case TokenKind::kw_wire:
		rType = NetType::Wire;
		break;
	case TokenKind::kw_wand:
		rType = NetType::Wand;
		break;
	case TokenKind::kw_wor:
		rType = NetType::Wor;
		break;
	default:
		rType = NetType::Unknown;
		break;
	}

	if( rType != NetType::Unknown) {
		consume_token();
	}

	return rType;
}

SigningType Parser::parse_signing()
{
	if( check_for_token_and_consume( TokenKind::kw_signed ) ) {
		return SigningType::Signed;
	} else if( check_for_token_and_consume( TokenKind::kw_unsigned ) ) {
		return SigningType::Unsigned;
	}

	return SigningType::Unknown;
}
bool Parser::parse_struct_union_member()
{
	return false;
}
bool Parser::parse_data_type_or_void()
{
	return false;
}
bool Parser::parse_struct_union()
{
	return false;
}
bool Parser::parse_type_reference()
{
	return false;
}