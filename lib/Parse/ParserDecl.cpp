

#include <llvm/ADT/ArrayRef.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/SMLoc.h>
#include <llvm/ADT/Twine.h>


#include "vlang/Parse/Parser.h"
#include "vlang/Parse/ParserResult.h"
#include "vlang/Parse/ParseDiagnostic.h"
#include "vlang/Basic/TokenKinds.h"
#include "vlang/Sema/Sema.h"

using namespace vlang;

// Parses one of these and stops at commas
//                                                                       data_type_or_implicit
// var                                                                   data_type_or_implicit
// net_type [ drive_strength | charge_strength ] [ vectored | scalared ] data_type_or_implicit [ delay3 ]
// [ const ] [ var ] [ lifetime ]                                        data_type_or_implicit

DeclTypeResult Parser::ParseDeclarationTypeInfo(){

	auto dataType = DataType::Unknown;
   auto declarationTokenStart = Tok;
   bool isConst  = ConsumeIfMatch(tok::kw_const);
   bool isVar    = ConsumeIfMatch(tok::kw_var);
	auto lifetime = ParseLifetime();

	// TODO: Handle cases where the order is incorrect
	auto netType = ParseNetType();
	if( netType != NetType::Unknown) {
		// Check for strength
		
		// Check for vectored/scalared
	}

	auto rType = ParseDataTypeOrImplicit(netType);

   // Check for drive/charge strength
   if(Tok.is(tok::l_paren) ){

   }

   if(Tok.is(tok::hash) ){
      ParseDelay3();
   }

   // Expect an identifier
	if( Tok.isNot(tok::identifier)){
      Diag(Tok, diag::err_expected_ident_for) << "declaration";

      // TODO: Handle error
      if( Tok.is(tok::eof)  ) {
         return DeclTypeResult(false);
      }
	}

   if( NextToken().is(tok::identifier)){
      Diag(Tok, diag::err_unsupported_feature) <<  "User defined types";
      // TODO: Handle error
   }

	// Failed to find a type, so it's an implicit, with 0 dimension

	// HACK: Testing token range
//	auto type = rType.get();
	//llvm::SMRange r = type->getSourceRange();
	//llvm::ArrayRef<llvm::SMRange> Ranges = llvm::ArrayRef<llvm::SMRange>( r );
//	DeclTypeInfo *declType = new DeclTypeInfo(rType.get());

//	return DeclTypeResult(declType);
   return DeclTypeResult(false);
}

// data_type_or_implicit ::=   data_type
//                         |   implicit_data_type
// implicit_data_type    ::= [ signing ] { packed_dimension }
TypeResult Parser::ParseDataTypeOrImplicit(NetType nType)
{
	auto tokenStart = Tok;
	auto rType = ParseDataType(nType);
	if( rType.isUsable()) {
		return rType;
	}

	// Parse implicit data type
	SigningType sType = ParseSigning();

	if( Tok.is(tok::l_square)){
		auto dimResult = ParseDimension();
		//auto rType = new TypeInfo(DataType::Implicit, nType, sType, dimResult.get() );
		//rType->setSourceRange(getTokenRange(tokenStart, token));
      return TypeResult(false);
		//return TypeResult(rType);
	}

	//auto *type = new TypeInfo(DataType::Implicit, nType, sType);
	return TypeResult(false);
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
bool Parser::ParseDataDeclarationList(){
   if( Tok.is(tok::kw_parameter) || Tok.is(tok::kw_specparam) || Tok.is(tok::kw_localparam) ){
      ConsumeToken();
   }

	auto declTypeResult = ParseDeclarationTypeInfo();

    // Failed to get declaration type
	if( declTypeResult.isInvalid()) {
		return false;
	}

	// Parse list_of_*_decl_assignments
	llvm::StringRef ident;
	do {
      if( Tok.isNot(tok::identifier) ){
         Diag(Tok, diag::err_expected_ident_for) << "Declaration";
         // TODO: Handle error
      } else {
         ParseIdentifier(&ident);
      }

		// Parse array dimensions
		while( Tok.is(tok::l_square)) {
			auto dimResult = ParseDimension();
			if( dimResult.isUsable() ) {
				//declTypeResult.get()->addArrayDimension(*dimResult.get());
			}
		}

		// Parse initial assignment if it exists
		ExprResult expr(true);
		if( Tok.is(tok::equal)){
         ConsumeToken();
			expr = ParseExpression(prec::Assignment);

         // Skip until "," or ";"
         if(expr.isInvalid()){
            SkipUntil(tok::comma, true);
         }
		}

		if(expr.isUsable()) {
			//sema->ActOnDataDeclaration(getTokenRange(tokenStart, token), ident, declTypeResult.get(), expr.release() );
		} else {
			//sema->ActOnDataDeclaration(getTokenRange(tokenStart, token), ident, declTypeResult.get(), nullptr );
		}
	} while( ConsumeIfMatch(tok::comma));
	
   ExpectAndConsumeSemi(diag::err_expected_semi_after_decl);

	return true;
}

bool Parser::ParsePackageImportDeclaration()
{
	return false;
}
bool Parser::ParseImportItem()
{
	return false;
}
bool Parser::ParsePackageExportDeclaration()
{
	return false;
}
bool Parser::ParseGenvarDeclaration()
{
	return false;
}

bool Parser::ParseTypeDeclaration()
{
	return false;
}

DeclLifetime Parser::ParseLifetime(){
	if( ConsumeIfMatch ( tok::kw_static ) ) {
		return DeclLifetime::Static;
	} else if ( ConsumeIfMatch( tok::kw_automatic ) ) {
		return DeclLifetime::Automatic;
	}

	return DeclLifetime::Unknown;
}

// Section A.2.2 - Declaration data types
// Section A.2.2.1 - Net/Variable types
bool Parser::ParseCastingType()
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
TypeResult Parser::ParseDataType(NetType nType)
{
	DataType  dType = DataType::Unknown;

	switch( Tok.getKind() ) {
	case tok::kw_bit:
		dType = DataType::Bit;
		break;
	case tok::kw_logic:
		dType = DataType::Logic;
		break;
	case tok::kw_reg:
		dType = DataType::Reg;
		break;
	case tok::kw_byte:
		dType = DataType::Byte;
		break;
	case tok::kw_shortint:
		dType = DataType::ShortInt;
		break;
	case tok::kw_int:
		dType = DataType::Int;
		break;
	case tok::kw_longint:
		dType = DataType::LongInt;
		break;
	case tok::kw_integer:
		dType = DataType::Integer;
		break;
	case tok::kw_time:
		dType = DataType::Time;
		break;
	case tok::kw_event:
		dType = DataType::Event;
		break;
	case tok::kw_shortreal:
		dType = DataType::Shortreal;
		break;
	case tok::kw_real:
		dType = DataType::Real;
		break;
	case tok::kw_realtime:
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
	ConsumeToken();

	// Get the signing type
	SigningType sType = ParseSigning();
	
	// Get the dimension if it exists
	DimResult   dResult;
	if( Tok.is(tok::l_square) ){
		dResult = ParseDimension();
	}

	// TODO: Add error checking for multiple dimensions

    // Construct TypeInfo object
	TypeInfo *rType = nullptr;
	if( dResult.isUsable() ) {
		auto dim = dResult.get();
		//TypeInfo *rType = new TypeInfo(dType, nType, sType, dResult.get());
	} else {
		//TypeInfo *rType = new TypeInfo(dType, nType, sType);
	}

	//return TypeResult(rType);
   return TypeResult(false);
}

bool Parser::ParseEnumBaseType()
{
	return false;
}
bool Parser::ParseEnumNameDeclaration()
{
	return false;
}
bool Parser::ParseClassScope()
{
	return false;
}
bool Parser::ParseClassType()
{
	return false;
}

// net_type ::= supply0 | supply1 | tri | triand | trior | trireg| tri0 | tri1 | uwire| wire | wand | wor
NetType Parser::ParseNetType()
{
	NetType rType = NetType::Unknown;

	switch( Tok.getKind()) {
	case tok::kw_supply0:
		rType = NetType::Supply0;
		break;
	case tok::kw_supply1:
		rType = NetType::Supply1;
		break;
	case tok::kw_tri:
		rType = NetType::Tri;
		break;
	case tok::kw_triand:
		rType = NetType::TriAnd;
		break;
	case tok::kw_trior:
		rType = NetType::TriOr;
		break;
	case tok::kw_trireg:
		rType = NetType::TriReg;
		break;
	case tok::kw_tri0:
		rType = NetType::Tri0;
		break;
	case tok::kw_tri1:
		rType = NetType::Tri1;
		break;
	case tok::kw_uwire:
		rType = NetType::Uwire;
		break;
	case tok::kw_wire:
		rType = NetType::Wire;
		break;
	case tok::kw_wand:
		rType = NetType::Wand;
		break;
	case tok::kw_wor:
		rType = NetType::Wor;
		break;
	default:
		rType = NetType::Unknown;
		break;
	}

	if( rType != NetType::Unknown) {
		ConsumeToken();
	}

	return rType;
}

SigningType Parser::ParseSigning()
{
	if( ConsumeIfMatch( tok::kw_signed ) ) {
		return SigningType::Signed;
	} else if( ConsumeIfMatch( tok::kw_unsigned ) ) {
		return SigningType::Unsigned;
	}

	return SigningType::Unknown;
}
bool Parser::ParseStructUnionMember()
{
	return false;
}

bool Parser::ParseStructUnion()
{
	return false;
}
bool Parser::ParseTypeReference()
{
	return false;
}