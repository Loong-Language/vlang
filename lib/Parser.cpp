#include <stdio.h>
#include <assert.h>

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

void Parser::process_error()
{
	//previous_token();
	llvm::SMLoc loc = llvm::SMLoc::getFromPointer( token->tokenString.data() );
	llvm::SMLoc end = llvm::SMLoc::getFromPointer( token->tokenString.data() + token->tokenString.size() );
	const llvm::StringRef ref = tok::getTokenDebugName( token->kind );
	llvm::ArrayRef<llvm::SMRange> Ranges = llvm::ArrayRef<llvm::SMRange>( llvm::SMRange(loc, end) );

	srcMgr->PrintMessage( loc, llvm::SourceMgr::DK_Error, llvm::Twine( "Verilog parse error at token kind ", ref ), Ranges );
	//consume_token();
}

llvm::SMRange Parser::consume_token( bool skipWhiteSpace )
{
	assert( lexer != NULL && "Null lexer\n" );
	auto tokenRange = llvm::SMRange(
		llvm::SMLoc::getFromPointer( token->tokenString.data() ),
		llvm::SMLoc::getFromPointer( token->tokenString.data() + token->tokenString.size() ));
	const llvm::StringRef ref = tok::getTokenDebugName( token->kind );
	llvm::ArrayRef<llvm::SMRange> Ranges = llvm::ArrayRef<llvm::SMRange>(tokenRange);

	//srcMgr->PrintMessage( tokenRange.Start,llvm::SourceMgr::DK_Note,  llvm::Twine( "Processed token ", ref ));
	token = lexer->getNextToken(skipWhiteSpace);
	assert( token != NULL && "Null token received\n" );

	if( token->kind == TokenKind::comment ) {
		consume_token();
	}
	return tokenRange;
}

void Parser::previous_token( )
{
	assert( lexer != NULL && "Null lexer\n" );
	token = lexer->getPreviousToken();
	assert( token != NULL && "Null token received\n" );
	return;
}

bool Parser::check_for_next_token( TokenKind kind )
{
	assert( lexer != NULL && "Null lexer\n" );
	Token *t = lexer->getNextToken( false );
	assert( t != NULL && "Null token received\n" );
	return ( kind == t->kind );
}

Token* Parser::get_next_token()
{
	assert( lexer != NULL && "Null lexer\n" );
	Token *t = lexer->getNextToken( false );
	assert( t != NULL && "Null token received\n" );
	return t;
}

Token* Parser::get_previous_token()
{
	assert( lexer != NULL && "Null lexer\n" );
	Token *t = lexer->getPreviousToken(  );
	assert( t != NULL && "Null token received\n" );
	return t;
}

void Parser::initialize_token()
{
	assert( lexer != NULL && "Null lexer\n" );
	token = lexer->getCurToken();
	assert( token != NULL && "Null token received\n" );
	return;
}

bool Parser::check_for_token_and_consume( TokenKind kind )
{
	if( !check_for_token( kind ) ) {
		return false;
	}
	consume_token();
	return true;
}

bool Parser::check_for_token( TokenKind kind )
{
	if( token->kind != kind ) {
		return 0;
	}
	return 1;
}

bool Parser::check_for_token_after( TokenKind afterKind, TokenKind kind )
{
	assert( lexer != NULL && "Null lexer\n" );
	Token *t = lexer->searchForTokenAfter( afterKind );
	assert( t != NULL && "Null token received\n" );
	return ( t->kind == kind );
}

bool Parser::parse_identifier( llvm::StringRef *ref)
{
	if( !check_for_token( TokenKind::identifier ) ) {
		return 0;
	}
	*ref = token->tokenString;
	consume_token();
	return 1;
}

int Parser::ParseFile( const char *filename )
{
	const std::string stringFileName = filename;
	std::string includedFile;
	std::string IncludedFile;
	if( srcMgr == nullptr ) {
		srcMgr = new llvm::SourceMgr();
	}
	int NewBuf = srcMgr->AddIncludeFile( stringFileName, llvm::SMLoc(), includedFile );
	if( NewBuf == -1 ) {
		printf( "ERROR: Could not open %s\n", filename );
		exit( 0 );
	}
	lexer = new Lexer( srcMgr, NewBuf );
	lexer->tokenize();
	initialize_token();

	if( sema == nullptr) {
		sema = new Semantic();
	}

	globalNamespace = new Scope();

	parse_description();

	return 0;
}

//description ::=
// module_declaration
// | udp_declaration
// | interface_declaration
// | program_declaration
// | package_declaration
// | { attribute_instance } package_item
// | { attribute_instance } bind_directive
// | config_declaration
bool Parser::parse_description()
{
	while( token->kind != TokenKind::eof ) {
		switch( token->kind ) {
		case TokenKind::cd_define:
			consume_token();
			if( !check_for_token_and_consume( TokenKind::define_name ) ) {
				printf( "ERROR: Expected define name after `define\n" );
				process_error();
			}
			if( !check_for_token_and_consume( TokenKind::define_value ) ) { }
			break;
		case TokenKind::kw_program:
		case TokenKind::kw_module:
		case TokenKind::kw_macromodule:
		case TokenKind::kw_interface:
			if( !parse_design_element_declaration() ) {
				printf( "ERROR: Could not process module\n" );
				process_error();
			}
			break;
		case TokenKind::cd_timescale:
			consume_token();
			consume_token(); // constant
			consume_token(); // timescale
			consume_token(); // '/'
			consume_token(); // constant
			consume_token(); // timescale
			break;
		case TokenKind::cd_include:
			consume_token();
			consume_token(); // Included filename
			break;
		default:
			printf("ERROR: Invalid description item\n");
			process_error();
			consume_token();
			break;
		}
	}
	return true;
}

// *_nonansi_header         ::= { attribute_instance } * [ lifetime ] module_identifier
//                              { package_import_declaration } [ parameter_port_list ] list_of_ports ;
// *_ansi_header            ::= { attribute_instance } * [ lifetime ] identifier
//                              { package_import_declaration } [ parameter_port_list ] [ list_of_port_declarations ] ;
// module_declaration       ::= *_nonansi_header [ timeunits_declaration ] { *_item }
//                              end* [ : identifier ]
//                            | *_ansi_header [ timeunits_declaration ] { non_port_*_item }
//                              end* [ : identifier ]
//                            | extern *_nonansi_header
//                            | extern *_ansi_header
// module_declaration    +=   ::= { attribute_instance } module_keyword [ lifetime ] module_identifier ( .* ) ;
//                                [ timeunits_declaration ] { module_item } endmodule [ : module_identifier ]
// interface_declaration +=   ::= { attribute_instance } interface interface_identifier ( .* ) ;
//                                [ timeunits_declaration ] { interface_item }
//                                endinterface [ : interface_identifier ]
// program_declaration   +=   ::= { attribute_instance } program program_identifier ( .* ) ;
//                                [ timeunits_declaration ] { program_item }
//                                endprogram [ : program_identifier ]
// module_keyword             ::= module | macromodule
bool Parser::parse_design_element_declaration()
{
	llvm::StringRef module_name;
	llvm::StringRef module_end_name;
	bool isInterface = false;

	// TODO: Handle extern's

	// Find the design type being declared
	DesignType type;
	switch(token->kind) {
		case TokenKind::kw_module:
		case TokenKind::kw_macromodule:
			type = DesignType::Module;
			break;
		case TokenKind::kw_interface:
			type = DesignType::Interface;
			break;
		case TokenKind::kw_program:
			type = DesignType::Program;
			break;
		default:
            assert(0 && "Unknown design element type");
			return false;
			break;
	}
	consume_token();

	// Check for optional lifetime
	DeclLifetime lifetime = DeclLifetime::Unknown;
	if( check_for_token_and_consume ( TokenKind::kw_static ) ) {
		lifetime = DeclLifetime::Static;
	} else if ( check_for_token_and_consume( TokenKind::kw_automatic ) ) {
		lifetime = DeclLifetime::Automatic;
	}

	// Check for name of module
	if( !parse_identifier( &module_name ) ) {
		printf( "ERROR: Expected module identifier\n" );
		process_error();
	}

	// Call semantic analysis of design declaration start
//	if( !actions->ActOnDesignDeclaration(module_name, type, lifetime ) ) {
		// TODO: Support error handling if needed
//	}

	// TODO: support package_import_list
	//if( parse_package_import_list() ) {
	//}

	// Parse parameters
	if( check_for_token(TokenKind::pound) ) {
		if(!parse_parameter_port_list() ) {
			printf("ERROR: Expected parameter list in module declaration\n");
			process_error();
		}
	}
	
	// Check for port list, either ansi or nonansi versions
	if( check_for_token(TokenKind::l_paren) ) {
		// Determine if it is an ansi or nonansi header
		// TODO: This doesn't work for use defined interfaces
		if( check_for_next_token(TokenKind::identifier) ) {
			parse_list_of_ports();
		} else {
			parse_list_of_port_declarations();
		}
	}

	// Module header should always end with a semicolon
	if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
		printf( "ERROR: Expected semicolon after module header\n" );
		process_error();
	}

	// Process the body of the module
	while( parse_module_item() ) { }

	switch(token->kind){
	case TokenKind::kw_endmodule:
	case TokenKind::kw_endinterface:
	case TokenKind::kw_endprogram:
		consume_token();
		break;
	default:
		printf("ERROR: Expected end<kind> at the end of the design element declaration\n");
		process_error();
		break;
	}

	if( check_for_token_and_consume( TokenKind::colon ) ) {
		if( !parse_identifier( &module_end_name ) ) {
			printf( "ERROR: Expected end module identifier to match header module name\n" );
			process_error();
		}
	}
	return true;
}
bool Parser::parse_udp_declaration()
{
	return false;
}

bool Parser::parse_package_declaration()
{
	return false;
}
bool Parser::parse_bind_directive()
{
	return false;
}
bool Parser::parse_config_declaration()
{
	return false;
}

// parameter_port_list ::=
//   # ( list_of_param_assignments { , parameter_port_declaration } )
// | # ( parameter_port_declaration { , parameter_port_declaration } )
// | #( )
bool Parser::parse_parameter_port_list()
{
	if( !check_for_token_and_consume( TokenKind::pound ) ) {
		return false;
	}
	if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
		printf( "ERROR: Expected '(' for parameter port declarations\n" );
		process_error();
	}

	// TODO
	//   if( parse_list_of_param_assignments() ) {
	//      expect_comma = true;
	//   }
	do {
		if( !parse_parameter_port_declaration() ) {
			break;
		}
	} while( check_for_token_and_consume(TokenKind::comma) );

	if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
		printf( "ERROR: Expected ')' to close parameter port declarations\n" );
		process_error();
	}

	return true;
}

// parameter_port_declaration ::= parameter_declaration
//                              | local_parameter_declaration
//                              | data_type list_of_param_assignments
//                              | type list_of_type_assignments
bool Parser::parse_parameter_port_declaration()
{
	return false;
}

// list_of_ports   ::= ( port { , port } )
// port            ::= [ port_expression ]
//                   |  . port_identifier ( [ port_expression ] )
// port_expression ::= port_reference
//                   | { port_reference { , port_reference } }
// port_reference  ::= port_identifier constant_select
bool Parser::parse_list_of_ports()
{
	llvm::StringRef ident;
	if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
		assert( 0 && "Expected '(' for list of ports" );
	}
	do{
		if( parse_identifier( &ident) ) {
		} else if(check_for_token_and_consume(TokenKind::period) ) {
			if( !parse_identifier(&ident) ) {
				printf("ERROR: port identifier required after '.'\n");
				process_error();
			}
			if( !check_for_token_and_consume(TokenKind::l_paren) ) {
				printf("ERROR: Expected '(' after port identifier\n");
				process_error();
			}
			do{
				if(parse_identifier(&ident) ){
					auto result = parse_select_or_range();
				}
			} while( check_for_token_and_consume(TokenKind::comma));
				
			if( !check_for_token_and_consume(TokenKind::r_paren) ) {
				printf("ERROR: Expected ')' to close port expression\n");
				process_error();
			}
		}
	} while( check_for_token_and_consume(TokenKind::comma) );

	if( !check_for_token_and_consume(TokenKind::r_paren) ) {
		printf("ERROR: Expected ')' to close port list\n");
		process_error();
	}
	return true;
}

// list_of_port_declarations ::=  ( [ { attribute_instance} ansi_port_declaration { , { attribute_instance} ansi_port_declaration } ] )
// ansi_port_declaration     ::= [ port_direction ] [ net_port_type]                   port_identifier { unpacked_dimension } [ = constant_expression ]
//                             | interface_identifier [ . modport_identifier ]         port_identifier { unpacked_dimension } [ = constant_expression ]
//                             | interface [ . modport_identifier ]                    port_identifier { unpacked_dimension } [ = constant_expression ]
//                             | [ port_direction ] variable_port_type                 port_identifier { variable_dimension } [ = constant_expression ]
//                             | [ port_direction ] .                                  port_identifier ( [ expression ] )
// port_direction       ::= input | output | inout | ref
bool Parser::parse_list_of_port_declarations()
{
	DimensionInfo *dimension = nullptr;
	DimensionKind kind = DimensionKind::Unknown;
	ExprResult *lhs = nullptr;
	ExprResult *rhs = nullptr;

	llvm::StringRef ident;
	if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
		assert( 0 && "Expected '(' for parse_list_of_port_declarations()\n");
	}
	DataType dataType = DataType::Unknown;
	PortKind portKind = PortKind::Unknown;

	do {
		bool allowAssignment      = true;
		bool allowParenExpression = false;
		switch(token->kind) {
		case TokenKind::kw_input:
			portKind = PortKind::Input;
			consume_token();
			break;
		case TokenKind::kw_output:
			portKind = PortKind::Output;
			consume_token();
			break;
		case TokenKind::kw_inout:
			portKind = PortKind::Inout;
			consume_token();
			break;
		case TokenKind::kw_ref:
			portKind = PortKind::Ref;
			consume_token();
			break;
		case TokenKind::kw_interface:
			if( token->kind == TokenKind::period) {
			}
			break;

		case TokenKind::period:
			consume_token();
			allowAssignment       = false;
			allowParenExpression  = true;
			break;

		// End of empty port declaration
		case TokenKind::r_paren:
			continue;
			break;

	    // Empty port entry, go to while evaluation to consume comma
		case TokenKind::comma:
			continue;
			break;

        // Typedef variable
		case TokenKind::kw_var:
			assert(0 && "Typedef ports are not supported");
			continue;

		// Bare identifier
		case TokenKind::identifier:
			break;
		default:
			process_error();
			assert(0 && "Not Implemented\n");
			break;
		}

		// Check for implicit data type
		//dataType = parse_net_type();
		auto decl_info = parse_declaration_type_info();

        // This should never happen
		if( !decl_info.isUsable() ) {
			assert(0 && "Declaration Type Info is null\n");
		}

		// Check for port_identifier
		if( !parse_identifier(&ident) ) {
			printf("ERROR: Expected identifier in port declaration\n");
			process_error();
		}

		// Check if a dimension is specified
		if( check_for_token(TokenKind::l_square) ){
			parse_dimension();
		}

		// Check for assignment
		if( check_for_token_and_consume(TokenKind::equal) ) {
			if( !allowAssignment ) {
				printf("ERROR: Assignments are not allowed for this kind of port declaration\n");
				process_error();
			}

			auto result = parse_expression(Precedence::Assignment);
		}

		// Check for ( [ expression ] )
		if( check_for_token_and_consume(TokenKind::l_paren) ) {
			if( !allowParenExpression ) {
				printf("ERROR: '(' is not allowed for this kind of port declaration\n");
				process_error();
			}

			auto result = parse_expression(Precedence::Assignment);
			if(  result.isInvalid() ) {
				printf("ERROR: Expected expression\n");
				process_error();
			}
			if( !check_for_token_and_consume(TokenKind::r_paren) ) {
				printf("ERROR: Expected ')' after port expression\n");
				process_error();
			}
		}
	} while( check_for_token_and_consume( TokenKind::comma ) );

	if( !check_for_token_and_consume(TokenKind::r_paren ) ) {
		printf("ERROR: Expected '(' to close port list\n");
		process_error();
	}
	return true;
}

// Section A.1.4
bool Parser::parse_elaboration_system_task()
{
	return false;
}

// module_common_item ::=
//   module_or_generate_item_declaration
// | interface_instantiation -- TODO
// | program_instantiation -- TODO
// | assertion_item -- TODO
// | bind_directive -- TODO
// | continuous_assign
// | net_alias
// | initial_construct
// | final_construct
// | always_construct
// | loop_generate_construct -- TODO
// | conditional_generate_construct -- TODO
// | elaboration_system_task -- TODO
bool Parser::parse_module_common_item()
{
	switch(token->kind){
	case TokenKind::kw_assign:
		parse_continuous_assign();
		break;
	case TokenKind::kw_initial:
		parse_initial_construct();
		break;
	case TokenKind::kw_final:
		parse_final_construct();
		break;
	case TokenKind::kw_always:
	case TokenKind::kw_always_comb:
	case TokenKind::kw_always_ff:
	case TokenKind::kw_always_latch:
		parse_always_construct();
		break;
	case TokenKind::kw_alias:
		parse_net_alias();
		break;
	default:
		if( !parse_module_or_generate_item_declaration() ) {
			return false;
		}
		break;
	}

	return true;
}

// module_item ::=
//   port_declaration ;
// | non_port_module_item
// non_port_module_item ::=
//   generate_region
// | module_or_generate_item
// | specify_block -- TODO
// | { attribute_instance } specparam_declaration
// | program_declaration   -- TODO
// | module_declaration    -- TODO
// | interface_declaration -- TODO
// | timeunits_declaration -- TODO

bool Parser::parse_module_item()
{
	if( check_for_token(TokenKind::kw_generate) ) {
		parse_generate_region();
	}else if( parse_port_declaration(  ) ) {
		return true;
	} else if ( parse_module_or_generate_item() ) {
		return true;
	} else if ( parse_specparam_declaration() ) {
		return true;
	}
	return false;
}

// module_or_generate_item ::=
// { attribute_instance } parameter_override
// | { attribute_instance } gate_instantiation -- TODO
// | { attribute_instance } udp_instantiation -- TODO
// | { attribute_instance } module_instantiation
// | { attribute_instance } module_common_item
bool Parser::parse_module_or_generate_item()
{
	if( parse_parameter_override() ) {
		return true;
	} else if ( parse_module_instantiation() ) {
		return true;
	} else if ( parse_module_common_item() ) {
		return true;
	} else if ( parse_gate_instantiation() ) {
		return true;
	}
	return false;
}

// module_or_generate_item_declaration ::=
//   package_or_generate_item_declaration
// | genvar list_of_identifiers ;
// | clocking_declaration -- TODO
// | default clocking clocking_identifier ; -- TODO
bool Parser::parse_module_or_generate_item_declaration()
{
	if( parse_package_or_generate_item_declaration() ) {
		return true;
	} else if (check_for_token_and_consume(TokenKind::kw_genvar) ) {
//		if( !parse_list_of_identifiers(false, false, false, false)) {
//			printf("ERROR: Expected list of genvar identifiers\n");
//			process_error();
//		}
//		if( !check_for_token_and_consume(TokenKind::semicolon) ) {
//			printf("ERROR: Expected ';' after genvar identifier list\n");
//			process_error();
//		}
		return true;
	}
	return false;
}
bool Parser::parse_port_module_item()
{
	return false;
}
bool Parser::parse_parameter_override()
{
	return false;
}
bool Parser::parse_bind_target_scope()
{
	return false;
}
bool Parser::parse_bind_target_instance()
{
	return false;
}
bool Parser::parse_bind_target_instance_list()
{
	return false;
}
bool Parser::parse_bind_instantiation()
{
	return false;
}

// Section A.1.5 - Configuration source text
// TODO

// Section A.1.6 - Interface items
bool Parser::parse_interface_or_generate_item()
{
	return false;
}
bool Parser::parse_extern_tf_declaration()
{
	return false;
}
bool Parser::parse_interface_item()
{
	return false;
}
bool Parser::parse_non_port_interface_item()
{
	return false;
}

// Section A.1.7 - Program items
bool Parser::parse_program_item()
{
	return false;
}
bool Parser::parse_non_port_program_item()
{
	return false;
}
bool Parser::parse_program_generate_item()
{
	return false;
}

// Section A.1.8 - Checker items
// TODO

// Section A.1.9 - Class items
// TODO

// Section A.1.10 - Constraints
// TODO

// Section A.1.11 - Package items

bool Parser::parse_package_item()
{
	return false;
}

// package_or_generate_item_declaration ::=
//   net_declaration
// | data_declaration
// | task_declaration
// | function_declaration
// | checker_declaration -- TODO
// | dpi_import_export -- TODO
// | extern_constraint_declaration -- TODO
// | class_declaration -- TODO
// | class_constructor_declaration -- TODO
// | local_parameter_declaration ; -- TODO
// | parameter_declaration ; -- TODO
// | covergroup_declaration -- TODO
// | overload_declaration -- TODO
// | assertion_item_declaration -- TODO
// | ;

bool Parser::parse_package_or_generate_item_declaration()
{
	switch( token->kind ) {
	case TokenKind::kw_supply0:
	case TokenKind::kw_supply1:
	case TokenKind::kw_tri:
	case TokenKind::kw_triand:
	case TokenKind::kw_trior:
	case TokenKind::kw_trireg:
	case TokenKind::kw_tri0:
	case TokenKind::kw_tri1:
	case TokenKind::kw_uwire:
	case TokenKind::kw_wire:
	case TokenKind::kw_wand:
	case TokenKind::kw_wor:
	case TokenKind::kw_bit:
	case TokenKind::kw_logic:
	case TokenKind::kw_reg:
	case TokenKind::kw_byte:
	case TokenKind::kw_shortint:
	case TokenKind::kw_int:
	case TokenKind::kw_longint:
	case TokenKind::kw_integer:
	case TokenKind::kw_time:
	case TokenKind::kw_event:
	case TokenKind::kw_shortreal:
	case TokenKind::kw_real:
	case TokenKind::kw_realtime:
	case TokenKind::kw_localparam:
	case TokenKind::kw_parameter:
	case TokenKind::kw_specparam:
	case TokenKind::kw_const:
	case TokenKind::kw_var:
		// TODO: parse data declaration
		parse_data_declaration_list();
		break;
	case TokenKind::kw_task:
	case TokenKind::kw_function:
		parse_task_or_function_declaration();
		break;
	default:
		return false;
		break;
	}

	return true;
}

bool Parser::parse_anonymous_program()
{
	return false;
}

bool Parser::parse_anonymous_program_item()
{
	return false;
}

// Section A.2 - Declarations

bool Parser::parse_specparam_declaration()
{
	return false;
}

// Section A.2.1.2 - Port Declarations
bool Parser::parse_ref_declaration()
{
	return false;
}

// TODO: Support interface port declarations
// port_declaration           ::= { attribute_instance } inout_declaration
//                              | { attribute_instance } input_declaration
//                              | { attribute_instance } output_declaration
//                              | { attribute_instance } ref_declaration
//                              | { attribute_instance } interface_port_declaration
// inout_declaration          ::= inout net_port_type list_of_port_identifiers
// input_declaration          ::= input net_port_type list_of_port_identifiers
//                              | input variable_port_type list_of_variable_identifiers
// output_declaration         ::= output net_port_type list_of_port_identifiers
//                              | output variable_port_type list_of_variable_port_identifiers
// ref_declaration            ::= ref variable_port_type list_of_port_identifiers
// interface_port_declaration ::= interface_identifier list_of_interface_identifiers
//                              | interface_identifier . modport_identifier list_of_interface_identifiers
// net_port_type              ::= [ net_type ] data_type_or_implicit
bool Parser::parse_port_declaration(  )
{
	bool require_net_port_type = false;
	switch( token->kind ) {
	case TokenKind::kw_inout:
		require_net_port_type = true;
	case TokenKind::kw_input:
	case TokenKind::kw_output:
		consume_token();
		break;
	default:
		return false;
		break;
	}

	parse_net_type();

//	if( !parse_list_of_identifiers(false, false, true, false ) ) {
//		printf( "ERROR: Expected list of port names\n" );
//		process_error();
	//}

	if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
		printf( "ERROR: Expected semicolon\n" );
		process_error();
	}

	return true;
}

// Section A.2.2.2 - Strengnths
// TODO

// Section A.2.2.3 - Delays

// delay_value ::=
//   unsigned_number
// | real_number
// | ps_identifier
// | time_literal
// | 1step
bool Parser::parse_delay_value()
{
	switch( token->kind ) {
	case TokenKind::numeric_constant:
	case TokenKind::identifier:
		consume_token();
		break;
	default:
		return false;
		break;
	}
	return true;
}
// TODO

// Section A.2.3 - Declaration lists
bool Parser::parse_list_of_defparam_assignments()
{
	return false;
}

// list_of_genvar_identifiers        ::= genvar_identifier { , genvar_identifier }
// list_of_interface_identifiers     ::= interface_identifier { unpacked_dimension }
//                                       { , interface_identifier { unpacked_dimension } }
// list_of_port_identifiers          ::= port_identifier { unpacked_dimension }
//                                       { , port_identifier { unpacked_dimension } }
// list_of_udp_port_identifiers      ::= port_identifier { , port_identifier }
// list_of_tf_variable_identifiers   ::= port_identifier { variable_dimension } [ = expression ]
//                                       { , port_identifier { variable_dimension } [ = expression ] }
// list_of_variable_identifiers      ::= variable_identifier { variable_dimension }
//                                       { , variable_identifier { variable_dimension } }
// list_of_variable_port_identifiers ::= port_identifier { variable_dimension } [ = constant_expression ]
//                                     { , port_identifier { variable_dimension } [ = constant_expression ] }
// variable_identifier_list          ::= variable_identifier { , variable_identifier }
// list_of_net_decl_assignments      ::= net_identifier { unpacked_dimension } [ = expression ]
//                                     { , net_identifier { unpacked_dimension } [ = expression ] }
bool Parser::parse_list_of_identifiers( DeclTypeInfo *declType)
{
	llvm::StringRef ident;

	// Always start with an identifier
	if(!check_for_token(TokenKind::identifier) ) {
		return false;
	}

	do{
		// Fail if there isn't an identifier after a ','
		//  This first loop should always be an identifier because we exit early if it isn't
		if(!parse_identifier( &ident) ) {
			printf("ERROR: Expected identifier after ',' in identifier list\n");
			process_error();
		}

		// Always check for dimensions, this let the parser detect incorrect syntax and report it
		if( check_for_token(TokenKind::l_square) ) {
			parse_dimension();
			bool foundDimension = false;
		}

		// Check for assignment expression
		if( check_for_token_and_consume(TokenKind::equal) ) {
			auto result = parse_expression(Precedence::Assignment);

			if( result.isInvalid() ) {
				printf("ERROR: Expected expression after identifier list assignment\n");
				process_error();
			}
		}
	} while(check_for_token_and_consume(TokenKind::comma));

	return true;
}

// list_of_param_assignments ::= param_assignment { , param_assignment }
// param_assignment ::= parameter_identifier { unpacked_dimension } [ = constant_param_expression ]
bool Parser::parse_list_of_param_assignments()
{
	llvm::StringRef ident;
	bool found_ident = false;

	do {
		if( !parse_identifier( &ident ) ) {
			if( found_ident == true ) {
				printf( "ERROR: Expected identifier in param list\n" );
				process_error();
			}
			return false;
		}
		found_ident = true;

		if(check_for_token(TokenKind::l_square)) {
			parse_dimension();
		}

		if( check_for_token_and_consume( TokenKind::equal ) ) {
			if( parse_expression(Precedence::Assignment).isInvalid() ) {
				printf( "ERROR: Expected constant param expression\n" );
				process_error();
			}
		}
	} while( check_for_token_and_consume( TokenKind::comma ) );

	return true;
}

bool Parser::parse_list_of_specparam_assignments()
{
	return false;
}

bool Parser::parse_list_of_type_assignments()
{
	return false;
}

// list_of_variable_decl_assignments ::= variable_decl_assignment { , variable_decl_assignment }
bool Parser::parse_list_of_variable_decl_assignments()
{
	bool found_decl = false;
	while( parse_variable_decl_assignment() ) {
		found_decl = true;
		if( !check_for_token_and_consume( TokenKind::comma ) ) {
			break;
		}
	}
	return found_decl;
}

bool Parser::parse_list_of_virtual_interface_decl()
{
	return false;
}

// Section A.2.4 Declaration assignments
bool Parser::parse_defparam_assignment()
{
	return false;
}

bool Parser::parse_specparam_assignment()
{
	return false;
}
bool Parser::parse_type_assignment()
{
	return false;
}
bool Parser::parse_pulse_control_specparam()
{
	return false;
}
bool Parser::parse_error_limit_value()
{
	return false;
}
bool Parser::parse_reject_limit_value()
{
	return false;
}
bool Parser::parse_limit_value()
{
	return false;
}

// variable_decl_assignment ::=
//   variable_identifier { variable_dimension } [ = expression ]
// | dynamic_array_variable_identifier unsized_dimension { variable_dimension } [ = dynamic_array_new ]
// | class_variable_identifier [ = class_new ] -- TODO
bool Parser::parse_variable_decl_assignment()
{
	llvm::StringRef var_name;
	bool dyn_array = false;
	if( parse_identifier( &var_name ) ) {

		if( check_for_token(TokenKind::l_square)) {
			auto dimResult = parse_dimension();
			if (!dimResult.isInvalid() ) {
			}
		}

		if( check_for_token_and_consume( TokenKind::equal ) ) {
			if( dyn_array ) {
				if( !parse_dynamic_array_new() ) {
					printf( "ERROR: Expected dynamic array new statement\n" );
					process_error();
				}
				return true;
			}

			auto result = parse_expression(Precedence::Assignment);
			if ( result.isInvalid()){
				printf( "ERROR: Expected expression for variable initial value\n" );
				process_error();
			}
		}
		return true;
	}
	return false;
}
bool Parser::parse_class_new()
{
	return false;
}
bool Parser::parse_dynamic_array_new()
{
	return false;
}

// Section A.2.5 - Declaration ranges
// unpacked_dimension    ::= [ constant_range ]
//                         | [ constant_expression ]
// packed_dimension      ::= [ constant_range ]
//                         | [ ]
// variable_dimension    ::= unsized_dimension
//                         | unpacked_dimension
//                         | associative_dimension
//                         | queue_dimension
// unsized_dimension     ::= [ ]
// associative_dimension ::= [ data_type ] -- TODO
//                         | [ * ]
// queue_dimension       ::= [ $ [ : constant_expression ] ]
DimResult Parser::parse_dimension()
{
	bool allowErrorProcess = true;

	if( !check_for_token_and_consume(TokenKind::l_square) ) {
		assert(0 && "parse_dimension should never be called with the current token not '['\n");
		return DimResult(true);
	}

	DimensionInfo *info = nullptr;

	switch(token->kind){
	case TokenKind::r_square:
		info = new DimensionInfo(DimensionKind::Unsized, nullptr, nullptr);
		break;

    // TODO: Support type'd associative arrays
	case TokenKind::multiply:
		consume_token();
		info = new DimensionInfo(DimensionKind::Associative, nullptr, nullptr);
		break;

    // TODO: Support constant expression
	case TokenKind::dollar:
		if( check_for_token_and_consume(TokenKind::colon) ) {
			auto result = parse_expression(Precedence::Assignment);
			if( result.isInvalid() ) {
				printf("ERROR: Constant expression required in queue dimension after ':'\n");
				process_error();
			}

			info = new DimensionInfo(DimensionKind::Queue, result.get(), nullptr);
		}
		break;
	default:
		ExprResult lhsResult = parse_expression(Precedence::Assignment);
		if(  lhsResult.isInvalid() ) {
			printf("ERROR: Expected expression in dimension\n");
				process_error();
		}
		if( check_for_token_and_consume(TokenKind::colon) ) {
			auto rhsResult = parse_expression(Precedence::Assignment);
			if( rhsResult.isInvalid() ){
				printf("ERROR: Expected constant expression in range\n");
				process_error();
			}

			info = new DimensionInfo(DimensionKind::VariableRange, lhsResult.get(), rhsResult.get());
		} else {
			info = new DimensionInfo(DimensionKind::VariableRange, lhsResult.get(), nullptr);
		}
		break;
	}
		
	if( !check_for_token_and_consume(TokenKind::r_square) ) {
		printf("ERROR: ']' required to terminate dimension\n");
		process_error();
	}

	assert((info != nullptr) && "DimensionInfo shold never be nullptr\n");

    // TODO
	return DimResult(info);
}

// Section A.2.6 - Function declarations

bool Parser::parse_dpi_import_export()
{
	return false;
}

bool Parser::parse_dpi_spec_string()
{
	return false;
}

bool Parser::parse_dpi_function_import_property()
{
	return false;
}

bool Parser::parse_dpi_task_import_property()
{
	return false;
}

bool Parser::parse_dpi_function_proto()
{
	return false;
}

bool Parser::parse_dpi_task_proto()
{
	return false;
}

// Section A.2.7 - Task declarations
// task_declaration ::= task [ lifetime ] task_body_declaration
// function_declaration ::= function [ lifetime ] function_body_declaration

// task_body_declaration ::=
//   [ interface_identifier . | class_scope ] task_identifier ;
//     { tf_item_declaration }
//     { statement_or_null }
//   endtask [ : task_identifier ]
// | [ interface_identifier . | class_scope ] task_identifier ( [ tf_port_list ] ) ;
//     { block_item_declaration }
//     { statement_or_null }
//   endtask [ : task_identifier ]

// function_body_declaration ::=
//   function_data_type_or_implicit [ interface_identifier . | class_scope ] function_identifier ;
//     { tf_item_declaration }
//     { function_statement_or_null }
//   endfunction [ : function_identifier ]
// | function_data_type_or_implicit [ interface_identifier . | class_scope ] function_identifier ( [ tf_port_list ] ) ;
//     { block_item_declaration }
//     { function_statement_or_null }
//   endfunction [ : function_identifier ]

// function_data_type_or_implicit ::=
//   data_type_or_void
// | implicit_data_type
bool Parser::parse_task_or_function_declaration()
{
	bool is_task = (token->kind == TokenKind::kw_task);
	// Should never enter this if token isn't function or task
	assert((token->kind == TokenKind::kw_task || token->kind == TokenKind::kw_function) && "Token is not task or function");
	consume_token();

	if( check_for_token_and_consume( TokenKind::kw_static ) ) {
	} else if ( check_for_token_and_consume( TokenKind::kw_automatic ) ) {
	}

	// Look for function_data_type_or_implicit if function
	if( !is_task ) {
		if( parse_data_type_or_void()) {
//		} else if (parse_implicit_data_type() ) {
		}
	}

	llvm::StringRef ident;
	if( !parse_identifier( &ident ) ) {
		printf( "ERROR: Expected task identifier\n" );
		process_error();
	}
	if( check_for_token( TokenKind::l_paren) ) {
		//parse_tf_port_list();
		parse_list_of_port_declarations(); // TODO: Make sure list_of_port_declarations is compatabile with tf_port_list
	}
	if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
		printf( "ERROR: Expected ';' after task identifier\n" );
		process_error();
	}

	while( parse_tf_item_declaration() ) {}
	while( parse_statement_or_null() ) {}

	if( is_task ) {
		if( !check_for_token_and_consume( TokenKind::kw_endtask ) ) {
			printf( "ERROR: Expected endtask after task body\n" );
			process_error();
		}
	} else {
		if( !check_for_token_and_consume( TokenKind::kw_endfunction )) {
			printf("ERROR: Expected endfunction after function body\n");
			process_error();
		}
	}

	if( check_for_token_and_consume( TokenKind::colon ) ) {
		if ( !parse_identifier( &ident ) ) {
			printf( "ERROR: Expected identifier after endtask ':'\n" );
			process_error();
		}
	}

	return true;
}

// tf_item_declaration ::=
//   block_item_declaration
// | tf_port_declaration
bool Parser::parse_tf_item_declaration()
{
	if( parse_block_item_declaration() ) {
		return true;
	} else if ( parse_tf_port_declaration() ) {
		return true;
	}
	return false;
}

bool Parser::parse_tf_port_list()
{
	return false;
}
bool Parser::parse_tf_port_item()
{
	return false;
}

// tf_port_direction ::= port_direction | const ref
// tf_port_declaration ::= { attribute_instance } tf_port_direction [ var ] data_type_or_implicit list_of_tf_variable_identifiers ;
bool Parser::parse_tf_port_declaration()
{
	if( check_for_token_and_consume( TokenKind::kw_const ) ) {
		if( !check_for_token_and_consume( TokenKind::kw_ref ) ) {
			printf( "ERROR: Expected 'ref'\n" );
			process_error();
		}
	} else {
		switch( token->kind ) {
		case TokenKind::kw_input:
		case TokenKind::kw_output:
		case TokenKind::kw_inout:
			consume_token();
			break;
		default:
			return false;
			break;
		}
//		if( parse_data_type_or_implicit() ) { }
//		if( !parse_list_of_identifiers(true, false, false, true) ) {
//		}
	}

	if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
		printf( "ERROR: Expected semicolon after task/function port list\n" );
		process_error();
	}
	return true;
}
bool Parser::parse_task_prototype()
{
	return false;
}

// Section A.2.8 - Block item declarations

// block_item_declaration ::=
//   { attribute_instance } data_declaration
// | { attribute_instance } local_parameter_declaration ;
// | { attribute_instance } parameter_declaration ;
// | { attribute_instance } overload_declaration
// | { attribute_instance } let_declaration
bool Parser::parse_block_item_declaration()
{
	switch( token->kind ) {
	case TokenKind::kw_supply0:
	case TokenKind::kw_supply1:
	case TokenKind::kw_tri:
	case TokenKind::kw_triand:
	case TokenKind::kw_trior:
	case TokenKind::kw_trireg:
	case TokenKind::kw_tri0:
	case TokenKind::kw_tri1:
	case TokenKind::kw_uwire:
	case TokenKind::kw_wire:
	case TokenKind::kw_wand:
	case TokenKind::kw_wor:
	case TokenKind::kw_bit:
	case TokenKind::kw_logic:
	case TokenKind::kw_reg:
	case TokenKind::kw_byte:
	case TokenKind::kw_shortint:
	case TokenKind::kw_int:
	case TokenKind::kw_longint:
	case TokenKind::kw_integer:
	case TokenKind::kw_time:
	case TokenKind::kw_event:
	case TokenKind::kw_shortreal:
	case TokenKind::kw_real:
	case TokenKind::kw_realtime:
	case TokenKind::kw_localparam:
	case TokenKind::kw_parameter:
	case TokenKind::kw_specparam:
	case TokenKind::kw_const:
	case TokenKind::kw_var:
		// TODO: parse data declaration
		parse_data_declaration_list();
		return true;
		break;

    // identifier identifier - is a typedef/struct/class declaration
	case TokenKind::identifier:
		if(check_for_next_token(TokenKind::identifier)) {
			parse_data_declaration_list();
			return true;
		}
		return false;
		break;
	default:
		break;
	}

	return false;
}
bool Parser::parse_overload_declaration()
{
	return false;
}
bool Parser::parse_overload_operator()
{
	return false;
}
bool Parser::parse_overload_proto_formals()
{
	return false;
}

// Section A.2.9 - Interface declarations
// Section A.2.10 - Assertion declarations
// Section A.2.11 - Covergroup declarations
// TODO

// Section A.3 - Primitive instances
// gate_instantiation ::=
//   cmos_switchtype [delay3] cmos_switch_instance { , cmos_switch_instance } ;
// | enable_gatetype [drive_strength] [delay3] enable_gate_instance { , enable_gate_instance } ;
// | mos_switchtype [delay3] mos_switch_instance { , mos_switch_instance } ;
// | n_input_gatetype [drive_strength] [delay2] n_input_gate_instance { , n_input_gate_instance } ;
// | n_output_gatetype [drive_strength] [delay2] n_output_gate_instance { , n_output_gate_instance } ;
// | pass_en_switchtype [delay2] pass_enable_switch_instance { , pass_enable_switch_instance } ;
// | pass_switchtype pass_switch_instance { , pass_switch_instance } ;
// | pulldown [pulldown_strength] pull_gate_instance { , pull_gate_instance } ;
// | pullup [pullup_strength] pull_gate_instance { , pull_gate_instance } ;

// cmos_switch_instance ::= [ name_of_instance ] ( output_terminal , input_terminal , ncontrol_terminal , pcontrol_terminal )
// enable_gate_instance ::= [ name_of_instance ] ( output_terminal , input_terminal , enable_terminal )
// mos_switch_instance ::= [ name_of_instance ] ( output_terminal , input_terminal , enable_terminal )
// n_input_gate_instance ::= [ name_of_instance ] ( output_terminal , input_terminal { , input_terminal } )
// n_output_gate_instance ::= [ name_of_instance ] ( output_terminal { , output_terminal } , input_terminal )
// pass_switch_instance ::= [ name_of_instance ] ( inout_terminal , inout_terminal )
// pass_enable_switch_instance ::= [ name_of_instance ] ( inout_terminal , inout_terminal , enable_terminal )
// pull_gate_instance ::= [ name_of_instance ] ( output_terminal )

bool Parser::parse_gate_instantiation()
{
	llvm::StringRef ident;
	enum GATE_INSTANCE_TERMS {
		GATE_INSTANCE_TERM_OUTPUT,
		GATE_INSTANCE_TERM_MULTI_OUTPUT,
		GATE_INSTANCE_TERM_INPUT,
		GATE_INSTANCE_TERM_MULTI_INPUT,
		GATE_INSTANCE_TERM_INOUT,
		GATE_INSTANCE_TERM_NCONTROL,
		GATE_INSTANCE_TERM_PCONTROL,
		GATE_INSTANCE_TERM_ENABLE,
		GATE_INSTANCE_TERM_NONE
	};
	GATE_INSTANCE_TERMS term[5];
	term[0] = GATE_INSTANCE_TERM_NONE;
	term[1] = GATE_INSTANCE_TERM_NONE;
	term[2] = GATE_INSTANCE_TERM_NONE;
	term[3] = GATE_INSTANCE_TERM_NONE;
	term[4] = GATE_INSTANCE_TERM_NONE;

	switch( token->kind ) {
	case TokenKind::kw_cmos:
	case TokenKind::kw_rcmos:
		term[0] = GATE_INSTANCE_TERM_OUTPUT;
		term[1] = GATE_INSTANCE_TERM_INPUT;
		term[2] = GATE_INSTANCE_TERM_NCONTROL;
		term[3] = GATE_INSTANCE_TERM_PCONTROL;
		// if( parse_delay3()) {}
		break;
	case TokenKind::kw_bufif0:
	case TokenKind::kw_bufif1:
	case TokenKind::kw_notif0:
	case TokenKind::kw_notif1:
		term[0] = GATE_INSTANCE_TERM_OUTPUT;
		term[1] = GATE_INSTANCE_TERM_INPUT;
		term[2] = GATE_INSTANCE_TERM_ENABLE;
		// if( parse_drive_strength()) {}
		// if( parse_delay3()){}
		break;
	case TokenKind::kw_nmos:
	case TokenKind::kw_pmos:
	case TokenKind::kw_rnmos:
	case TokenKind::kw_rpmos:
		term[0] = GATE_INSTANCE_TERM_OUTPUT;
		term[1] = GATE_INSTANCE_TERM_INPUT;
		term[2] = GATE_INSTANCE_TERM_ENABLE;
		break;
	case TokenKind::kw_and:
	case TokenKind::kw_nand:
	case TokenKind::kw_or:
	case TokenKind::kw_nor:
	case TokenKind::kw_xor:
	case TokenKind::kw_xnor:
		term[0] = GATE_INSTANCE_TERM_OUTPUT;
		term[1] = GATE_INSTANCE_TERM_MULTI_INPUT;
		break;
	case TokenKind::kw_buf:
	case TokenKind::kw_not:
		term[0] = GATE_INSTANCE_TERM_MULTI_OUTPUT;
		term[1] = GATE_INSTANCE_TERM_INPUT;
		break;
	case TokenKind::kw_tranif0:
	case TokenKind::kw_tranif1:
	case TokenKind::kw_rtranif1:
	case TokenKind::kw_rtranif0:
		term[0] = GATE_INSTANCE_TERM_INOUT;
		term[1] = GATE_INSTANCE_TERM_INOUT;
		break;
	case TokenKind::kw_tran:
	case TokenKind::kw_rtran:
		term[0] = GATE_INSTANCE_TERM_INOUT;
		term[1] = GATE_INSTANCE_TERM_INOUT;
		term[2] = GATE_INSTANCE_TERM_ENABLE;
		break;
	case TokenKind::kw_pulldown:
	case TokenKind::kw_pullup:
		term[0] = GATE_INSTANCE_TERM_OUTPUT;
		break;
	default:
		return false;
		break;
	}
	consume_token();

	if( parse_delay_control() ) {
	}

	do {
		if( parse_identifier( &ident ) ) {}

		if( check_for_token(TokenKind::l_square)){
			parse_dimension();
		}

		if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
			printf( "ERROR: Expected '(' after gate instantiations\n" );
			process_error();
		}

		// Process the arguments
		int total_args = lexer->countTokensBetween( TokenKind::comma, TokenKind::r_paren, TokenKind::semicolon ) + 1;
		int seen_args  = 0;
		int term_index = 0;
		ExprResult result;
		do {
			switch( term[term_index] ) {
			case GATE_INSTANCE_TERM_INPUT:
			case GATE_INSTANCE_TERM_OUTPUT:
			case GATE_INSTANCE_TERM_ENABLE:
			case GATE_INSTANCE_TERM_NCONTROL:
			case GATE_INSTANCE_TERM_PCONTROL:
				term_index++;
				result = parse_expression(Precedence::Assignment);
				if( result.isInvalid() ) {
					printf( "ERROR: Expected expression for gate port\n" );
					process_error();
				}
				break;
			case GATE_INSTANCE_TERM_MULTI_OUTPUT: /* Always one before end */
				if( seen_args == total_args - 2 ) {
					term_index++;
				}

				result = parse_expression(Precedence::Assignment);
				if( result.isInvalid() ) {
					printf( "ERROR: Expected expression for gate port\n" );
					process_error();
				}
				break;
			case GATE_INSTANCE_TERM_MULTI_INPUT:  /* Always at end */
				result = parse_expression(Precedence::Assignment);

				if( result.isInvalid()) {
					printf( "ERROR: Expected expression for gate port\n" );
					process_error();
				}
				break;
			case GATE_INSTANCE_TERM_INOUT:
				term_index++;
				if( parse_lvalue(true).isInvalid() ) {
					printf( "ERROR: Expected value for inout\n" );
					process_error();
				}
			default:
				break;
			}

			seen_args++;

			if( seen_args != total_args ) {
				if( !check_for_token_and_consume( TokenKind::comma ) ) {
					printf( "ERROR: Expected ',' between expressions in gate instance\n" );
					process_error();
				}
			}
		} while( seen_args != total_args );

		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' after gate instantiations\n" );
			process_error();
		}
	} while ( check_for_token_and_consume( TokenKind::comma ) );

	if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
		printf( "ERROR: Expected ';' after gate instantiation\n" );
		process_error();
	}
	return true;
}

// Section A.4 - Instantiations
// Section A.4.1.1 - Module instantiation
// module_instantiation ::= module_identifier [ parameter_value_assignment ] hierarchical_instance { , hierarchical_instance } ;
bool Parser::parse_module_instantiation()
{
	bool require_ident = false;
	llvm::StringRef ident;
	if( !parse_identifier( &ident ) ) {
		return false;
	}

	do {
		if( parse_hierarchical_instance() ) {
			require_ident = true;
		} else if ( require_ident ) {
			printf( "ERROR: Expected hierarchical instance\n" );
			process_error();
		}
	} while ( check_for_token_and_consume( TokenKind::comma ) );

	if( require_ident && !check_for_token_and_consume( TokenKind::semicolon ) ) {
		printf( "ERROR: Expected ';' after hierarchical instance\n" );
		process_error();
	}
	return true;
}
bool Parser::parse_parameter_value_assignment()
{
	return false;
}
bool Parser::parse_list_of_parameter_assignments()
{
	return false;
}
bool Parser::parse_ordered_parameter_assignment()
{
	return false;
}
bool Parser::parse_named_parameter_assignment()
{
	return false;
}

//  hierarchical_instance ::= name_of_instance ( [ list_of_port_connections ] )
bool Parser::parse_hierarchical_instance()
{
	llvm::StringRef ident;
	if( parse_identifier( &ident ) ) {
		if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
			printf( "ERROR: Expected '(' after instance identifier\n" );
			process_error();
		}
		if( parse_list_of_port_connections() ) { }
		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' after list of ports\n" );
			process_error();
		}
		return true;
	}
	return false;
}
bool Parser::parse_name_of_instance()
{
	return false;
}

// list_of_port_connections ::=
//      ordered_port_connection { , ordered_port_connection }
//    | named_port_connection { , named_port_connection }
// ordered_port_connection ::= { attribute_instance } [ expression ]
// named_port_connection ::=
//      { attribute_instance } . port_identifier [ ( [ expression ] ) ]
//    | { attribute_instance } .*   // TODO
bool Parser::parse_list_of_port_connections()
{
	llvm::StringRef ident;
	bool namedPortConnection = false;
	bool seenPortConnection  = false;
	bool firstPortConnection = true;

	// Named port connection
	if( check_for_token(TokenKind::period) ) {
		namedPortConnection = true;
		seenPortConnection  = true;
	}

	do {
		bool flaggedError = false;

		if( !firstPortConnection ) {
			seenPortConnection = true;
		}

		if( check_for_token_and_consume( TokenKind::period ) ) {
			if( !namedPortConnection ) {
				flaggedError = true;
				printf("ERROR: Not allowed to mix ordered ports and named ports\n");
				process_error();
			}
		} else if ( namedPortConnection ) {
			flaggedError = true;
			printf("ERROR: Not allowed to mix ordered ports and named ports\n");
			process_error();
		}

		if( (namedPortConnection || flaggedError) && !parse_identifier( &ident ) ) {
			printf( "ERROR: Expected port identifier\n" );
			process_error();
		}

		if( namedPortConnection && !check_for_token_and_consume(TokenKind::l_paren) ) {
			printf("ERROR: Expected '(' after named port\n");
			process_error();
		}

		auto result = parse_expression(Precedence::Assignment);
		if( !result.isInvalid() ) {
			seenPortConnection = true;
		}
		if( namedPortConnection && !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' to close port expression\n" );
			process_error();
		}
	} while( check_for_token_and_consume( TokenKind::comma ) );

	return seenPortConnection;
}

// NOT USED
bool Parser::parse_ordered_port_connection()
{
	printf( "ERROR: Should not use parse_ordered_port_connection\n" );
	process_error();
	return false;
}

// NOT USED
bool Parser::parse_named_port_connection()
{
	printf( "ERROR: Should not use parse_named_port_connection\n" );
	process_error();
	return false;
}

// Section A.4.1.2 - Interface instantiation
// Section A.4.1.3 - Program instantiation
// Section A.4.1.4 - Checker instantiation
// Section A.4.2 - Generated instantiation
bool Parser::parse_generate_region()
{
	if( !check_for_token_and_consume(TokenKind::kw_generate) ) {
		return false;
	}

	while( parse_module_or_generate_item() ||
		   parse_interface_or_generate_item() ||
		   0 // TODO parse_checker_or_generate_item()
		   ) {}
	return true;
}
// Section A.5 - UDP's
// TODO

//////////////////////////////////////////
// Section A.6 - Behavioral statements
//////////////////////////////////////////
// Section A.6.1 - Continuous assignments

// continuous_assign ::=
//   assign [ drive_strength ] [ delay3 ] list_of_net_assignments ;
// | assign [ delay_control ] list_of_variable_assignments ;
bool Parser::parse_continuous_assign()
{
	if( !check_for_token_and_consume( TokenKind::kw_assign ) ) {
		return false;
	}

	// TODO: Parse drive_strenght or delay_control
	if( parse_delay_control() ) {}

	if( !parse_list_of_net_assignments() && !parse_list_of_variable_assignments() ) {
		printf( "ERROR: Expected list of assignments\n" );
		process_error();
	}

	if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
		printf( "ERROR: Expected ';' after assign\n" );
		process_error();
	}
	return true;;
}

// list_of_net_assignments ::= net_assignment { , net_assignment }
bool Parser::parse_list_of_net_assignments()
{
	if( !parse_net_assignment() ) {
		return false;
	}

	while( check_for_token_and_consume( TokenKind::comma ) ) {
		if( !parse_net_assignment() ) {
			printf( "ERROR: Expected net assignment after ','\n" );
			process_error();
		}
	}
	return true;
}

// list_of_variable_assignments ::= variable_assignment { , variable_assignment }
bool Parser::parse_list_of_variable_assignments()
{
	bool seen_first = false;
	do {
		if( !parse_variable_assignment() ) {
			if( seen_first ) {
				printf( "ERROR: Expected variable assignment after ','\n" );
				process_error();
			}
			return false;
		}
	} while( check_for_token_and_consume( TokenKind::comma ) );

	return true;
}

// net_alias ::= alias net_lvalue = net_lvalue { = net_lvalue } ;
bool Parser::parse_net_alias()
{
	return false;
}

// net_assignment ::= net_lvalue = expression
bool Parser::parse_net_assignment()
{
	if( parse_lvalue(true).isInvalid() ) {
		return false;
	}
	if( !check_for_token_and_consume( TokenKind::equal ) ) {
		printf( "ERROR: Expected '=' after assign variable name\n" );
		process_error();
	}

	auto result = parse_expression(Precedence::Assignment);
	if( result.isInvalid() ) {
		printf( "ERROR: Expected expression after '='\n" );
		process_error();
	}
	return true;
}

// Section A.6.2 - Procedural blocks and assignments
bool Parser::parse_initial_construct()
{
	if( check_for_token_and_consume( TokenKind::kw_initial ) ) {
		if( !parse_statement_or_null() ) {
			printf( "ERROR: Expected statement or null after initial keyword\n" );
			process_error();
		}
		return true;
	}
	return false;
}
bool Parser::parse_always_construct()
{
	if( !check_for_token_and_consume( TokenKind::kw_always      ) &&
		!check_for_token_and_consume( TokenKind::kw_always_comb ) &&
		!check_for_token_and_consume( TokenKind::kw_always_latch ) &&
		!check_for_token_and_consume( TokenKind::kw_always_ff   ) ) {
			return false;
	}
	if( !parse_statement() ) {
		printf( "ERROR: Expected statement after always keyword\n" );
		process_error();
	}
	return true;
}
bool Parser::parse_final_construct()
{
	return false;
}

// blocking_assignment ::=
//   variable_lvalue = [ delay_or_event_control ] expression
// | nonrange_variable_lvalue = dynamic_array_new
// | [ implicit_class_handle . | class_scope | package_scope ] hierarchical_variable_identifier select = class_new
// | operator_assignment
// nonblocking_assignment ::= variable_lvalue <= [ delay_or_event_control ] expression
bool Parser::parse_blocking_or_nonblocking_assignment( bool allow_blocking, bool allow_non_blocking )
{
	bool is_blocking = false;
//	if( parse_lvalue(false).isInvalid() ) {
//		return false;
//	}

	// Non blocking
	if( check_for_token_and_consume( TokenKind::less_equal ) ) {
		is_blocking = true;
		if( !allow_non_blocking ) {
			printf( "ERROR: Expected blocking statement\n" );
			process_error();
		}
	} else if ( check_for_token_and_consume( TokenKind::equal ) ) {
		if( !allow_blocking ) {
			printf( "ERROR: Expected non-blocking statement\n" );
			process_error();
		}
	} else {
		printf( "ERROR: Expected non-blocking or blocking statement\n" );
		process_error();
	}

	switch(token->kind ){
	case TokenKind::pound:
	case TokenKind::at:
	case TokenKind::kw_repeat:
		parse_delay_or_event_control();
		break;
	default:
		break;
	}

	auto result = parse_expression(Precedence::Assignment);
	if( result.isInvalid() ) {
		printf( "ERROR: Expected expression for blocking/nonblocking assignment\n" );
		process_error();
	}
	return true;
}

// operator_assignment ::= variable_lvalue assignment_operator expression
bool Parser::parse_operator_assignment()
{
	if( parse_lvalue(false).isInvalid() ) {
		return false;
	}

	if( !parse_assignment_operator() ) {
		printf( "ERROR: Expected assignment operator\n" );
		process_error();
	}

	auto result = parse_expression(Precedence::Assignment);
	if( result.isInvalid() ) {
		printf( "ERROR: Expected expression\n" );
		process_error();
	}
	return true;
}

// assignment_operator ::= = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= | <<<= | >>>=
bool Parser::parse_assignment_operator()
{
	switch( token->kind ) {
	case TokenKind::equal:
	case TokenKind::plus_equal:
	case TokenKind::minus_equal:
	case TokenKind::multiply_equal:
	case TokenKind::divide_equal:
	case TokenKind::modulo_equal:
	case TokenKind::and_equal:
	case TokenKind::or_equal:
	case TokenKind::xor_equal:
		consume_token();
		return true;
		break;
	default:
		break;
	}
	return false;
}

// variable_assignment ::= variable_lvalue = expression
bool Parser::parse_variable_assignment()
{
	if( parse_lvalue(false).isInvalid() ) {
		return false;
	}

	if( !check_for_token_and_consume( TokenKind::equal ) ) {
		printf( "ERROR: Expected '=' after lvalue of variable assignment\n" );
		process_error();
	}

	auto result = parse_expression(Precedence::Assignment);
	if( result.isInvalid() ) {
		printf( "ERROR: Expected expression for variable assignement\n" );
		process_error();
	}
	return true;
}

// Section A.6.3 - Parallel and sequential blocks
bool Parser::parse_action_block()
{
	return false;
}

// seq_block ::=
//   begin [ : block_identifier ] { block_item_declaration } { statement_or_null }
//     end [ : block_identifier ]
// par_block ::=
//   fork [ : block_identifier ] { block_item_declaration } { statement_or_null }
//     join_keyword [ : block_identifier ]
// join_keyword ::= join | join_any | join_none
bool Parser::parse_seq_or_par_block()
{
	bool is_seq_block = false;
	bool is_par_block = false;
	llvm::StringRef block_identifier = "";

	// Check for either block type
	if( check_for_token_and_consume( TokenKind::kw_begin ) ) {
		is_seq_block = true;
	} else if ( check_for_token_and_consume( TokenKind::kw_fork ) ) {
		is_par_block = true;
	} else {
		assert( 0 && "Not fork or begin" );
	}

	if( is_seq_block || is_par_block ) {
		if( check_for_token_and_consume( TokenKind::colon ) ) {
			if( !parse_identifier( &block_identifier ) ) {
				printf( "ERROR: Expected block identifier after colon\n" );
				process_error();
			}
		}

		while( parse_block_item_declaration() ) {}
		while( parse_statement_or_null() ) {}

		if( is_seq_block ) {
			if( !check_for_token_and_consume( TokenKind::kw_end ) ) {
				printf( "ERROR: Expected end keyword at end of sequential block\n" );
				process_error();
			}
		} else if( is_par_block ) {
			switch(token->kind){
			case TokenKind::kw_join:
			case TokenKind::kw_join_any:
			case TokenKind::kw_join_none:
				consume_token();
				break;
			default:
				printf( "ERROR: Expected join at end of parallel block\n" );
				process_error();
			}
		}

		if( check_for_token_and_consume( TokenKind::colon ) ) {
			if( !parse_identifier( &block_identifier ) ) {
				printf( "ERROR: Expected block identifier after colon\n" );
				process_error();
			}
		}
	}
	return is_seq_block || is_par_block;
}
// Section A.6.4 - Statements
// statement_or_null ::=
//   statement
// | { attribute_instance } ;
bool Parser::parse_statement_or_null()
{
	// Null statement
	if( check_for_token_and_consume( TokenKind::semicolon ) ) {
		return true;
	} else if ( parse_statement() ) {
		return true;
	}
	return false;
}

// statement ::= [ block_identifier : ] { attribute_instance } statement_item
bool Parser::parse_statement()
{
	llvm::StringRef block_identifier;
	bool require_statement_item = false;
	if( check_for_token( TokenKind::identifier ) && check_for_next_token( TokenKind::colon ) ) {
		// TODO: Handle block identifier
		if( !parse_identifier( &block_identifier ) ) {
			printf( "ERROR: Expected identifier\n" );
			process_error();
		}

		// Consume colon
		consume_token();
		require_statement_item = true;
	}

	if( parse_statement_item() ) {
		return true;
	} else if ( require_statement_item ) {
		printf( "ERROR: Expected statement\n" );
		process_error();
	}
	return false;
}

// statement_item ::=
//   blocking_assignment ;
// | nonblocking_assignment ;
// | procedural_continuous_assignment ;
// | case_statement
// | conditional_statement
// | inc_or_dec_expression ; -- TODO
// | subroutine_call_statement -- TODO
// | disable_statement
// | event_trigger
// | loop_statement
// | jump_statement -- TODO
// | par_block
// | procedural_timing_control_statement
// | seq_block
// | wait_statement
// | procedural_assertion_statement -- TODO
// | clocking_drive ; -- TODO
// | randsequence_statement -- TODO
// | randcase_statement -- TODO
// | expect_property_statement -- TODO

// loop_statement ::=
//   forever statement_or_null
// | repeat ( expression ) statement_or_null
// | while ( expression ) statement_or_null
// | for ( for_initialization ; expression ; for_step )
//     statement_or_null
// | do statement_or_null while ( expression ) ;
// | foreach ( ps_or_hierarchical_array_identifier [ loop_variables ] ) statement

// disable_statement ::=
//   disable hierarchical_task_identifier ;
// | disable hierarchical_block_identifier ;
// | disable fork ;

// procedural_timing_control_statement procedural_timing_control statement_or_null

// subroutine_call_statement ::= subroutine_call ;
//                             | void ' ( function_subroutine_call ) ;
bool Parser::parse_statement_item()
{
	Token *nToken;

	ExprResult result;
	switch( token->kind ) {
	case TokenKind::kw_assign: // procedural continuous assign
	case TokenKind::kw_force:
		consume_token();
		if( !parse_variable_assignment() ) {
			printf("ERROR: Expected variable assignment after 'assign'\n");
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
			printf( "ERROR: Expected ';' after assign statement\n" );
			process_error();
		}
		break;
	case TokenKind::kw_release:
	case TokenKind::kw_deassign:
		consume_token();
		if( parse_lvalue(false).isInvalid() ) {
			printf("ERROR: Expected lvalue after release/deassign\n");
			process_error();
		}
		if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
			printf( "ERROR: Expected ';' after release/deassign lvalue\n" );
			process_error();
		}
		break;
	case TokenKind::kw_disable: // Disable statement
		consume_token();
		switch(token->kind){
		case TokenKind::kw_fork:
			consume_token();
			break;
		case TokenKind::identifier:
			// TODO: Only check for block/task identifier
			if( !parse_hierarchical_identifier() ) {
				printf("ERROR: Expected block/task identifier\n");
				process_error();
			}
			break;
		default:
			printf("ERROR: Expected fork/identifier after disable keyword\n");
			process_error();
			break;
		}
		if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
			printf( "ERROR: Expected ';' after disable statement\n" );
			process_error();
		}
		break;
	case TokenKind::kw_begin:  // seq_block
	case TokenKind::kw_fork:   // par_block
		parse_seq_or_par_block();
		break;
	case TokenKind::kw_unique: //  case/if statement
	case TokenKind::kw_unique0:
	case TokenKind::kw_priority:
		nToken = get_next_token();
		switch( nToken->kind ) {
		case TokenKind::kw_if:
			parse_conditional_statement();
			break;
		case TokenKind::kw_case:
		case TokenKind::kw_casez:
		case TokenKind::kw_casex:
			parse_case_statement();
			break;
		default:
			printf( "ERROR: Expected case or if statement after priority\n" );
			process_error();
			break;
		}
		break;
	case TokenKind::kw_if: // conditional statement
		parse_conditional_statement();
		break;
	case TokenKind::kw_case: // case statement
	case TokenKind::kw_casez:
	case TokenKind::kw_casex:
		parse_case_statement();
		break;

	case TokenKind::kw_wait: // wait statement
	case TokenKind::kw_wait_order:
		if( !parse_wait_statement() ) {
			printf( "ERROR: Expected wait statement\n" );
			process_error();
		}
		break;
	case TokenKind::kw_forever: // loop statement
		consume_token();
		parse_statement_or_null();
		break;

	case TokenKind::kw_repeat: // loop statement
	case TokenKind::kw_while:  // loop statement
		consume_token();
		if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
			printf( "ERROR: Expected '(' after repeat keyword\n" );
			process_error();
		}

		result = parse_expression(Precedence::Assignment);
		if( result.isInvalid()) {
			printf( "ERROR: Expected expression inside repeat count\n" );
			process_error();
		}
		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' after repeat count\n" );
			process_error();
		}
		parse_statement_or_null();
		break;

	case TokenKind::kw_for: // loop statement
		consume_token();
		if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
			printf( "ERROR: Expected '(' after for keyword\n" );
			process_error();
		}

		if( !parse_list_of_variable_assignments() ) {
			printf( "ERROR: Expected list of variable assignments in for initialization\n" );
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
			printf( "ERROR: Expected ';' after variable initialzation of for loop\n" );
			process_error();
		}

		result = parse_expression(Precedence::Assignment);
		if( result.isInvalid() ) {
			printf( "ERROR: Expected end condition in for loop\n" );
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
			printf( "ERROR: Expected ';' to close end condition of for loop\n" );
			process_error();
		}

		if( !parse_for_step() ) {
			printf( "ERROR: Expected for step expressions\n" );
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' to close for statement\n" );
			process_error();
		}

		parse_statement_or_null();
		break;

	case TokenKind::kw_do:
	case TokenKind::kw_foreach:
		assert( 0 && "do/foreach not implemented" );
		break;

	case TokenKind::pound: // procedrual_timing_control_statement
		if( !parse_delay_control() ) {
			printf( "ERROR: Expected delay control\n" );
			process_error();
		}
		parse_statement_or_null();
		break;

	case TokenKind::at: // procedural_timing_control_statement
		if( !parse_event_control() ) {
			printf( "ERROR: Expected event control\n" );
			process_error();
		}
		parse_statement_or_null();
		break;
		// TODO - cycle_delay

	case TokenKind::l_brace:
		parse_concatenation();
		switch(token->kind) {
		case TokenKind::less_equal:
		case TokenKind::equal:
			if( !parse_blocking_or_nonblocking_assignment( true, true ) ) {
				printf( "ERROR: Expected assignment\n" );
				process_error();
			}

			if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
				printf( "ERROR: Expected ';' after assignment\n" );
				process_error();
			}
			break;
		default:
			assert(0 && "Not Implented\n");
			break;
		}
		break;
	case TokenKind::identifier:
		parse_hierarchical_identifier();
		switch(token->kind) {
			case TokenKind::l_paren: /* Sub or Functions */
				parse_tf_call();
				
				if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
					printf( "ERROR: Expected ';' after task/function call\n" );
					process_error();
				}
				break;
			case TokenKind::semicolon: /* Sub or Function */
				consume_token();
				break;
			case TokenKind::less_equal:
			case TokenKind::equal:
				if( !parse_blocking_or_nonblocking_assignment( true, true ) ) {
					printf( "ERROR: Expected assignment\n" );
					process_error();
				}

				if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
					printf( "ERROR: Expected ';' after assignment\n" );
					process_error();
				}
				break;
            // inc_or_dec_operator
			case TokenKind::minus_minus:
			case TokenKind::plus_plus:
				consume_token();
				if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
					printf( "ERROR: Expected ';' after ++|-- operator\n" );
					process_error();
				}
				break;
			default:
				assert(0 && "Not Implented\n");
				break;
		}
		break;

    // void ' ( function_subroutine_call ) ;
	case TokenKind::kw_void:
		consume_token();
		if( !check_for_token_and_consume( TokenKind::single_quote ) ) {
			printf( "ERROR: Expected ' after void keyword\n" );
			process_error();
		}
		if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
			printf( "ERROR: Expected '(' to start void function cast\n" );
			process_error();
		}

		if( parse_subroutine_call().isInvalid() ) {
			printf("ERROR: Expected subroutine call after void cast");
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' after void function call\n" );
			process_error();
		}
		
		if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
			printf( "ERROR: Expected ';' after function call\n" );
			process_error();
		}
		break;
	case TokenKind::implication:
	case TokenKind::implication_nonblock:
		if( !parse_event_trigger() ) {
			// This should never happen
			printf("ERROR: Internal error.  Expected even trigger\n");
		}
		break;
	default:
		if( token->kind != TokenKind::systask ) {
			return false;
		}
		parse_system_tf_call();
		if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
			printf( "ERROR: Expected ';' after system task call\n" );
			process_error();
		}
		break;
	}

	return true;
}

bool Parser::parse_function_statement()
{
	return false;
}
bool Parser::parse_function_statement_or_null()
{
	return false;
}

// Section A.6.5 - Timing controls
// delay_or_event_control ::=
//   delay_control
// | event_control
// | repeat ( expression ) event_control
bool Parser::parse_delay_or_event_control()
{
	if( parse_delay_control() ) {
		return true;
	} else if (parse_event_control() ) {
		return true;
	} else if (check_for_token_and_consume(TokenKind::kw_repeat) ) {
		if( !check_for_token_and_consume(TokenKind::l_paren)) {
			printf("ERROR: Expected '(' after repeat keyword\n");
			process_error();
		}
		parse_expression(Precedence::Assignment);
		if( !check_for_token_and_consume(TokenKind::r_paren)) {
			printf("ERROR: Expected ')' after repeat expression\n");
			process_error();
		}
		if( !parse_event_control() ) {
			printf("ERROR: Expected event control\n");
			process_error();
		}
	}

	// TODO: Should always find one of the above
	return true;
}

// delay_control ::=
//   # delay_value
// | # ( mintypmax_expression )
bool Parser::parse_delay_control()
{
	if( !check_for_token_and_consume( TokenKind::pound ) ) {
		return false;
	}

	if( check_for_token_and_consume( TokenKind::l_paren ) ) {
		auto result = parse_mintypmax_expression();
		if( result.isInvalid() ) {
			printf( "ERROR: Expected mintypmax expression\n" );
			process_error();
		}
		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' to close mintypmax expression\n" );
			process_error();
		}
	} else {
		if( !parse_delay_value() ) {
			printf( "ERROR: Expected delay value\n" );
			process_error();
		}
	}
	return true;
}

// event_control ::=
//    @ hierarchical_event_identifier -- TODO
//  | @ ( event_expression )
//  | @*
//  | @ (*)
//  | @ ps_or_hierarchical_sequence_identifier -- TODO
bool Parser::parse_event_control()
{
	if( !check_for_token_and_consume( TokenKind::at ) ) {
		return false;
	}

	// either event_expression or '*'
	if( check_for_token_and_consume( TokenKind::l_paren ) ) {
		if( !check_for_token_and_consume( TokenKind::multiply ) &&
			!parse_event_expression() ) {
				printf( "ERROR: Expected event expression inside '()'\n" );
				process_error();
		}

		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' to close even expression\n" );
			process_error();
		}
	} else {
		if ( !check_for_token_and_consume( TokenKind::multiply ) &&
			!parse_hierarchical_identifier()) {
				printf( "ERROR: Expected some event expression\n" );
				process_error();
		}
	}

	return true;
}

// event_expression ::=
//   [ edge_identifier ] expression [ iff expression ]
// | sequence_instance [ iff expression ]
// | event_expression or event_expression
// | event_expression , event_expression
// | ( event_expression )
bool Parser::parse_event_expression()
{
	if( check_for_token_and_consume( TokenKind::kw_posedge ) ||
		check_for_token_and_consume( TokenKind::kw_negedge ) ||
		check_for_token_and_consume( TokenKind::kw_edge ) ) {
			auto result = parse_expression(Precedence::Assignment);
			if( result.isInvalid()) {
				printf( "ERROR: Expected expression for event\n" );
				process_error();
			}
			if( token->kind == TokenKind::kw_or ||
				token->kind == TokenKind::comma ) {
					consume_token();
					return parse_event_expression();
			}
			return true;
	}

	auto result = parse_expression(Precedence::Assignment);
	if( result.isInvalid()) {
		return false;
	}
	if( check_for_token_and_consume( TokenKind::kw_or ) ||
		check_for_token_and_consume( TokenKind::comma ) ) {
			if( !parse_event_expression() ) {
				printf( "ERROR: Expected even expression\n" );
				process_error();
			}
	}
	return true;
}

// procedural_timing_control ::=
//    delay_control
//  | event_control
//  | cycle_delay -- TODO
bool Parser::parse_procedural_timing_control()
{
	if( parse_delay_control() ) {
		return true;
	}

	if( parse_event_control() ) {
		return true;
	}

	return false;
}
bool Parser::parse_jump_statement()
{
	return false;
}

// wait_statement ::=
//   wait ( expression ) statement_or_null
// | wait fork ;
// | wait_order ( hierarchical_identifier { , hierarchical_identifier } ) action_block
bool Parser::parse_wait_statement()
{
	if( check_for_token_and_consume( TokenKind::kw_wait ) ) {
		if( check_for_token_and_consume( TokenKind::kw_fork ) ) {
			if( !check_for_token_and_consume( TokenKind::semicolon ) ) {
				printf( "ERROR: Expected ';' after wait fork\n" );
				process_error();
			}
		} else if ( !check_for_token_and_consume( TokenKind::l_paren ) ) {
			printf( "ERROR: Expected '(' after wait statement\n" );
			process_error();
		}

		auto result = parse_expression(Precedence::Assignment);
		if( result.isInvalid() ) {
			printf( "ERROR: Expected expression in wait statement\n" );
			process_error();
		}

		if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
			printf( "ERROR: Expected ')' to close wait expression\n" );
			process_error();
		}

		parse_statement_or_null();
	} else if ( check_for_token_and_consume( TokenKind::kw_wait_order ) ) {
	} else {
		assert( 0 && "Expected wait statement" );
	}
	return true;
}

// event_trigger ::=
//    -> hierarchical_event_identifier ;
// | ->> [ delay_or_event_control ] hierarchical_event_identifier ;
bool Parser::parse_event_trigger()
{
	// This is an internal error
	if( token->kind != TokenKind::implication && token->kind != TokenKind::implication_nonblock ) {
		printf("ERROR: Internal error.  Expected event trigger\n");
		process_error();
	}

	consume_token();

	// TODO: Handle delay_or_event_control if nonblocking
	if( !parse_hierarchical_identifier() ) {
		printf("ERROR: Expected even identifier\n");
		process_error();
	}

	if( !check_for_token_and_consume(TokenKind::semicolon)) {
		printf("ERROR: Expected ';' after even identifier\n");
		process_error();
	}
	return true;
}

// Section A.6.6 - Conditional Statements

// conditional_statement ::=
//   [ unique_priority ] if ( cond_predicate ) statement_or_null
//   { else if ( cond_predicate ) statement_or_null }
//   [ else statement_or_null ]
// unique_priority ::= unique | unique0 | priority

bool Parser::parse_conditional_statement()
{
	bool found_final_else = false;

	if( parse_unique_priority() ) { }

	if( !check_for_token_and_consume( TokenKind::kw_if ) ) {
		assert( 0 && "Expected if keyword" );
	}

	if( !parse_cond_predicate( true ) ) {
		printf( "ERROR: Expected conditional predicate after if keyword\n" );
		process_error();
	}

	if( !parse_statement_or_null() ) {
		printf( "ERROR: Expected statement or null in 'if' body\n" );
		process_error();
	}

	while( !found_final_else && check_for_token_and_consume( TokenKind::kw_else ) ) {
		if( check_for_token_and_consume( TokenKind::kw_if ) ) {
			if( !parse_cond_predicate( true ) ) {
				printf( "ERROR: Expected conditional predicate after if keyword\n" );
				process_error();
			}
		} else {
			found_final_else = true;
		}
		if( !parse_statement_or_null() ) {
			printf( "ERROR: Expected statement or null in 'else' body\n" );
			process_error();
		}
	}

	return true;
}
bool Parser::parse_unique_priority()
{
	return false;
}

// cond_predicate ::=
//   expression_or_cond_pattern { &&& expression_or_cond_pattern } -- TODO: &&&
bool Parser::parse_cond_predicate( bool require_paren )
{
	bool found_expression = false;
	if( !check_for_token_and_consume( TokenKind::l_paren ) && require_paren ) {
		printf( "ERROR: Expected '(' inside condition predicate\n" );
		process_error();
	}

	// Body
	if( parse_expression_or_cond_pattern() ) {
		found_expression = true;
	}

	if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
		printf( "ERROR: Expected ')' after if condition predicate\n" );
		process_error();
	}
	return found_expression;
}

// expression_or_cond_pattern ::= expression | cond_pattern
// cond_pattern ::= expression matches pattern -- TODO
bool Parser::parse_expression_or_cond_pattern()
{
	auto result = parse_expression(Precedence::Assignment);
	if( !result.isInvalid() ) {
		return true;
	}
	return false;
}

// Section A.6.7 - Case statement

//  case_statement ::=
//  [ unique_priority ] case_keyword ( case_expression )
//    case_item { case_item } endcase
//  | [ unique_priority ] case_keyword (case_expression )matches
//    case_pattern_item { case_pattern_item } endcase
//  | [ unique_priority ] case ( case_expression ) inside
//    case_inside_item { case_inside_item } endcase
bool Parser::parse_case_statement()
{
	switch(token->kind){
	case TokenKind::kw_case:
	case TokenKind::kw_casez:
	case TokenKind::kw_casex:
		consume_token();
		break;
	default:
		assert(0 && "Expected case statement");
		break;
	}

	if( !check_for_token_and_consume( TokenKind::l_paren ) ) {
		printf( "ERROR: Expected '(' after case keyword\n" );
		process_error();
	}

	auto result = parse_expression(Precedence::Assignment);
	if( result.isInvalid()) {
		printf( "ERROR: Expected case expression for case statement\n" );
		process_error();
	}

	if( !check_for_token_and_consume( TokenKind::r_paren ) ) {
		printf( "ERROR: Expected ')' after case expression\n" );
		process_error();
	}

	while( parse_case_item() ) { }

	if( !check_for_token_and_consume( TokenKind::kw_endcase ) ) {
		printf( "ERROR: Expected endcase keyword after case statements\n" );
		process_error();
	}
	return true;
}

//  case_item ::=
//    case_item_expression { , case_item_expression } : statement_or_null
//  | default [ : ] statement_or_null
bool Parser::parse_case_item()
{
	if( check_for_token_and_consume( TokenKind::kw_default ) ) {
		check_for_token_and_consume( TokenKind::colon );
	} else {
		auto result = parse_expression(Precedence::Assignment);
		if ( result.isInvalid() ) {
			while( check_for_token_and_consume( TokenKind::comma ) ) {
				auto result = parse_expression(Precedence::Assignment);
				if( result.isInvalid() ) {
					printf( "ERROR: Expected case expression\n" );
					process_error();
				}
			}
			if( !check_for_token_and_consume( TokenKind::colon ) ) {
				printf( "ERROR: ';' required after case item expression\n" );
				process_error();
			}
		} else {
			return false;
		}
	}
	if( !parse_statement_or_null() ) { }

	return true;
}
bool Parser::parse_case_pattern_item()
{
	return false;
}
bool Parser::parse_case_inside_item()
{
	return false;
}
bool Parser::parse_randcase_statement()
{
	return false;
}
bool Parser::parse_randcase_item()
{
	return false;
}

// Section A.6.7.1 - Patterns
// TODO

// Section A.6.8 - Looping
bool Parser::parse_for_variable_declaration()
{
	return false;
}

// for_step ::= for_step_assignment { , for_step_assignment }
// for_step_assignment ::=
//   operator_assignment
// | inc_or_dec_expression
// | function_subroutine_call

// inc_or_dec_expression ::=
//   inc_or_dec_operator { attribute_instance } variable_lvalue
// | variable_lvalue { attribute_instance } inc_or_dec_operator
bool Parser::parse_for_step()
{
	bool allow_next   = false;
	do {
		if( parse_operator_assignment() ) {
			allow_next = check_for_token_and_consume( TokenKind::comma );
		} else if ( parse_inc_or_dec_expression().isInvalid() ) {
			allow_next = check_for_token_and_consume( TokenKind::comma );
			//      } else if ( parse_subroutine_call() ) {
			//         allow_next = check_for_token_and_consume( TokenKind::comma );
		} else if ( allow_next ) {
			printf( "ERROR: Expected step assignment\n" );
			process_error();
		} else {
			return false;
		}
	} while ( allow_next );

	return true;
}
bool Parser::parse_loop_variables()
{
	return false;
}

// Section A.6.10 - Assertion statements
// Section A.6.11 - Clocking block
// Section A.6.12 - Randsequence
// TODO

//////////////////////////////////////
// Section A.7 - Specify's
//////////////////////////////////////
// TODO

/////////////////////////////////
// Section A.9 - General
/////////////////////////////////
// Section A.9.1 - Attributes
// Section A.9.2 - Comments
// TODO

// Section A.9.3 - Identifiers
// hierarchical_identifier ::= [ $root . ] { identifier constant_bit_select . } identifier
// select ::= [ { . member_identifier bit_select } . member_identifier ] bit_select [ [ part_select_range ] ]
bool Parser::parse_hierarchical_identifier()
{
	llvm::StringRef ident;
	bool found_ident = false;
	bool require_ident = false;
	bool found_period = false;

	// TODO: check for $root
#if 0
	if( check_for_token_and_consume( TokenKind::sys_root ) ) {
		if( !check_for_token_and_consume( TokenKind::period ) ) {
			printf( "ERROR: Expected '.' after $root\n" );
			process_error();
		}
	}
#endif

	do{
		if( !parse_identifier( &ident ) ) {
			break;
		}
		found_ident = true;
		found_period = false;

		// Check to see if we should look for a constant_bit_select or it is
		//   the end of the identifier, in which case ignore
		// TODO: This isn't right, it should check based on the identifier type
		//       not based on the '.' or not
		if( check_for_token( TokenKind::l_square ) ) {
			if( parse_select_or_range().isInvalid() ) {
				printf( "ERROR: Expected constant bit select\n" );
				process_error();
			}
		}
	} while( check_for_token_and_consume(TokenKind::period));

	if( !found_ident && require_ident ) {
		printf( "ERROR: Expected identifier after $root.\n" );
		process_error();
	}
	return found_ident;
}
bool Parser::parse_package_scope()
{
	return false;
}