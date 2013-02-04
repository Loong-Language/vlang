/*
<one line to give the program's name and a brief idea of what it does.>
Copyright (C) 2011  <copyright holder> <email>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

#ifndef VERILOG_PARSER_H
#define VERILOG_PARSER_H

#include <vector>

#include <llvm/Support/SMLoc.h>

#include "SemanticTypes.h"
#include "ParserResult.h"
#include "Tokens.h"

namespace llvm
{
	class SourceMgr;
	class StringRef;
}

namespace vlang {
	class Lexer;
	class Scope;
	class Semantic;

	enum class DataType    : char;
	enum class NetType     : char;
	enum class SigningType : char;
	enum class DeclLifetime: char;

	/// PrecedenceLevels - Low precedences numbers bind
	/// more weakly than high numbers.
	enum class Precedence : int {
		Unknown         = 0,    // Not binary operator.
		Concatenation   = 1,    // {} {{}}
		Assignment      = 2,    // = += -= *= /= %= &= ^= |= <<= >>= <<<= >>>= := :/ <=
		Implication     = 3,    // -> <->
		Conditional     = 4,    // ?: (conditional operator)
		LogicalOr       = 5,    // ||
		LogicalAnd      = 6,    // &&
		BitOr           = 7,    // | (binary)
		BitXor          = 8,    // ^ ~^ ^~ (binary)
		BitAnd          = 9,    // & (binary)
		Equality        = 10,   // == != === !== ==? !=?
		Relational      = 11,   // < <= > >= inside dist
		Shift           = 12,   // << >> <<< >>>
		Additive        = 13,   // + - (binary)
		Multiplicative  = 14,   // * / %
		Power           = 15,   // **
		Unary           = 16,   // + - ! ~ & ~& | ~| ^ ~^ ^~ ++ -- (unary)
	};


	class Parser
	{
	public:
		Parser() : srcMgr( nullptr ), lexer( nullptr ), globalNamespace (nullptr), sema(nullptr) {}
		~Parser();
		int ParseFile( const char *filename );

	private:
		llvm::SourceMgr *srcMgr;
		Lexer  *lexer;
		Token *token;
		Semantic *sema;
		const char *fileLoc;

		Scope *globalNamespace;

		void process_error();
		void initialize_token();
		llvm::SMRange   consume_token(bool skipWhiteSpace = true );
		void previous_token();

		bool check_for_next_token( TokenKind kind );
		bool check_for_token_and_consume( TokenKind kind );
		bool check_for_token( TokenKind kind );
		bool check_for_token_after( TokenKind afterToken, TokenKind kind);
		Token* get_next_token();
		Token* get_previous_token();

		ExprResult combine_expr_results(ExprResult result0, ExprResult result1);
		bool parse_description(); // Check for timescale identifier at beginning
		bool parse_design_element_declaration();
		bool parse_udp_declaration();
		bool parse_package_declaration();
		bool parse_bind_directive();
		bool parse_config_declaration();

		bool parse_parameter_port_list();
		bool parse_parameter_port_declaration();
		bool parse_list_of_ports();
		bool parse_list_of_port_declarations();
		bool parse_port_declaration();

		// Section A.1.4
		bool parse_elaboration_system_task();
		bool parse_module_common_item();
		bool parse_module_item();
		bool parse_module_or_generate_item();
		bool parse_module_or_generate_item_declaration();
		bool parse_port_module_item();
		bool parse_parameter_override();
		bool parse_bind_target_scope();
		bool parse_bind_target_instance();
		bool parse_bind_target_instance_list();
		bool parse_bind_instantiation();

		// Section A.1.5 - Configuration source text
		// TODO

		// Section A.1.6 - Interface items
		bool parse_interface_or_generate_item();
		bool parse_extern_tf_declaration();
		bool parse_interface_item();
		bool parse_non_port_interface_item();

		// Section A.1.7 - Program items
		bool parse_program_item();
		bool parse_non_port_program_item();
		bool parse_program_generate_item();

		// Section A.1.8 - Checker items
		// TODO

		// Section A.1.9 - Class items
		// TODO

		// Section A.1.10 - Constraints
		// TODO

		// Section A.1.11 - Package items
		bool parse_package_item();
		bool parse_package_or_generate_item_declaration();
		bool parse_anonymous_program();
		bool parse_anonymous_program_item();

		// Section A.2 - Declarations
		// Section A.2.1.1 - module parameter declarations
		bool parse_specparam_declaration();

		// Section A.2.1.2 - Port Declarations
		bool parse_ref_declaration();

		// Section A.2.1.3 - Type declarations
		bool parse_package_import_declaration();
		bool parse_import_item();
		bool parse_package_export_declaration();
		bool parse_genvar_declaration();
		bool parse_type_declaration();

		bool parse_data_declaration_list();
		DeclLifetime parse_lifetime();

		// Section A.2.2 - Declaration data types
		DeclTypeResult parse_declaration_type_info();

		// Section A.2.2.1 - Net/Variable types
		bool parse_casting_type();
		TypeResult parse_data_type(NetType nType);
		TypeResult parse_data_type_or_implicit(NetType nType);
		bool parse_enum_base_type();
		bool parse_enum_name_declaration();
		bool parse_class_scope();
		bool parse_class_type();
		bool parse_integer_type();
		bool parse_integer_atom_type();
		bool parse_integer_vector_type();
		bool parse_non_integer_type();
		NetType     parse_net_type();
		SigningType parse_signing();
		bool parse_simple_type();
		bool parse_struct_union_member();
		bool parse_data_type_or_void();
		bool parse_struct_union();
		bool parse_type_reference();

		// Section A.2.2.2 - Strength
		// TODO

		// Section A.2.2.3 - Delays
		bool parse_delay_value();
		// TODO

		// Section A.2.3 - Declaration lists
		bool parse_list_of_identifiers(DeclTypeInfo *declType);
		bool parse_list_of_defparam_assignments();
		bool parse_list_of_genvar_assignments();
		bool parse_list_of_net_decl_assignments();
		bool parse_list_of_param_assignments();
		bool parse_list_of_specparam_assignments();
		bool parse_list_of_type_assignments();
		bool parse_list_of_variable_decl_assignments();
		bool parse_list_of_virtual_interface_decl();

		// Section A.2.4 Declaration assignments
		bool parse_defparam_assignment();
		bool parse_net_decl_assignment();
		bool parse_param_assignment();
		bool parse_specparam_assignment();
		bool parse_type_assignment();
		bool parse_pulse_control_specparam();
		bool parse_error_limit_value();
		bool parse_reject_limit_value();
		bool parse_limit_value();
		bool parse_variable_decl_assignment();
		bool parse_class_new();
		bool parse_dynamic_array_new();

		// Section A.2.5 - Declaration ranges
		DimResult parse_dimension();

		// Section A.2.6 - Function declarations
		bool parse_function_prototype();
		bool parse_dpi_import_export();
		bool parse_dpi_spec_string();
		bool parse_dpi_function_import_property();
		bool parse_dpi_task_import_property();
		bool parse_dpi_function_proto();
		bool parse_dpi_task_proto();

		// Section A.2.7 - Task declarations
		bool parse_task_or_function_declaration();
		bool parse_tf_item_declaration();
		bool parse_tf_port_list();
		bool parse_tf_port_item();
		bool parse_tf_port_direction();
		bool parse_tf_port_declaration();
		bool parse_task_prototype();

		// Section A.2.8 - Block item declarations
		bool parse_block_item_declaration();
		bool parse_overload_declaration();
		bool parse_overload_operator();
		bool parse_overload_proto_formals();

		// Section A.2.9 - Interface declarations
		// Section A.2.10 - Assertion declarations
		// Section A.2.11 - Covergroup declarations
		// TODO

		// Section A.3 - Primitive instances
		// Section A.3.1 - Primitive instantiaion and instances
		bool parse_gate_instantiation();
		bool parse_cmos_switch_instance();
		bool parse_enable_gate_instance();
		bool parse_mos_switch_instance();
		bool parse_n_input_gate_instance();
		bool parse_n_output_gate_instance();
		bool parse_pass_switch_instance();
		bool parse_pass_enable_switch_instance();
		bool parse_pull_gate_instance();

		// Section A.3.2 0 Primitive strengths
		bool parse_pulldown_strength();
		bool parse_pullup_strength();

		// Section A.3.3 Primitive terminals
		bool parse_enable_terminal();
		bool parse_inout_terminal();
		bool parse_input_terminal();
		bool parse_ncontrol_terminal();
		bool parse_output_terminal();
		bool parse_pcontrol_terminal();

		// Section A.3.4
		bool parse_cmos_switchtype();
		bool parse_enable_gatetype();
		bool parse_mos_switchtype();
		bool parse_n_input_gatetype();
		bool parse_n_output_gatetype();
		bool parse_en_switchtype();
		bool parse_pass_switchtype();

		// TODO

		// Section A.4 - Instantiations
		// Section A.4.1.1 - Module instantiation
		bool parse_module_instantiation();
		bool parse_parameter_value_assignment();
		bool parse_list_of_parameter_assignments();
		bool parse_ordered_parameter_assignment();
		bool parse_named_parameter_assignment();
		bool parse_hierarchical_instance();
		bool parse_name_of_instance();
		bool parse_list_of_port_connections();
		bool parse_ordered_port_connection();
		bool parse_named_port_connection();

		// Section A.4.1.2 - Interface instantiation
		// Section A.4.1.3 - Program instantiation
		// Section A.4.1.4 - Checker instantiation
		// Section A.4.2 - Generated instantiation
		bool parse_generate_region();

		// Section A.5 - UDP's
		// TODO

		//////////////////////////////////////////
		// Section A.6 - Behavioral statements
		//////////////////////////////////////////
		// Section A.6.1 - Continuous assignments
		bool parse_continuous_assign();
		bool parse_list_of_variable_assignments();
		bool parse_net_alias();
		bool parse_list_of_net_assignments();
		bool parse_net_assignment();

		// Section A.6.2 - Procedural blocks and assignments
		bool parse_initial_construct();
		bool parse_always_construct();
		bool parse_final_construct();
		bool parse_blocking_or_nonblocking_assignment( bool allow_blocking, bool allow_non_blocking );
		bool parse_operator_assignment();
		bool parse_assignment_operator();
		bool parse_variable_assignment();

		// Section A.6.3 - Parallel and sequential blocks
		bool parse_action_block();
		bool parse_seq_or_par_block();

		// Section A.6.4 - Statements
		bool parse_statement_or_null();
		bool parse_statement();
		bool parse_statement_item();
		bool parse_function_statement();
		bool parse_function_statement_or_null();

		// Section A.6.5 - Timing controls
		bool parse_procedural_timing_control_statement();
		bool parse_delay_or_event_control();
		bool parse_delay_control();
		bool parse_event_control();
		bool parse_event_expression();
		bool parse_procedural_timing_control();
		bool parse_jump_statement();
		bool parse_wait_statement();
		bool parse_event_trigger();

		// Section A.6.6 - Conditional Statements
		bool parse_conditional_statement();
		bool parse_unique_priority();
		bool parse_cond_predicate(bool require_paren);
		bool parse_expression_or_cond_pattern();

		// Section A.6.7 - Case statement
		bool parse_case_statement();
		bool parse_case_item();
		bool parse_case_pattern_item();
		bool parse_case_inside_item();
		bool parse_randcase_statement();
		bool parse_randcase_item();

		// Section A.6.7.1 - Patterns
		// TODO

		// Section A.6.8 - Loopiing
		bool parse_loop_statement();
		bool parse_for_initizalization();
		bool parse_for_variable_declaration();
		bool parse_for_step();
		bool parse_for_step_assignment();
		bool parse_loop_variables();

		// Section A.6.10 - Assertion statements
		// Section A.6.11 - Clocking block
		// Section A.6.12 - Randsequence
		// TODO

		//////////////////////////////////////
		// Section A.7 - Specify's
		//////////////////////////////////////
		// TODO

		//////////////////////////////////////
		// Section A.8 Expressions
		//////////////////////////////////////
		// Section A.8.1 - Concatenations
		ExprResult parse_concatenation();
		ExprResult parse_module_path_concatenation();
		ExprResult parse_module_path_multiple_concatenation();
		ExprResult parse_streaming_concatenation();
		ExprResult parse_stream_operator();
		ExprResult parse_slice_size();
		ExprResult parse_stream_concatenation();
		ExprResult parse_stream_expression();
		ExprResult parse_array_range_expression();
		ExprResult parse_empty_queue();
		bool parse_list_of_expressions(ExprVector &Exprs, std::vector<llvm::SMLoc> &CommaLocs);

		// Section A.8.2 - Subroutine calls
		ExprResult parse_tf_call();
		ExprResult parse_system_tf_call();
		ExprResult parse_subroutine_call();
		ExprResult parse_function_subroutine_call();
		ExprResult parse_list_of_arguments();
		ExprResult parse_method_call();
		ExprResult parse_method_call_body();
		ExprResult parse_built_in_method_call();
		ExprResult parse_array_manipulation_call();
		ExprResult parse_randomize_call();
		ExprResult parse_method_call_root();
		ExprResult parse_array_method_call();

		// Section A.8.3 - Expressions
		ExprResult parse_inc_or_dec_expression();
		ExprResult parse_conditional_expression();
		ExprResult parse_expression(Precedence minPrec);
		ExprResult parse_rhs_expression(ExprResult LHS, Precedence minPrec);
		ExprResult parse_tagged_union_expression();
		ExprResult parse_inside_expression();
		ExprResult parse_value_range();
		ExprResult parse_mintypmax_expression();
		ExprResult parse_part_select_range();
		ExprResult parse_indexed_range();
		ExprResult parse_genvar_expression();

		// Section A.8.4 - Primarys
		ExprResult  parse_primary_with_unary();
		ExprResult  parse_class_qualifier();
		ExprResult  parse_primary_literal();
		ExprResult  parse_time_literal();
		ExprResult  parse_time_unit();
		ExprResult  parse_implicit_class_handle();
		ExprResult  parse_select_or_range();
		ExprResult  parse_constant_cast();
		ExprResult  parse_constant_let_expression();
		ExprResult  parse_cast();

		// Section A.8.5 - Expression left-side values
		ExprResult  parse_lvalue(bool net_lvalue);

		// Section A.8.6 - Operators
		bool is_unary_operator();
		bool is_binary_operator();
		ExprResult  parse_inc_or_dec_operator();
		ExprResult  parse_unary_module_path_operator();
		ExprResult  parse_binary_module_path_operator();

		// Section A.8.7 - Numbers
		ExprResult  parse_number();

		// Section A.8.8 - Strings
		// TODO

		/////////////////////////////////
		// Section A.9 - General
		/////////////////////////////////
		// Section A.9.1 - Attributes
		// Section A.9.2 - Comments
		// TODO

		// Section A.9.3 - Identifiers
		bool parse_identifier( llvm::StringRef *ref);
		bool parse_hierarchical_identifier();
		bool parse_package_scope();
	};
};
#endif
