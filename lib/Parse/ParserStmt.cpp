//===--- Parser.cpp - C Language Family Parser ----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the Parser interfaces.
//
//===----------------------------------------------------------------------===//

#include "vlang/Parse/Parser.h"
#include "vlang/Sema/Sema.h"
#include "RAIIObjectsForParser.h"
#include "vlang/Parse/ParseDiagnostic.h"
#include "llvm/Support/raw_ostream.h"
using namespace vlang;

#define UNIMPLEMENTED_PARSE_STMT(n) bool Parser::n(){ return false; }

//////////////////////////////////////////
// Section A.6 - Behavioral statements
//////////////////////////////////////////
// Section A.6.1 - Continuous assignments

// continuous_assign ::=
//   assign [ drive_strength ] [ delay3 ] list_of_net_assignments ;
// | assign [ delay_control ]             list_of_variable_assignments ;
bool Parser::ParseContinuousAssign()
{
   assert( Tok.is(tok::kw_assign));
   ConsumeToken();

   // TODO: Parse drive_strenght or delay_control
   if( ParseDelayControl() ) {}

   ParseListOfAssignments();

   ExpectAndConsumeSemi(diag::err_expected_semi_decl_list);
   return true;
}

// list_of_net_assignments      ::= net_assignment { , net_assignment }
// list_of_variable_assignments ::= variable_assignment { , variable_assignment }
// net_assignment               ::= net_lvalue      = expression
// variable_assignment          ::= variable_lvalue = expression
bool Parser::ParseListOfAssignments(){
   llvm::StringRef ident;
   do{
      // TODO: Check for netlvalue

      // An identifier is required after a comma
      if( Tok.isNot(tok::identifier)){
         Diag(Tok, diag::err_expected_ident_after_comma);
         SkipUntil(tok::comma);
         continue;
      }
      ParseIdentifier(&ident);

      // Skip to next entry if there is no assignment
      if( !Tok.is(tok::equal))
         continue;

      // Consume the assignment
      ConsumeToken();

      auto result = ParseExpression(prec::Assignment);
      // TODO: Handle expression failing

   } while( ConsumeIfMatch(tok::comma));

   return true;
}


// net_alias ::= alias net_lvalue = net_lvalue { = net_lvalue } ;
bool Parser::ParseNetAlias()
{
   return false;
}

// Section A.6.2 - Procedural blocks and assignments
bool Parser::ParseInitialConstruct()
{
   assert(Tok.is(tok::kw_initial));
   ConsumeToken();

   if( !ParseStatement() ) {
      Diag(Tok, diag::err_expected_statement);
   }
   return true;
}
bool Parser::ParseAlwaysConstruct()
{
   assert(Tok.is(tok::kw_always)    || Tok.is(tok::kw_always_comb) ||
      Tok.is(tok::kw_always_ff) || Tok.is(tok::kw_always_latch));
   ConsumeToken();

   if( !ParseStatement() ) {
      Diag(Tok, diag::err_expected_statement);
   }
   return true;
}
bool Parser::ParseFinalConstruct()
{
   assert(Tok.is(tok::kw_final));
   return false;
}

// blocking_assignment ::=
//   variable_lvalue = [ delay_or_event_control ] expression
// | nonrange_variable_lvalue = dynamic_array_new
// | [ implicit_class_handle . | class_scope | package_scope ] hierarchical_variable_identifier select = class_new
// | operator_assignment
// nonblocking_assignment ::= variable_lvalue <= [ delay_or_event_control ] expression
bool Parser::ParseBlockingOrNonblockingAssignment( bool allow_blocking, bool allow_non_blocking )
{
   assert(Tok.is(tok::lessequal) || Tok.is(tok::equal));

   bool is_blocking = false;
   ParseLvalue(false);
   //	if( ParseLvalue(false).isInvalid() ) {
   //		return false;
   //	}

   // Non blocking
   if( Tok.is(tok::lessequal) ) {
      is_blocking = true;
      if( !allow_non_blocking ) {
         Diag(Tok, diag::err_expected_blocking_stmt);
      }
   } else {
      if( !allow_blocking ) {
         Diag(Tok, diag::err_expected_nonblocking_stmt);
      }
   }
   ConsumeToken();

   switch(Tok.getKind() ){
   case tok::hash:
   case tok::at:
   case tok::kw_repeat:
      ParseDelayOrEventControl();
      break;
   default:
      break;
   }

   auto result = ParseExpression(prec::Assignment);

   if( result.isInvalid() ) {
      Diag(Tok, diag::err_expected_expression);
   }
   return true;
}

// operator_assignment ::= variable_lvalue assignment_operator expression
bool Parser::ParseOperatorAssignment()
{
   if( ParseLvalue(false).isInvalid() ) {
      return false;
   }

   if( !ParseAssignmentOperator() ) {
      Diag(Tok, diag::err_expected_assign_operator);
   }

   auto result = ParseExpression(prec::Assignment);
   if( result.isInvalid() ) {
      Diag(Tok, diag::err_expected_expression);
   }
   return true;
}

// assignment_operator ::= = | += | -= | *= | /= | %= | &= | |= | ^= | <<= | >>= | <<<= | >>>=
bool Parser::ParseAssignmentOperator()
{
   switch( Tok.getKind() ) {
   case tok::equal:
   case tok::plusequal:
   case tok::minusequal:
   case tok::starequal:
   case tok::slashequal:
   case tok::percentequal:
   case tok::ampequal:
   case tok::pipeequal:
   case tok::caretequal:
      ConsumeToken();
      return true;
      break;
   default:
      break;
   }
   return false;
}

// Section A.6.3 - Parallel and sequential blocks
UNIMPLEMENTED_PARSE_STMT(ParseActionBlock)

   // seq_block ::=
   //   begin [ : block_identifier ] { block_item_declaration } { statement_or_null }
   //     end [ : block_identifier ]
   // par_block ::=
   //   fork [ : block_identifier ] { block_item_declaration } { statement_or_null }
   //     join_keyword [ : block_identifier ]
   // join_keyword ::= join | join_any | join_none
   bool Parser::ParseSeqOrParBlock() {
      assert(Tok.is(tok::kw_begin) || Tok.is(tok::kw_fork));
      tok::TokenKind TokArray[] = {tok::kw_end, tok::kw_join, tok::kw_join_any, tok::kw_join_none};

      bool is_seq_block = false;
      llvm::StringRef block_identifier = "";

      // Check for either block type
      is_seq_block = Tok.is(tok::kw_begin);
      ConsumeToken();

      // Named block
      if( Tok.is(tok::colon) ) {
         ConsumeToken();
         if( Tok.isNot(tok::identifier)){
            Diag(Tok, diag::err_expected_ident);
            SkipUntil(TokArray, false, true);
            goto CloseBlock;
         } else {
            ParseIdentifier(&block_identifier);
         }
      }

      while( ParseBlockItemDeclaration() ) {}

      do{
         // On error, 
         auto stmt = ParseStatementOrNull();

         // On error skip until end/join
         if( !stmt ){
            SkipUntil(TokArray, false, true);
         }
      } while(Tok.isNot(tok::kw_end)      && Tok.isNot(tok::kw_join) &&
         Tok.isNot(tok::kw_join_any) && Tok.isNot(tok::kw_join_none));

CloseBlock:
      switch(Tok.getKind()){
      case tok::kw_end:
         if( !is_seq_block )  Diag(Tok, diag::err_expected_join);
         ConsumeToken();
         break;
      case tok::kw_join:
      case tok::kw_join_any:
      case tok::kw_join_none:
         if( is_seq_block )   Diag(Tok, diag::err_expected_end);
         ConsumeToken();
         break;
      default:
         // TODO: Handle error case
         Diag(Tok, is_seq_block ? diag::err_expected_end : diag::err_expected_join);
         return false;
      }

      if( Tok.is(tok::colon) ){
         ConsumeToken();
         if( Tok.isNot(tok::identifier) ){
            Diag(Tok, diag::err_expected_matching_ident);
            return true;
         }

         llvm::StringRef ident;
         ParseIdentifier(&ident);
         // TODO: Check that name matches top label
      }
      return true;
}
// Section A.6.4 - Statements
// statement_or_null ::=
//   statement
// | { attribute_instance } ;
bool Parser::ParseStatementOrNull()
{
   // Null statement
   if( Tok.is(tok::semi) ) {
      ConsumeToken();
      return true;
   } else if ( ParseStatement() ) {
      return true;
   }
   return false;
}

// statement ::= [ block_identifier : ] { attribute_instance } statement_item
bool Parser::ParseStatement()
{
   if( Tok.is(tok::eof) ) {
      return false;
   }

   llvm::StringRef block_identifier;
   bool require_statement_item = false;
   auto nextToken = NextToken();

   if( Tok.is( tok::identifier ) && nextToken.is(tok::colon) ) {
      // TODO: Handle block identifier
      ParseIdentifier(&block_identifier);

      // Consume colon
      assert(Tok.is(tok::colon));
      ConsumeToken();
      require_statement_item = true;
   }

   if( ParseStatementItem() ) {
      return true;
   }

   if ( require_statement_item ) {
      Diag(Tok, diag::err_expected_statement);
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
bool Parser::ParseStatementItem()
{

   ExprResult result;
   switch( Tok.getKind() ) {
   case tok::kw_assign: // procedural continuous assign
   case tok::kw_force:
      ConsumeToken();

      switch(Tok.getKind() ){
      case tok::identifier:
         ParseHierarchicalIdentifier();
         if( Tok.isNot(tok::equal) && Tok.isNot(tok::equalgreater) ){
           Diag(Tok, diag::err_expected_assign_operator);
           SkipUntil(tok::semi,true,true);
           break;
         }
         ParseBlockingOrNonblockingAssignment( true, true );
         ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
         break;
      case tok::l_brace:
         ParseConcatenation();
         if( Tok.isNot(tok::equal) && Tok.isNot(tok::equalgreater) ){
            Diag(Tok, diag::err_expected_assign_operator);
            SkipUntil(tok::semi,true,true);
            break;
         }
         ParseBlockingOrNonblockingAssignment( true, true );
         ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
         break;
      default:
         Diag(Tok, diag::err_unsupported_feature);
         SkipUntil(tok::semi, true, true);
         break;
      }

      ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
      break;
   case tok::kw_release:
   case tok::kw_deassign:
      ConsumeToken();
      switch(Tok.getKind() ){
      case tok::identifier:
         ParseHierarchicalIdentifier();
         break;
      case tok::l_brace:
         ParseConcatenation();
         break;
      default:
         Diag(Tok, diag::err_unsupported_feature);
         SkipUntil(tok::semi, true, true);
         break;
      }
      ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
      break;
   case tok::kw_disable: // Disable statement
      ConsumeToken();
      if(Tok.is(tok::kw_fork) ){
         ConsumeToken();
      } else{
         ParseHierarchicalIdentifier();
      }
      ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
      break;
   case tok::kw_begin:  // seq_block
   case tok::kw_fork:   // par_block
      ParseSeqOrParBlock();
      break;
   case tok::kw_unique: //  case/if statement
   case tok::kw_unique0:
   case tok::kw_priority:
      switch( Tok.getKind() ) {
      case tok::kw_if:
         ParseIfStatement();
         break;
      case tok::kw_case:
      case tok::kw_casez:
      case tok::kw_casex:
         ParseCaseStatement();
         break;
      default:
         Diag(Tok, diag::err_expected_case_or_if_after_pri) << "unique/unique0/priority";
         break;
      }
      break;
   case tok::kw_if: // conditional statement
      ParseIfStatement();
      break;
   case tok::kw_case: // case statement
   case tok::kw_casez:
   case tok::kw_casex:
      ParseCaseStatement();
      break;

   case tok::kw_wait: // wait statement
   case tok::kw_wait_order:
      ParseWaitStatement();
      break;
   case tok::kw_forever: // loop statement
      ConsumeToken();
      ParseStatementOrNull();
      break;

   case tok::kw_repeat: // loop statement
   case tok::kw_while:  // loop statement
      ConsumeToken();

      ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "repeat/while");

      result = ParseExpression(prec::Assignment);

      ExpectAndConsume(tok::r_paren, diag::err_expected_rparen, "", tok::r_paren);

      ParseStatementOrNull();
      break;

   case tok::kw_for: // loop statement
      ParseForLoop();
      break;

   case tok::kw_do:
   case tok::kw_foreach:
      assert( 0 && "do/foreach not implemented" );
      break;

   case tok::hash: // procedrual_timing_control_statement
      ParseDelayControl();
      ParseStatementOrNull();
      break;

   case tok::at: // procedural_timing_control_statement
      ParseEventControl();
      ParseStatementOrNull();
      break;
      // TODO - cycle_delay

   case tok::l_brace:
      ParseConcatenation();
      switch(Tok.getKind()) {
      case tok::lessequal:
      case tok::equal:
         ParseBlockingOrNonblockingAssignment( true, true );
         ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
         break;
      default:
         assert(0 && "Not Implented\n");
         break;
      }
      break;
   case tok::identifier:
      ParseHierarchicalIdentifier();
      switch(Tok.getKind()) {
      case tok::l_paren: /* Sub or Functions */
         ParseTfCall(false,llvm::StringRef(""));
         ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
         break;
      case tok::semi: /* Sub or Function */
         ConsumeToken();
         break;
      case tok::lessequal:
      case tok::equal:
         ParseBlockingOrNonblockingAssignment( true, true );
         ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
         break;
         // inc_or_dec_operator
      case tok::minusminus:
      case tok::plusplus:
         ConsumeToken();
         ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
         break;
      default:
         assert(0 && "Not Implented\n");
         break;
      }
      break;

      // void ' ( function_subroutine_call ) ;
   case tok::kw_void:
      ConsumeToken();

      ExpectAndConsume(tok::quote, diag::err_expected_quote_after_void);

      ExpectAndConsume(tok::l_paren, diag::err_expected_lparen);
      // TODO: Handle error

      ParseTfCall(true,llvm::StringRef(""));

      ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
      // TODO: Handle error

      ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
      break;

   case tok::arrow:
      ParseEventTrigger();
      break;

      //case tok::implication:
      //case tok::implication_nonblock:
      //   if( !ParseEventTrigger() ) {
      //      // This should never happen
      //      printf("ERROR: Internal error.  Expected even trigger\n");
      //   }
      //   break;
   default:
      Diag(Tok, diag::err_expected_statement);
      SkipUntil(tok::semi, false, false);
      break;
   }

   return true;
}

bool Parser::ParseFunctionStatement()
{
   return false;
}
bool Parser::ParseFunctionStatementOrNull()
{
   return false;
}

// Section A.6.5 - Timing controls
// delay_or_event_control ::=
//   delay_control
// | event_control
// | repeat ( expression ) event_control
bool Parser::ParseDelayOrEventControl()
{
   if( ParseDelayControl() ) {
      return true;
   } else if (ParseEventControl() ) {
      return true;
   } else if (ConsumeIfMatch(tok::kw_repeat) ) {
      ConsumeToken();
      if( Tok.isNot(tok::l_paren)) {
         Diag(Tok, diag::err_expected_lparen_after) << "repeat/while";
         // TODO: Handle error
      }

      ParseExpression(prec::Assignment);

      ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
      ParseEventControl();
   }

   // TODO: Should always find one of the above
   return true;
}

// delay_control ::=
//   # delay_value
// | # ( mintypmax_expression )
bool Parser::ParseDelayControl()
{
   if( Tok.isNot(tok::hash) ){
      return false;
   }

   ConsumeToken();

   if( Tok.isNot(tok::l_paren) ){
      ParseDelayValue();
      return true;
   }


   ConsumeParen();
   auto result = ParseMintypmaxExpression();
   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   return true;
}

// event_control ::=
//    @ hierarchical_event_identifier -- TODO
//  | @ ( event_expression )
//  | @*
//  | @ (*)
//  | @ ps_or_hierarchical_sequence_identifier -- TODO
bool Parser::ParseEventControl()
{
   if( Tok.isNot(tok::at) ){
      return false;
   }
   ConsumeToken();

   // Look for not '(' versions first
   if( Tok.isNot(tok::l_paren) ){
      if ( !ConsumeIfMatch( tok::star ) &&
         !ParseHierarchicalIdentifier()) {
            // TODO: Handle error
            //            printf( "ERROR: Expected some event expression\n" );
            //            process_error();
      }
      return true;
   }

   ConsumeParen();
   if( Tok.is(tok::star) ){
      ConsumeToken();
   } else {
      if(!ParseEventExpression()){
         // TODO: Handle error
      }
   }

   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   return true;
}

// event_expression ::=
//   [ edge_identifier ] expression [ iff expression ]
// | sequence_instance              [ iff expression ] -- TODO
// | event_expression or event_expression
// | event_expression , event_expression
// | ( event_expression )
bool Parser::ParseEventExpression()
{
   do {
      switch(Tok.getKind()){
         // edge_identifier
      case tok::kw_posedge:
      case tok::kw_negedge:
      case tok::kw_edge:
         ConsumeToken();
         ParseExpression(prec::Assignment);
         break;
      case tok::l_paren:
         ConsumeParen();
         ParseEventExpression();
         ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
         break;
      default:
         ParseExpression(prec::Assignment);
         break;
      }

   } while( ConsumeIfMatch(tok::kw_or) || ConsumeIfMatch( tok::comma) );
   return true;
}

// procedural_timing_control ::=
//    delay_control
//  | event_control
//  | cycle_delay -- TODO
bool Parser::ParseProceduralTimingControl()
{
   if( ParseDelayControl() ) {
      return true;
   }

   if( ParseEventControl() ) {
      return true;
   }

   return false;
}
bool Parser::ParseJumpStatement()
{
   return false;
}

// wait_statement ::=
//   wait ( expression ) statement_or_null
// | wait fork ;
// | wait_order ( hierarchical_identifier { , hierarchical_identifier } ) action_block
bool Parser::ParseWaitStatement()
{
   assert(Tok.is(tok::kw_wait) || Tok.is(tok::kw_wait_order));

   ConsumeToken();
   if( Tok.is(tok::kw_fork) ) {
      ConsumeToken();
      ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);
      return true;
   }

   ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after,"wait");
   auto result = ParseExpression(prec::Assignment);
   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   ParseStatementOrNull();
   return true;
}

// event_trigger ::=
//    -> hierarchical_event_identifier ;
// | ->> [ delay_or_event_control ] hierarchical_event_identifier ;
bool Parser::ParseEventTrigger()
{
   assert(Tok.is(tok::arrow) );

   ConsumeToken();
   ParseHierarchicalIdentifier();
   ExpectAndConsumeSemi(diag::err_expected_semi_after_stmt);

   return true;
}

// Section A.6.6 - Conditional Statements

// conditional_statement ::=
//   [ unique_priority ] if ( cond_predicate ) statement_or_null
//   { else if ( cond_predicate ) statement_or_null }
//   [ else statement_or_null ]
// unique_priority ::= unique | unique0 | priority

void Parser::ParseIfStatement()
{
   bool found_final_else = false;

   ParseUniquePriority();

   assert(Tok.is(tok::kw_if));
   ConsumeToken();

   ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "if");
   ParseCondPredicate(true);
   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   ParseStatementOrNull();

   while( !found_final_else && ConsumeIfMatch( tok::kw_else ) ) {
      if( Tok.is(tok::kw_if) ){
         ConsumeToken();
         ParseCondPredicate(true);
      } else {
         found_final_else = true;
      }

      ParseStatementOrNull();
   }
}
bool Parser::ParseUniquePriority()
{
   return false;
}

// cond_predicate ::=
//   expression_or_cond_pattern { &&& expression_or_cond_pattern } -- TODO: &&&
// expression_or_cond_pattern ::= expression | cond_pattern
// cond_pattern ::= expression matches pattern -- TODO
void Parser::ParseCondPredicate( bool require_paren )
{
   do {
      auto exprResult = ParseExpression(prec::Assignment);
   } while( ConsumeIfMatch(tok::ampampamp));

}


// Section A.6.7 - Case statement

//  case_statement ::=
//  [ unique_priority ] case_keyword ( case_expression )
//    case_item { case_item } endcase
//  | [ unique_priority ] case_keyword (case_expression )matches
//    case_pattern_item { case_pattern_item } endcase
//  | [ unique_priority ] case ( case_expression ) inside
//    case_inside_item { case_inside_item } endcase
//  case_item ::= 
//    case_item_expression { , case_item_expression } : statement_or_null
//  | default [ : ] statement_or_null
void Parser::ParseCaseStatement()
{
   ParseUniquePriority();
   assert(Tok.is(tok::kw_case) || Tok.is(tok::kw_casez) || Tok.is(tok::kw_casex));

   ConsumeToken();

   ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "case keyword");

   auto result = ParseExpression(prec::Assignment);

   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen);
   do{
      bool lastWasDefault = false;
      do {
         if( Tok.is(tok::kw_default) ){
            ConsumeToken();
            lastWasDefault = true;
         } else {
            ParseExpression(prec::Assignment);
            lastWasDefault = false;
         }
      } while (ConsumeIfMatch(tok::comma));

      if( Tok.is(tok::colon) ){
         ConsumeToken();
      } else if( !lastWasDefault) {
         Diag(Tok, diag::err_expected_colon_after) << "case item";
      }

      ParseStatementOrNull();
   } while(Tok.isNot(tok::kw_endcase));

   ConsumeToken();
}


bool Parser::ParseCasePatternItem()
{
   return false;
}
bool Parser::ParseCaseInsideItem()
{
   return false;
}
bool Parser::ParseRandcaseStatement()
{
   return false;
}
bool Parser::ParseRandcaseItem()
{
   return false;
}

// Section A.6.7.1 - Patterns
// TODO

// Section A.6.8 - Looping
bool Parser::ParseForVariableDeclaration()
{
   return false;
}


//   for ( for_initialization ; expression ; for_step )
//     statement_or_null
// for_step ::= for_step_assignment { , for_step_assignment }
// for_step_assignment ::= operator_assignment
//                       | inc_or_dec_expression
//                       | function_subroutine_call
// inc_or_dec_expression ::= inc_or_dec_operator { attribute_instance } variable_lvalue
//                         | variable_lvalue { attribute_instance } inc_or_dec_operator
bool Parser::ParseForLoop(){
   assert(Tok.is(tok::kw_for));
   ConsumeToken();

   ExpectAndConsume(tok::l_paren, diag::err_expected_lparen_after, "for", tok::semi);

   // Initial value
   ParseListOfAssignments();
   ExpectAndConsumeSemi(diag::err_expected_semi_after_expr);

   // End condition
   ParseExpression(prec::Assignment);
   ExpectAndConsumeSemi(diag::err_expected_semi_after_expr);

   // Step expression
   bool allow_next   = false;
   do {
      if( ParseOperatorAssignment() ) {
         allow_next = ConsumeIfMatch( tok::comma );
      } else if ( ParseIncOrDecExpression().isInvalid() ) {
         allow_next = ConsumeIfMatch( tok::comma );
         //      } else if ( ParseSubroutineCall() ) {
         //         allow_next = ConsumeIfMatch( tok::comma );
      } else if ( allow_next ) {
         //printf( "ERROR: Expected step assignment\n" );
         //process_error();
      } else {
         break;
      }
   } while ( allow_next );

   ExpectAndConsume(tok::r_paren, diag::err_expected_rparen, "", tok::r_paren);

   // Loop statement
   ParseStatementOrNull();

   return true;
}

bool Parser::ParseLoopVariables()
{
   return false;
}

// Section A.6.10 - Assertion statements
// Section A.6.11 - Clocking block
// Section A.6.12 - Randsequence
// TODO
