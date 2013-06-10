//===--- Specifiers.h - Declaration and Type Specifiers ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief Defines various enumerations that describe declaration and
/// type specifiers.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_BASIC_SPECIFIERS_H
#define LLVM_VLANG_BASIC_SPECIFIERS_H

namespace vlang {

   /// \brief Specifies the data type category
   enum class TypeDataCategory {
      TDC_singular,
      TDC_aggregate
   };

   enum class TypeDataObject {
      TDO_variable,
      TDO_net
   };

   /// \brief Specifies the number of states for each bit
  enum class TypeDataState{
     TDS_2state, // Verilog 2-state per-bit (0, 1)
     TDS_3state, // Internal 3-state per-bit (0, 1, X)
     TDS_4state  // Verilog 4-state per-bit (0, 1, X, Z)
  };

  /// \brief Specifies the signedness of a type, e.g., signed or unsigned.
  enum class TypeSpecifierSign {
    TSS_unspecified,
    TSS_signed,
    TSS_unsigned
  };
  
  /// \brief Specifies the kind of type.
  enum class TypeSpecifierType {
    TST_unspecified,
    TST_void,
    TST_error         // erroneous type
  };

  /// \brief A C++ access specifier (public, private, protected), plus the
  /// special value "none" which means different things in different contexts.
  enum class AccessSpecifier {
    AS_public,
    AS_protected,
    AS_private,
    AS_none
  };

  /// \brief The categorization of expression values, currently following the
  /// C++11 scheme.
  enum class ExprValueKind {
    /// \brief An r-value expression (a pr-value in the C++11 taxonomy)
    /// produces a temporary value.
    VK_RValue,

    /// \brief An l-value expression is a reference to an object with
    /// independent storage.
    VK_LValue,

    /// \brief An x-value expression is a reference to an object with
    /// independent storage but which can be "moved", i.e.
    /// efficiently cannibalized for its resources.
    VK_XValue
  };

  /// \brief In-class initialization styles for non-static data members.
  enum class InClassInitStyle {
    ICIS_NoInit,   ///< No in-class initializer.
    ICIS_CopyInit, ///< Copy initialization.
    ICIS_ListInit  ///< Direct list-initialization.
  };

} // end namespace vlang

#endif // LLVM_VLANG_BASIC_SPECIFIERS_H
