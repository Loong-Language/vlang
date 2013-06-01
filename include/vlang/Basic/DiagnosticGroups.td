//==--- DiagnosticGroups.td - Diagnostic Group Definitions ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

def ImplicitFunctionDeclare : DiagGroup<"implicit-function-declaration">;
def ImplicitInt : DiagGroup<"implicit-int">;

// Aggregation warning settings.
def Implicit : DiagGroup<"implicit", [
    ImplicitFunctionDeclare,
    ImplicitInt
]>;

// Empty DiagGroups are recognized by vlang but ignored.
def : DiagGroup<"abi">;
def : DiagGroup<"address">;
def AddressOfTemporary : DiagGroup<"address-of-temporary">;
def : DiagGroup<"aggregate-return">;
def AmbigMemberTemplate : DiagGroup<"ambiguous-member-template">;
def ArrayBounds : DiagGroup<"array-bounds">;
def ArrayBoundsPointerArithmetic : DiagGroup<"array-bounds-pointer-arithmetic">;
def Availability : DiagGroup<"availability">;
def Section : DiagGroup<"section">;
def BadArrayNewLength : DiagGroup<"bad-array-new-length">;
def BuiltinMacroRedefined : DiagGroup<"builtin-macro-redefined">;
def BuiltinRequiresHeader : DiagGroup<"builtin-requires-header">;
def C99Compat : DiagGroup<"c99-compat">;
def CXXCompat: DiagGroup<"c++-compat">;
def CastAlign : DiagGroup<"cast-align">;
def : DiagGroup<"cast-qual">;
def : DiagGroup<"char-align">;
def Comment : DiagGroup<"comment">;
def ConfigMacros : DiagGroup<"config-macros">;
def : DiagGroup<"ctor-dtor-privacy">;
def GNUDesignator : DiagGroup<"gnu-designator">;

def DeleteNonVirtualDtor : DiagGroup<"delete-non-virtual-dtor">;
def AbstractFinalClass : DiagGroup<"abstract-final-class">;

def : DiagGroup<"disabled-optimization">;
def : DiagGroup<"discard-qual">;
def : DiagGroup<"div-by-zero">;

def DocumentationHTML : DiagGroup<"documentation-html">;
def DocumentationUnknownCommand : DiagGroup<"documentation-unknown-command">;
def DocumentationPedantic : DiagGroup<"documentation-pedantic",
                                      [DocumentationUnknownCommand]>;
def DocumentationDeprecatedSync : DiagGroup<"documentation-deprecated-sync">;
def Documentation : DiagGroup<"documentation",
                              [DocumentationHTML,
                               DocumentationDeprecatedSync]>;

def EmptyBody : DiagGroup<"empty-body">;
def ExtraTokens : DiagGroup<"extra-tokens">;
def CXX11ExtraSemi : DiagGroup<"c++11-extra-semi">;
def ExtraSemi : DiagGroup<"extra-semi", [CXX11ExtraSemi]>;

def FormatExtraArgs : DiagGroup<"format-extra-args">;
def FormatZeroLength : DiagGroup<"format-zero-length">;

// Warnings for C++1y code which is not compatible with prior C++ standards.
def CXXPre1yCompat : DiagGroup<"cxx98-cxx11-compat">;
def CXXPre1yCompatPedantic : DiagGroup<"cxx98-cxx11-compat-pedantic",
                                       [CXXPre1yCompat]>;

def CXX98CompatBindToTemporaryCopy :
  DiagGroup<"c++98-compat-bind-to-temporary-copy">;
def CXX98CompatLocalTypeTemplateArgs :
  DiagGroup<"c++98-compat-local-type-template-args">;
def CXX98CompatUnnamedTypeTemplateArgs :
  DiagGroup<"c++98-compat-unnamed-type-template-args">;

def CXX98Compat : DiagGroup<"c++98-compat",
                            [CXX98CompatBindToTemporaryCopy,
                             CXX98CompatLocalTypeTemplateArgs,
                             CXX98CompatUnnamedTypeTemplateArgs,
                             CXXPre1yCompat]>;
// Warnings for C++11 features which are Extensions in C++98 mode.
def CXX98CompatPedantic : DiagGroup<"c++98-compat-pedantic",
                                    [CXX98Compat,
                                     CXXPre1yCompatPedantic]>;

def CXX11Narrowing : DiagGroup<"c++11-narrowing">;

// Original name of this warning in Vlang
def : DiagGroup<"c++0x-narrowing", [CXX11Narrowing]>;

// Name of this warning in GCC
def : DiagGroup<"narrowing", [CXX11Narrowing]>;

def CXX11CompatReservedUserDefinedLiteral :
  DiagGroup<"c++11-compat-reserved-user-defined-literal">;
def ReservedUserDefinedLiteral :
  DiagGroup<"reserved-user-defined-literal",
            [CXX11CompatReservedUserDefinedLiteral]>;

def CXX11Compat : DiagGroup<"c++11-compat",
                            [CXX11Narrowing,
                             CXX11CompatReservedUserDefinedLiteral,
                             CXXPre1yCompat]>;
def : DiagGroup<"c++0x-compat", [CXX11Compat]>;
def CXX11CompatPedantic : DiagGroup<"c++11-compat-pedantic",
                                    [CXXPre1yCompatPedantic]>;

def : DiagGroup<"effc++">;
def DivZero : DiagGroup<"division-by-zero">;
def ExitTimeDestructors : DiagGroup<"exit-time-destructors">;
def FlexibleArrayExtensions : DiagGroup<"flexible-array-extensions">;
def FourByteMultiChar : DiagGroup<"four-char-constants">;
def GlobalConstructors : DiagGroup<"global-constructors">;
def BitwiseOpParentheses: DiagGroup<"bitwise-op-parentheses">;
def LogicalOpParentheses: DiagGroup<"logical-op-parentheses">;
def ShiftOpParentheses: DiagGroup<"shift-op-parentheses">;
def OverloadedShiftOpParentheses: DiagGroup<"overloaded-shift-op-parentheses">;
def DanglingElse: DiagGroup<"dangling-else">;
def DanglingField : DiagGroup<"dangling-field">;
def DistributedObjectModifiers : DiagGroup<"distributed-object-modifiers">;
def IgnoredQualifiers : DiagGroup<"ignored-qualifiers">;
def : DiagGroup<"import">;
def IncompatiblePointerTypesDiscardsQualifiers 
  : DiagGroup<"incompatible-pointer-types-discards-qualifiers">;
def IncompatiblePointerTypes
  : DiagGroup<"incompatible-pointer-types",
    [IncompatiblePointerTypesDiscardsQualifiers]>;
def IncompleteUmbrella : DiagGroup<"incomplete-umbrella">;
def InvalidNoreturn : DiagGroup<"invalid-noreturn">;
def InvalidSourceEncoding : DiagGroup<"invalid-source-encoding">;
def KNRPromotedParameter : DiagGroup<"knr-promoted-parameter">;
def : DiagGroup<"init-self">;
def : DiagGroup<"inline">;
def : DiagGroup<"invalid-pch">;
def LiteralRange : DiagGroup<"literal-range">;
def LocalTypeTemplateArgs : DiagGroup<"local-type-template-args",
                                      [CXX98CompatLocalTypeTemplateArgs]>;
def MalformedWarningCheck : DiagGroup<"malformed-warning-check">;
def Main : DiagGroup<"main">;
def MainReturnType : DiagGroup<"main-return-type">;
def MissingBraces : DiagGroup<"missing-braces">;
def MissingDeclarations: DiagGroup<"missing-declarations">;
def : DiagGroup<"missing-format-attribute">;
def : DiagGroup<"missing-include-dirs">;
def MissingNoreturn : DiagGroup<"missing-noreturn">;
def MultiChar : DiagGroup<"multichar">;
def : DiagGroup<"nested-externs">;
def CXX11LongLong : DiagGroup<"c++11-long-long">;
def LongLong : DiagGroup<"long-long", [CXX11LongLong]>;
def MethodSignatures : DiagGroup<"method-signatures">;
def MismatchedParameterTypes : DiagGroup<"mismatched-parameter-types">;
def MismatchedReturnTypes : DiagGroup<"mismatched-return-types">;
def MismatchedTags : DiagGroup<"mismatched-tags">;
def MissingFieldInitializers : DiagGroup<"missing-field-initializers">;
def ModuleConflict : DiagGroup<"module-conflict">;
def NullArithmetic : DiagGroup<"null-arithmetic">;
def NullCharacter : DiagGroup<"null-character">;
def NullDereference : DiagGroup<"null-dereference">;
def InitializerOverrides : DiagGroup<"initializer-overrides">;
def NonNull : DiagGroup<"nonnull">;
def NonPODVarargs : DiagGroup<"non-pod-varargs">;
def : DiagGroup<"nonportable-cfstrings">;
def NonVirtualDtor : DiagGroup<"non-virtual-dtor">;
def OveralignedType : DiagGroup<"over-aligned">;
def : DiagGroup<"old-style-cast">;
def : DiagGroup<"old-style-definition">;
def OutOfLineDeclaration : DiagGroup<"out-of-line-declaration">;
def : DiagGroup<"overflow">;
def ForwardClassReceiver : DiagGroup<"receiver-forward-class">;
def MethodAccess : DiagGroup<"objc-method-access">;
def ObjCReceiver : DiagGroup<"receiver-expr">;
def OverlengthStrings : DiagGroup<"overlength-strings">;
def OverloadedVirtual : DiagGroup<"overloaded-virtual">;
def PrivateExtern : DiagGroup<"private-extern">;
def SelTypeCast : DiagGroup<"cast-of-sel-type">;
def BadFunctionCast : DiagGroup<"bad-function-cast">;
def ObjCPropertyImpl : DiagGroup<"objc-property-implementation">;
def ObjCPropertyNoAttribute : DiagGroup<"objc-property-no-attribute">;
def ObjCMissingSuperCalls : DiagGroup<"objc-missing-super-calls">;
def ObjCRetainBlockProperty : DiagGroup<"objc-noncopy-retain-block-property">;
def ObjCReadonlyPropertyHasSetter : DiagGroup<"objc-readonly-with-setter-property">;
def ObjCRootClass : DiagGroup<"objc-root-class">;
def DeprecatedObjCIsaUsage : DiagGroup<"deprecated-objc-isa-usage">;
def Packed : DiagGroup<"packed">;
def Padded : DiagGroup<"padded">;
def PointerArith : DiagGroup<"pointer-arith">;
def PoundWarning : DiagGroup<"#warnings">,
                   DiagCategory<"#warning Directive">;
def PoundPragmaMessage : DiagGroup<"#pragma-messages">,
                         DiagCategory<"#pragma message Directive">;
def : DiagGroup<"pointer-to-int-cast">;
def : DiagGroup<"redundant-decls">;
def ReturnStackAddress : DiagGroup<"return-stack-address">;
def ReturnTypeCLinkage : DiagGroup<"return-type-c-linkage">;
def ReturnType : DiagGroup<"return-type", [ReturnTypeCLinkage]>;
def BindToTemporaryCopy : DiagGroup<"bind-to-temporary-copy",
                                    [CXX98CompatBindToTemporaryCopy]>;
def SelfAssignmentField : DiagGroup<"self-assign-field">;
def SelfAssignment : DiagGroup<"self-assign", [SelfAssignmentField]>;
def SemiBeforeMethodBody : DiagGroup<"semicolon-before-method-body">;
def Sentinel : DiagGroup<"sentinel">;
def MissingMethodReturnType : DiagGroup<"missing-method-return-type">;
def Shadow : DiagGroup<"shadow">;
def Shorten64To32 : DiagGroup<"shorten-64-to-32">;
def : DiagGroup<"sign-promo">;
def SignCompare : DiagGroup<"sign-compare">;
def : DiagGroup<"stack-protector">;
def : DiagGroup<"switch-default">;
def : DiagGroup<"synth">;
def SizeofArrayArgument : DiagGroup<"sizeof-array-argument">;
def SizeofArrayDecay : DiagGroup<"sizeof-array-decay">;
def SizeofPointerMemaccess : DiagGroup<"sizeof-pointer-memaccess">;
def StaticInInline : DiagGroup<"static-in-inline">;
def StaticLocalInInline : DiagGroup<"static-local-in-inline">;
def GNUStaticFloatInit : DiagGroup<"gnu-static-float-init">;
def StaticFloatInit : DiagGroup<"static-float-init", [GNUStaticFloatInit]>;
def StringPlusInt : DiagGroup<"string-plus-int">;
def StrncatSize : DiagGroup<"strncat-size">;
def TautologicalOutOfRangeCompare : DiagGroup<"tautological-constant-out-of-range-compare">;
def TautologicalCompare : DiagGroup<"tautological-compare",
                                    [TautologicalOutOfRangeCompare]>;
def HeaderHygiene : DiagGroup<"header-hygiene">;
def DuplicateDeclSpecifier : DiagGroup<"duplicate-decl-specifier">;
def CompareDistinctPointerType : DiagGroup<"compare-distinct-pointer-types">;

def Unsequenced : DiagGroup<"unsequenced">;
// GCC name for -Wunsequenced
def : DiagGroup<"sequence-point", [Unsequenced]>;

// Preprocessor warnings.
def AmbiguousMacro : DiagGroup<"ambiguous-macro">;

def InvalidOffsetof : DiagGroup<"invalid-offsetof">;
def : DiagGroup<"strict-prototypes">;
def StrictSelector : DiagGroup<"strict-selector-match">;
def ImplicitFallthroughPerFunction :
  DiagGroup<"implicit-fallthrough-per-function">;
def ImplicitFallthrough  : DiagGroup<"implicit-fallthrough",
                                     [ImplicitFallthroughPerFunction]>;
def InvalidPPToken : DiagGroup<"invalid-pp-token">;

def : DiagGroup<"type-limits">;
def UndefinedReinterpretCast : DiagGroup<"undefined-reinterpret-cast">;
def ReinterpretBaseClass : DiagGroup<"reinterpret-base-class">;
def Unicode  : DiagGroup<"unicode">;
def UninitializedMaybe : DiagGroup<"conditional-uninitialized">;
def UninitializedSometimes : DiagGroup<"sometimes-uninitialized">;
def UninitializedStaticSelfInit : DiagGroup<"static-self-init">;
def Uninitialized  : DiagGroup<"uninitialized", [UninitializedSometimes,
                                                 UninitializedStaticSelfInit]>;
def UnknownWarningOption : DiagGroup<"unknown-warning-option">;
def UnnamedTypeTemplateArgs : DiagGroup<"unnamed-type-template-args",
                                        [CXX98CompatUnnamedTypeTemplateArgs]>;
def UnusedArgument : DiagGroup<"unused-argument">;
def UnusedSanitizeArgument : DiagGroup<"unused-sanitize-argument">;
def UnusedCommandLineArgument : DiagGroup<"unused-command-line-argument",
                                          [UnusedSanitizeArgument]>;
def UnusedComparison : DiagGroup<"unused-comparison">;
def UnusedExceptionParameter : DiagGroup<"unused-exception-parameter">;
def UnneededInternalDecl : DiagGroup<"unneeded-internal-declaration">;
def UnneededMemberFunction : DiagGroup<"unneeded-member-function">;
def UnusedPrivateField : DiagGroup<"unused-private-field">;
def UnusedFunction : DiagGroup<"unused-function", [UnneededInternalDecl]>;
def UnusedMemberFunction : DiagGroup<"unused-member-function",
                                     [UnneededMemberFunction]>;
def UnusedLabel : DiagGroup<"unused-label">;
def UnusedParameter : DiagGroup<"unused-parameter">;
def UnusedResult : DiagGroup<"unused-result">;
def UnusedValue : DiagGroup<"unused-value", [UnusedComparison, UnusedResult]>;
def UnusedVariable : DiagGroup<"unused-variable">;
def Selector : DiagGroup<"selector">;
def Protocol : DiagGroup<"protocol">;
def SuperSubClassMismatch : DiagGroup<"super-class-method-mismatch">;
def OverridingMethodMismatch : DiagGroup<"overriding-method-mismatch">;
def VLA : DiagGroup<"vla">;
def VLAExtension : DiagGroup<"vla-extension">;
def Visibility : DiagGroup<"visibility">;
def ZeroLengthArray : DiagGroup<"zero-length-array">;

def CharSubscript : DiagGroup<"char-subscripts">;
def LargeByValueCopy : DiagGroup<"large-by-value-copy">;
def DuplicateArgDecl : DiagGroup<"duplicate-method-arg">;

// Aggregation warning settings.

// -Widiomatic-parentheses contains warnings about 'idiomatic'
// missing parentheses;  it is off by default.  We do not include it
// in -Wparentheses because most users who use -Wparentheses explicitly
// do not want these warnings.
def ParenthesesOnEquality : DiagGroup<"parentheses-equality">;
def Parentheses : DiagGroup<"parentheses",
                            [LogicalOpParentheses,
                             BitwiseOpParentheses,
                             ShiftOpParentheses,
                             OverloadedShiftOpParentheses,
                             ParenthesesOnEquality,
                             DanglingElse]>;

def Unused : DiagGroup<"unused",
                       [UnusedArgument, UnusedFunction, UnusedLabel,
                        UnusedPrivateField,
                        UnusedValue, UnusedVariable]>,
                        DiagCategory<"Unused Entity Issue">;

// Format settings.
def FormatInvalidSpecifier : DiagGroup<"format-invalid-specifier">;
def FormatSecurity : DiagGroup<"format-security">;
def FormatNonStandard : DiagGroup<"format-non-iso">;
def FormatY2K : DiagGroup<"format-y2k">;
def Format : DiagGroup<"format",
                       [FormatExtraArgs, FormatZeroLength, NonNull,
                        FormatSecurity, FormatY2K, FormatInvalidSpecifier]>,
             DiagCategory<"Format String Issue">;
def FormatNonLiteral : DiagGroup<"format-nonliteral", [FormatSecurity]>;
def Format2 : DiagGroup<"format=2",
                        [FormatNonLiteral, FormatSecurity, FormatY2K]>;

def Extra : DiagGroup<"extra", [
    MissingFieldInitializers,
    IgnoredQualifiers,
    InitializerOverrides,
    SemiBeforeMethodBody,
    MissingMethodReturnType,
    SignCompare,
    UnusedParameter
  ]>;

def Most : DiagGroup<"most", [
    CharSubscript,
    Comment,
    DeleteNonVirtualDtor,
    Format,
    Implicit,
    MismatchedTags,
    MissingBraces,
    MultiChar,
    ReturnType,
    SelfAssignment,
    SizeofArrayArgument,
    SizeofArrayDecay,
    StringPlusInt,
    Uninitialized,
    Unused,
    OverloadedVirtual,
    PrivateExtern,
    SelTypeCast
 ]>;

// Note that putting warnings in -Wall will not disable them by default. If a
// warning should be active _only_ when -Wall is passed in, mark it as
// DefaultIgnore in addition to putting it here.
def : DiagGroup<"all", [Most, Parentheses]>;

// Warnings enabled by -pedantic.  This is magically filled in by TableGen.
def Pedantic : DiagGroup<"pedantic">;

// Aliases.
def : DiagGroup<"", [Extra]>;                   // -W = -Wextra
def : DiagGroup<"endif-labels", [ExtraTokens]>; // -Wendif-labels=-Wendif-tokens
def : DiagGroup<"comments", [Comment]>;         // -Wcomments = -Wcomment

// A warning group for warnings that we want to have on by default in vlang,
// but which aren't on by default in GCC.
def NonGCC : DiagGroup<"non-gcc",
    [SignCompare, LiteralRange]>;

// A warning group for warnings about using C++11 features as extensions in
// earlier C++ versions.
def CXX11 : DiagGroup<"c++11-extensions", [CXX11ExtraSemi, CXX11LongLong]>;

// A warning group for warnings about using C++1y features as extensions in
// earlier C++ versions.
def CXX1y : DiagGroup<"c++1y-extensions">;

def : DiagGroup<"c++0x-extensions", [CXX11]>;
def DelegatingCtorCycles :
  DiagGroup<"delegating-ctor-cycles">;

// A warning group for warnings about using C11 features as extensions.
def C11 : DiagGroup<"c11-extensions">;

// A warning group for warnings about using C99 features as extensions.
def C99 : DiagGroup<"c99-extensions">;

// A warning group for warnings about GCC extensions.
def GNU : DiagGroup<"gnu", [GNUDesignator, VLAExtension,
                            ZeroLengthArray, GNUStaticFloatInit]>;
