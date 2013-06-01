//===--- InitPreprocessor.cpp - PP initialization code. ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the vlang::InitializePreprocessor function.
//
//===----------------------------------------------------------------------===//

#include "vlang/Frontend/Utils.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/MacroBuilder.h"
#include "vlang/Basic/SourceManager.h"
#include "vlang/Basic/TargetInfo.h"
#include "vlang/Basic/Version.h"
#include "vlang/Frontend/FrontendDiagnostic.h"
#include "vlang/Frontend/FrontendOptions.h"
#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Lex/Preprocessor.h"
#include "vlang/Lex/PreprocessorOptions.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"
using namespace vlang;

// Append a #define line to Buf for Macro.  Macro should be of the form XXX,
// in which case we emit "`define XXX 1" or "XXX=Y z W" in which case we emit
// "`define XXX Y z W".  To get a `define with no value, use "XXX=".
static void DefineBuiltinMacro(MacroBuilder &Builder, StringRef Macro,
                               DiagnosticsEngine &Diags) {
  std::pair<StringRef, StringRef> MacroPair = Macro.split('=');
  StringRef MacroName = MacroPair.first;
  StringRef MacroBody = MacroPair.second;
  if (MacroName.size() != Macro.size()) {
    // Per GCC -D semantics, the macro ends at \n if it exists.
    StringRef::size_type End = MacroBody.find_first_of("\n\r");
    if (End != StringRef::npos)
      Diags.Report(diag::warn_fe_macro_contains_embedded_newline)
        << MacroName;
    Builder.defineMacro(MacroName, MacroBody.substr(0, End));
  } else {
    // Push "macroname 1".
    Builder.defineMacro(Macro);
  }
}

/// AddImplicitInclude - Add an implicit \#include of the specified file to the
/// predefines buffer.
static void AddImplicitInclude(MacroBuilder &Builder, StringRef File,
                               FileManager &FileMgr) {
  Builder.append(Twine("#include \"") +
                 HeaderSearch::NormalizeDashIncludePath(File, FileMgr) + "\"");
}

static void AddImplicitIncludeMacros(MacroBuilder &Builder,
                                     StringRef File,
                                     FileManager &FileMgr) {
  Builder.append(Twine("#__include_macros \"") +
                 HeaderSearch::NormalizeDashIncludePath(File, FileMgr) + "\"");
  // Marker token to stop the __include_macros fetch loop.
  Builder.append("##"); // ##?
}

/// PickFP - This is used to pick a value based on the FP semantics of the
/// specified FP model.
template <typename T>
static T PickFP(const llvm::fltSemantics *Sem, T IEEESingleVal,
                T IEEEDoubleVal, T X87DoubleExtendedVal, T PPCDoubleDoubleVal,
                T IEEEQuadVal) {
  if (Sem == (const llvm::fltSemantics*)&llvm::APFloat::IEEEsingle)
    return IEEESingleVal;
  if (Sem == (const llvm::fltSemantics*)&llvm::APFloat::IEEEdouble)
    return IEEEDoubleVal;
  if (Sem == (const llvm::fltSemantics*)&llvm::APFloat::x87DoubleExtended)
    return X87DoubleExtendedVal;
  if (Sem == (const llvm::fltSemantics*)&llvm::APFloat::PPCDoubleDouble)
    return PPCDoubleDoubleVal;
  assert(Sem == (const llvm::fltSemantics*)&llvm::APFloat::IEEEquad);
  return IEEEQuadVal;
}

/// DefineTypeSize - Emit a macro to the predefines buffer that declares a macro
/// named MacroName with the max value for a type with width 'TypeWidth' a
/// signedness of 'isSigned' and with a value suffix of 'ValSuffix' (e.g. LL).
static void DefineTypeSize(StringRef MacroName, unsigned TypeWidth,
                           StringRef ValSuffix, bool isSigned,
                           MacroBuilder &Builder) {
  llvm::APInt MaxVal = isSigned ? llvm::APInt::getSignedMaxValue(TypeWidth)
                                : llvm::APInt::getMaxValue(TypeWidth);
  Builder.defineMacro(MacroName, MaxVal.toString(10, isSigned) + ValSuffix);
}

/// DefineTypeSize - An overloaded helper that uses TargetInfo to determine
/// the width, suffix, and signedness of the given type
static void DefineTypeSize(StringRef MacroName, TargetInfo::IntType Ty,
                           const TargetInfo &TI, MacroBuilder &Builder) {
  DefineTypeSize(MacroName, TI.getTypeWidth(Ty), TI.getTypeConstantSuffix(Ty), 
                 TI.isTypeSigned(Ty), Builder);
}

static void DefineType(const Twine &MacroName, TargetInfo::IntType Ty,
                       MacroBuilder &Builder) {
  Builder.defineMacro(MacroName, TargetInfo::getTypeName(Ty));
}

static void DefineTypeWidth(StringRef MacroName, TargetInfo::IntType Ty,
                            const TargetInfo &TI, MacroBuilder &Builder) {
  Builder.defineMacro(MacroName, Twine(TI.getTypeWidth(Ty)));
}

static void DefineTypeSizeof(StringRef MacroName, unsigned BitWidth,
                             const TargetInfo &TI, MacroBuilder &Builder) {
  Builder.defineMacro(MacroName,
                      Twine(BitWidth / TI.getCharWidth()));
}

static void DefineExactWidthIntType(TargetInfo::IntType Ty, 
                               const TargetInfo &TI, MacroBuilder &Builder) {
  int TypeWidth = TI.getTypeWidth(Ty);

  // Use the target specified int64 type, when appropriate, so that [u]int64_t
  // ends up being defined in terms of the correct type.
  if (TypeWidth == 64)
    Ty = TI.getInt64Type();

  DefineType("__INT" + Twine(TypeWidth) + "_TYPE__", Ty, Builder);

  StringRef ConstSuffix(TargetInfo::getTypeConstantSuffix(Ty));
  if (!ConstSuffix.empty())
    Builder.defineMacro("__INT" + Twine(TypeWidth) + "_C_SUFFIX__",
                        ConstSuffix);
}

/// Get the value the ATOMIC_*_LOCK_FREE macro should have for a type with
/// the specified properties.
static const char *getLockFreeValue(unsigned TypeWidth, unsigned TypeAlign,
                                    unsigned InlineWidth) {
  // Fully-aligned, power-of-2 sizes no larger than the inline
  // width will be inlined as lock-free operations.
  if (TypeWidth == TypeAlign && (TypeWidth & (TypeWidth - 1)) == 0 &&
      TypeWidth <= InlineWidth)
    return "2"; // "always lock free"
  // We cannot be certain what operations the lib calls might be
  // able to implement as lock-free on future processors.
  return "1"; // "sometimes lock free"
}

static void InitializeStandardPredefinedMacros(const TargetInfo &TI,
                                               const LangOptions &LangOpts,
                                               const FrontendOptions &FEOpts,
                                               MacroBuilder &Builder) {
}

static void InitializePredefinedMacros(const TargetInfo &TI,
                                       const LangOptions &LangOpts,
                                       const FrontendOptions &FEOpts,
                                       MacroBuilder &Builder) {

  if (TI.getShortWidth() > TI.getCharWidth())
    DefineExactWidthIntType(TargetInfo::SignedShort, TI, Builder);

  if (TI.getIntWidth() > TI.getShortWidth())
    DefineExactWidthIntType(TargetInfo::SignedInt, TI, Builder);

  if (TI.getLongWidth() > TI.getIntWidth())
    DefineExactWidthIntType(TargetInfo::SignedLong, TI, Builder);

  if (TI.getLongLongWidth() > TI.getLongWidth())
    DefineExactWidthIntType(TargetInfo::SignedLongLong, TI, Builder);

  // Used by libstdc++ to implement ATOMIC_<foo>_LOCK_FREE.
  unsigned InlineWidthBits = TI.getMaxAtomicInlineWidth();

  int Dig = PickFP(&TI.getLongDoubleFormat(), -1/*FIXME*/, 17, 21, 33, 36);
  Builder.defineMacro("__DECIMAL_DIG__", Twine(Dig));

  // Define a macro that exists only when using the static analyzer.
  if (FEOpts.ProgramAction == frontend::RunAnalysis)
    Builder.defineMacro("__vlang_analyzer__");


  // Get other target #defines.
  TI.getTargetDefines(LangOpts, Builder);
}

// Initialize the remapping of files to alternative contents, e.g.,
// those specified through other files.
static void InitializeFileRemapping(DiagnosticsEngine &Diags,
                                    SourceManager &SourceMgr,
                                    FileManager &FileMgr,
                                    const PreprocessorOptions &InitOpts) {
  // Remap files in the source manager (with buffers).
  for (PreprocessorOptions::const_remapped_file_buffer_iterator
         Remap = InitOpts.remapped_file_buffer_begin(),
         RemapEnd = InitOpts.remapped_file_buffer_end();
       Remap != RemapEnd;
       ++Remap) {
    // Create the file entry for the file that we're mapping from.
    const FileEntry *FromFile = FileMgr.getVirtualFile(Remap->first,
                                                Remap->second->getBufferSize(),
                                                       0);
    if (!FromFile) {
      Diags.Report(diag::err_fe_remap_missing_from_file)
        << Remap->first;
      if (!InitOpts.RetainRemappedFileBuffers)
        delete Remap->second;
      continue;
    }

    // Override the contents of the "from" file with the contents of
    // the "to" file.
    SourceMgr.overrideFileContents(FromFile, Remap->second,
                                   InitOpts.RetainRemappedFileBuffers);
  }

  // Remap files in the source manager (with other files).
  for (PreprocessorOptions::const_remapped_file_iterator
         Remap = InitOpts.remapped_file_begin(),
         RemapEnd = InitOpts.remapped_file_end();
       Remap != RemapEnd;
       ++Remap) {
    // Find the file that we're mapping to.
    const FileEntry *ToFile = FileMgr.getFile(Remap->second);
    if (!ToFile) {
      Diags.Report(diag::err_fe_remap_missing_to_file)
      << Remap->first << Remap->second;
      continue;
    }
    
    // Create the file entry for the file that we're mapping from.
    const FileEntry *FromFile = FileMgr.getVirtualFile(Remap->first,
                                                       ToFile->getSize(), 0);
    if (!FromFile) {
      Diags.Report(diag::err_fe_remap_missing_from_file)
      << Remap->first;
      continue;
    }
    
    // Override the contents of the "from" file with the contents of
    // the "to" file.
    SourceMgr.overrideFileContents(FromFile, ToFile);
  }

  SourceMgr.setOverridenFilesKeepOriginalName(
                                        InitOpts.RemappedFilesKeepOriginalName);
}

/// InitializePreprocessor - Initialize the preprocessor getting it and the
/// environment ready to process a single file. This returns true on error.
///
void vlang::InitializePreprocessor(Preprocessor &PP,
                                   const PreprocessorOptions &InitOpts,
                                   const HeaderSearchOptions &HSOpts) {
  const LangOptions &LangOpts = PP.getLangOpts();
  std::string PredefineBuffer;
  PredefineBuffer.reserve(4080);
  llvm::raw_string_ostream Predefines(PredefineBuffer);
  MacroBuilder Builder(Predefines);

  InitializeFileRemapping(PP.getDiagnostics(), PP.getSourceManager(),
                          PP.getFileManager(), InitOpts);

  // Emit line markers for various builtin sections of the file.  We don't do
  // this in asm preprocessor mode, because "# 4" is not a line marker directive
  // in this mode.
  Builder.append("# 1 \"<built-in>\" 3");

  // Add on the predefines from the driver.  Wrap in a #line directive to report
  // that they come from the command line.
  Builder.append("# 1 \"<command line>\" 1");

  // Process #define's and #undef's in the order they are given.
  for (unsigned i = 0, e = InitOpts.Macros.size(); i != e; ++i) {
    if (InitOpts.Macros[i].second)  // isUndef
      Builder.undefineMacro(InitOpts.Macros[i].first);
    else
      DefineBuiltinMacro(Builder, InitOpts.Macros[i].first,
                         PP.getDiagnostics());
  }

  // If -imacros are specified, include them now.  These are processed before
  // any -include directives.
  for (unsigned i = 0, e = InitOpts.MacroIncludes.size(); i != e; ++i)
    AddImplicitIncludeMacros(Builder, InitOpts.MacroIncludes[i],
                             PP.getFileManager());

  // Process -include directives.
  for (unsigned i = 0, e = InitOpts.Includes.size(); i != e; ++i) {
    const std::string &Path = InitOpts.Includes[i];
    AddImplicitInclude(Builder, Path, PP.getFileManager());
  }

  // Exit the command line and go back to <built-in> (2 is LC_LEAVE).
  Builder.append("# 1 \"<built-in>\" 2");

  // Instruct the preprocessor to skip the preamble.
  PP.setSkipMainFilePreamble(InitOpts.PrecompiledPreambleBytes.first,
                             InitOpts.PrecompiledPreambleBytes.second);
                          
  // Copy PredefinedBuffer into the Preprocessor.
  PP.setPredefines(Predefines.str());
  
  // Initialize the header search object.
  ApplyHeaderSearchOptions(PP.getHeaderSearchInfo(), HSOpts,
                           PP.getLangOpts());
}
