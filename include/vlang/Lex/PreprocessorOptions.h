//===--- PreprocessorOptions.h ----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_VLANG_LEX_PREPROCESSOROPTIONS_H_
#define LLVM_VLANG_LEX_PREPROCESSOROPTIONS_H_

#include "vlang/Basic/SourceLocation.h"
#include "llvm/ADT/IntrusiveRefCntPtr.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include <cassert>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace llvm {
  class MemoryBuffer;
}

namespace vlang {

class Preprocessor;
class LangOptions;

/// PreprocessorOptions - This class is used for passing the various options
/// used in preprocessor initialization to InitializePreprocessor().
class PreprocessorOptions : public RefCountedBase<PreprocessorOptions> {
public:
  std::vector<std::pair<std::string, bool/*isUndef*/> > Macros;
  std::vector<std::string> Includes;
  std::vector<std::string> MacroIncludes;

  /// \brief Initialize the preprocessor with the compiler and target specific
  /// predefines.
  unsigned UsePredefines : 1;

  /// \brief Whether we should maintain a detailed record of all macro
  /// definitions and expansions.
  unsigned DetailedRecord : 1;

  /// \brief Headers that will be converted to chained PCHs in memory.
  std::vector<std::string> ChainedIncludes;

  /// \brief If non-zero, the implicit PCH include is actually a precompiled
  /// preamble that covers this number of bytes in the main source file.
  ///
  /// The boolean indicates whether the preamble ends at the start of a new
  /// line.
  std::pair<unsigned, bool> PrecompiledPreambleBytes;

  /// If given, a PTH cache file to use for speeding up header parsing.
  std::string TokenCache;

  /// \brief True if the SourceManager should report the original file name for
  /// contents of files that were remapped to other files. Defaults to true.
  bool RemappedFilesKeepOriginalName;

  /// \brief The set of file remappings, which take existing files on
  /// the system (the first part of each pair) and gives them the
  /// contents of other files on the system (the second part of each
  /// pair).
  std::vector<std::pair<std::string, std::string> >  RemappedFiles;

  /// \brief The set of file-to-buffer remappings, which take existing files
  /// on the system (the first part of each pair) and gives them the contents
  /// of the specified memory buffer (the second part of each pair).
  std::vector<std::pair<std::string, const llvm::MemoryBuffer *> > 
    RemappedFileBuffers;
  
  /// \brief Whether the compiler instance should retain (i.e., not free)
  /// the buffers associated with remapped files.
  ///
  /// This flag defaults to false; it can be set true only through direct
  /// manipulation of the compiler invocation object, in cases where the 
  /// compiler invocation and its buffers will be reused.
  bool RetainRemappedFileBuffers;
    
  /// \brief Records the set of modules
  class FailedModulesSet : public RefCountedBase<FailedModulesSet> {
    llvm::StringSet<> Failed;

  public:
    bool hasAlreadyFailed(StringRef module) {
      return Failed.count(module) > 0;
    }

    void addFailed(StringRef module) {
      Failed.insert(module);
    }
  };
  
  /// \brief The set of modules that failed to build.
  ///
  /// This pointer will be shared among all of the compiler instances created
  /// to (re)build modules, so that once a module fails to build anywhere,
  /// other instances will see that the module has failed and won't try to
  /// build it again.
  IntrusiveRefCntPtr<FailedModulesSet> FailedModules;

  typedef std::vector<std::pair<std::string, std::string> >::iterator
    remapped_file_iterator;
  typedef std::vector<std::pair<std::string, std::string> >::const_iterator
    const_remapped_file_iterator;
  remapped_file_iterator remapped_file_begin() { 
    return RemappedFiles.begin();
  }
  const_remapped_file_iterator remapped_file_begin() const {
    return RemappedFiles.begin();
  }
  remapped_file_iterator remapped_file_end() { 
    return RemappedFiles.end();
  }
  const_remapped_file_iterator remapped_file_end() const { 
    return RemappedFiles.end();
  }

  typedef std::vector<std::pair<std::string, const llvm::MemoryBuffer *> >::
                                  iterator remapped_file_buffer_iterator;
  typedef std::vector<std::pair<std::string, const llvm::MemoryBuffer *> >::
                            const_iterator const_remapped_file_buffer_iterator;
  remapped_file_buffer_iterator remapped_file_buffer_begin() {
    return RemappedFileBuffers.begin();
  }
  const_remapped_file_buffer_iterator remapped_file_buffer_begin() const {
    return RemappedFileBuffers.begin();
  }
  remapped_file_buffer_iterator remapped_file_buffer_end() {
    return RemappedFileBuffers.end();
  }
  const_remapped_file_buffer_iterator remapped_file_buffer_end() const {
    return RemappedFileBuffers.end();
  }
  
public:
  PreprocessorOptions() : UsePredefines(true), DetailedRecord(false),
                          PrecompiledPreambleBytes(0, true),
                          RemappedFilesKeepOriginalName(true),
                          RetainRemappedFileBuffers(false) { }

  void addMacroDef(StringRef Name) {
    Macros.push_back(std::make_pair(Name, false));
  }
  void addMacroUndef(StringRef Name) {
    Macros.push_back(std::make_pair(Name, true));
  }
  void addRemappedFile(StringRef From, StringRef To) {
    RemappedFiles.push_back(std::make_pair(From, To));
  }
  
  remapped_file_iterator eraseRemappedFile(remapped_file_iterator Remapped) {
    return RemappedFiles.erase(Remapped);
  }
  
  void addRemappedFile(StringRef From, const llvm::MemoryBuffer * To) {
    RemappedFileBuffers.push_back(std::make_pair(From, To));
  }
  
  remapped_file_buffer_iterator
  eraseRemappedFile(remapped_file_buffer_iterator Remapped) {
    return RemappedFileBuffers.erase(Remapped);
  }
  
  void clearRemappedFiles() {
    RemappedFiles.clear();
    RemappedFileBuffers.clear();
  }
  
  /// \brief Reset any options that are not considered when building a
  /// module.
  void resetNonModularOptions() {
    Includes.clear();
    MacroIncludes.clear();
    ChainedIncludes.clear();
    TokenCache.clear();
    RetainRemappedFileBuffers = true;
    PrecompiledPreambleBytes.first = 0;
    PrecompiledPreambleBytes.second = 0;
  }
};

} // end namespace vlang

#endif
