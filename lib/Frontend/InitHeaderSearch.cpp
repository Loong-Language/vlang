//===--- InitHeaderSearch.cpp - Initialize header search paths ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the InitHeaderSearch class.
//
//===----------------------------------------------------------------------===//

#include "vlang/Frontend/Utils.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/LangOptions.h"
#include "vlang/Config/config.h" // C_INCLUDE_DIRS
#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Lex/HeaderSearchOptions.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

using namespace vlang;
using namespace vlang::frontend;

namespace {

/// InitHeaderSearch - This class makes it easier to set the search paths of
///  a HeaderSearch object. InitHeaderSearch stores several search path lists
///  internally, which can be sent to a HeaderSearch object in one swoop.
class InitHeaderSearch {
  std::vector<std::pair<IncludeDirGroup, DirectoryLookup> > IncludePath;
  typedef std::vector<std::pair<IncludeDirGroup,
                      DirectoryLookup> >::const_iterator path_iterator;
  std::vector<std::pair<std::string, bool> > SystemHeaderPrefixes;
  HeaderSearch &Headers;
  bool Verbose;
  std::string IncludeSysroot;
  bool HasSysroot;

public:

  InitHeaderSearch(HeaderSearch &HS, bool verbose, StringRef sysroot)
    : Headers(HS), Verbose(verbose), IncludeSysroot(sysroot),
      HasSysroot(!(sysroot.empty() || sysroot == "/")) {
  }

  /// AddPath - Add the specified path to the specified group list, prefixing
  /// the sysroot if used.
  void AddPath(const Twine &Path, IncludeDirGroup Group);

  /// AddUnmappedPath - Add the specified path to the specified group list,
  /// without performing any sysroot remapping.
  void AddUnmappedPath(const Twine &Path, IncludeDirGroup Group);

  /// Realize - Merges all search path lists into one list and send it to
  /// HeaderSearch.
  void Realize(const LangOptions &Lang);
};

}  // end anonymous namespace.

static bool CanPrefixSysroot(StringRef Path) {
#if defined(_WIN32)
  return !Path.empty() && llvm::sys::path::is_separator(Path[0]);
#else
  return llvm::sys::path::is_absolute(Path);
#endif
}

void InitHeaderSearch::AddPath(const Twine &Path, IncludeDirGroup Group) {
  // Add the path with sysroot prepended, if desired and this is a system header
  // group.
  if (HasSysroot) {
    SmallString<256> MappedPathStorage;
    StringRef MappedPathStr = Path.toStringRef(MappedPathStorage);
    if (CanPrefixSysroot(MappedPathStr)) {
      AddUnmappedPath(IncludeSysroot + Path, Group);
      return;
    }
  }

  AddUnmappedPath(Path, Group);
}

void InitHeaderSearch::AddUnmappedPath(const Twine &Path, IncludeDirGroup Group) {
  assert(!Path.isTriviallyEmpty() && "can't handle empty path here");

  FileManager &FM = Headers.getFileMgr();
  SmallString<256> MappedPathStorage;
  StringRef MappedPathStr = Path.toStringRef(MappedPathStorage);

  // Compute the DirectoryLookup type.
  SrcMgr::CharacteristicKind Type;
  if (Group == Quoted || Group == Angled || Group == IndexHeaderMap) {
    Type = SrcMgr::C_User;
  } else if (Group == ExternCSystem) {
    Type = SrcMgr::C_ExternCSystem;
  } else {
    Type = SrcMgr::C_System;
  }

  // If the directory exists, add it.
  if (const DirectoryEntry *DE = FM.getDirectory(MappedPathStr)) {
    IncludePath.push_back(
      std::make_pair(Group, DirectoryLookup(DE, Type, false)));
    return;
  }

  // Check to see if this is an apple-style headermap (which are not allowed to
  // be frameworks).
  if (const FileEntry *FE = FM.getFile(MappedPathStr)) {
      if (const HeaderMap *HM = Headers.CreateHeaderMap(FE)) {
        // It is a headermap, add it to the search path.
        IncludePath.push_back(
          std::make_pair(Group,
                         DirectoryLookup(HM, Type, Group == IndexHeaderMap)));
        return;
      }
  }

  if (Verbose)
    llvm::errs() << "ignoring nonexistent directory \""
                 << MappedPathStr << "\"\n";
}

/// RemoveDuplicates - If there are duplicate directory entries in the specified
/// search list, remove the later (dead) ones.  Returns the number of non-system
/// headers removed, which is used to update NumAngled.
static unsigned RemoveDuplicates(std::vector<DirectoryLookup> &SearchList,
                                 unsigned First, bool Verbose) {
  llvm::SmallPtrSet<const DirectoryEntry *, 8> SeenDirs;
  llvm::SmallPtrSet<const DirectoryEntry *, 8> SeenFrameworkDirs;
  llvm::SmallPtrSet<const HeaderMap *, 8> SeenHeaderMaps;
  unsigned NonSystemRemoved = 0;
  for (unsigned i = First; i != SearchList.size(); ++i) {
    unsigned DirToRemove = i;

    const DirectoryLookup &CurEntry = SearchList[i];

    if (CurEntry.isNormalDir()) {
      // If this isn't the first time we've seen this dir, remove it.
      if (SeenDirs.insert(CurEntry.getDir()))
        continue;
    } else {
      assert(CurEntry.isHeaderMap() && "Not a headermap or normal dir?");
      // If this isn't the first time we've seen this headermap, remove it.
      if (SeenHeaderMaps.insert(CurEntry.getHeaderMap()))
        continue;
    }

    // If we have a normal #include dir/framework/headermap that is shadowed
    // later in the chain by a system include location, we actually want to
    // ignore the user's request and drop the user dir... keeping the system
    // dir.  This is weird, but required to emulate GCC's search path correctly.
    //
    // Since dupes of system dirs are rare, just rescan to find the original
    // that we're nuking instead of using a DenseMap.
    if (CurEntry.getDirCharacteristic() != SrcMgr::C_User) {
      // Find the dir that this is the same of.
      unsigned FirstDir;
      for (FirstDir = 0; ; ++FirstDir) {
        assert(FirstDir != i && "Didn't find dupe?");

        const DirectoryLookup &SearchEntry = SearchList[FirstDir];

        // If these are different lookup types, then they can't be the dupe.
        if (SearchEntry.getLookupType() != CurEntry.getLookupType())
          continue;

        bool isSame;
        if (CurEntry.isNormalDir())
          isSame = SearchEntry.getDir() == CurEntry.getDir();
        else {
          assert(CurEntry.isHeaderMap() && "Not a headermap or normal dir?");
          isSame = SearchEntry.getHeaderMap() == CurEntry.getHeaderMap();
        }

        if (isSame)
          break;
      }

      // If the first dir in the search path is a non-system dir, zap it
      // instead of the system one.
      if (SearchList[FirstDir].getDirCharacteristic() == SrcMgr::C_User)
        DirToRemove = FirstDir;
    }

    if (Verbose) {
      llvm::errs() << "ignoring duplicate directory \""
                   << CurEntry.getName() << "\"\n";
      if (DirToRemove != i)
        llvm::errs() << "  as it is a non-system directory that duplicates "
                     << "a system directory\n";
    }
    if (DirToRemove != i)
      ++NonSystemRemoved;

    // This is reached if the current entry is a duplicate.  Remove the
    // DirToRemove (usually the current dir).
    SearchList.erase(SearchList.begin()+DirToRemove);
    --i;
  }
  return NonSystemRemoved;
}


void InitHeaderSearch::Realize(const LangOptions &Lang) {
  // Concatenate ANGLE+SYSTEM+AFTER chains together into SearchList.
  std::vector<DirectoryLookup> SearchList;
  SearchList.reserve(IncludePath.size());

  // Quoted arguments go first.
  for (path_iterator it = IncludePath.begin(), ie = IncludePath.end();
       it != ie; ++it) {
    if (it->first == Quoted)
      SearchList.push_back(it->second);
  }
  // Deduplicate and remember index.
  RemoveDuplicates(SearchList, 0, Verbose);
  unsigned NumQuoted = SearchList.size();

  for (path_iterator it = IncludePath.begin(), ie = IncludePath.end();
       it != ie; ++it) {
    if (it->first == Angled || it->first == IndexHeaderMap)
      SearchList.push_back(it->second);
  }

  RemoveDuplicates(SearchList, NumQuoted, Verbose);
  unsigned NumAngled = SearchList.size();

  for (path_iterator it = IncludePath.begin(), ie = IncludePath.end();
       it != ie; ++it) {
    if (it->first == System || it->first == ExternCSystem )
      SearchList.push_back(it->second);
  }

  for (path_iterator it = IncludePath.begin(), ie = IncludePath.end();
       it != ie; ++it) {
    if (it->first == After)
      SearchList.push_back(it->second);
  }

  // Remove duplicates across both the Angled and System directories.  GCC does
  // this and failing to remove duplicates across these two groups breaks
  // #include_next.
  unsigned NonSystemRemoved = RemoveDuplicates(SearchList, NumQuoted, Verbose);
  NumAngled -= NonSystemRemoved;

  bool DontSearchCurDir = false;  // TODO: set to true if -I- is set?
  Headers.SetSearchPaths(SearchList, NumQuoted, NumAngled, DontSearchCurDir);

  Headers.SetSystemHeaderPrefixes(SystemHeaderPrefixes);

  // If verbose, print the list of directories that will be searched.
  if (Verbose) {
    llvm::errs() << "#include \"...\" search starts here:\n";
    for (unsigned i = 0, e = SearchList.size(); i != e; ++i) {
      if (i == NumQuoted)
        llvm::errs() << "#include <...> search starts here:\n";
      const char *Name = SearchList[i].getName();
      const char *Suffix;
      if (SearchList[i].isNormalDir())
        Suffix = "";
      else {
        assert(SearchList[i].isHeaderMap() && "Unknown DirectoryLookup");
        Suffix = " (headermap)";
      }
      llvm::errs() << " " << Name << Suffix << "\n";
    }
    llvm::errs() << "End of search list.\n";
  }
}

void vlang::ApplyHeaderSearchOptions(HeaderSearch &HS,
                                     const HeaderSearchOptions &HSOpts,
                                     const LangOptions &Lang) {
  InitHeaderSearch Init(HS, HSOpts.Verbose, HSOpts.Sysroot);

  // Add the user defined entries.
  for (unsigned i = 0, e = HSOpts.UserEntries.size(); i != e; ++i) {
    const HeaderSearchOptions::Entry &E = HSOpts.UserEntries[i];
    if (E.IgnoreSysRoot) {
      Init.AddUnmappedPath(E.Path, E.Group);
    } else {
      Init.AddPath(E.Path, E.Group);
    }
  }

  Init.Realize(Lang);
}
