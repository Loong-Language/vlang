//===--- HeaderSearch.cpp - Resolve Header File Locations ---===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file implements the DirectoryLookup and HeaderSearch interfaces.
//
//===----------------------------------------------------------------------===//

#include "vlang/Lex/HeaderSearch.h"
#include "vlang/Diag/Diagnostic.h"
#include "vlang/Basic/FileManager.h"
#include "vlang/Basic/IdentifierTable.h"
#include "vlang/Lex/HeaderMap.h"
#include "vlang/Lex/HeaderSearchOptions.h"
#include "vlang/Lex/Lexer.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Capacity.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include <cstdio>
#if defined(LLVM_ON_UNIX)
#include <limits.h>
#endif
using namespace vlang;

const IdentifierInfo *
HeaderFileInfo::getControllingMacro(ExternalIdentifierLookup *External) {
  if (ControllingMacro)
    return ControllingMacro;

  if (!ControllingMacroID || !External)
    return 0;

  ControllingMacro = External->GetIdentifier(ControllingMacroID);
  return ControllingMacro;
}

ExternalHeaderFileInfoSource::~ExternalHeaderFileInfoSource() {}

HeaderSearch::HeaderSearch(IntrusiveRefCntPtr<HeaderSearchOptions> HSOpts,
                           FileManager &FM, DiagnosticsEngine &Diags,
                           const LangOptions &LangOpts)
  : HSOpts(HSOpts), FileMgr(FM)
{
  AngledDirIdx = 0;
  SystemDirIdx = 0;
  NoCurDirSearch = false;

  ExternalLookup = 0;
  ExternalSource = 0;
  NumIncluded = 0;
  NumMultiIncludeFileOptzn = 0;
}

HeaderSearch::~HeaderSearch() {
  // Delete headermaps.
  for (unsigned i = 0, e = HeaderMaps.size(); i != e; ++i)
    delete HeaderMaps[i].second;
}

void HeaderSearch::PrintStats() {
  fprintf(stderr, "\n*** HeaderSearch Stats:\n");
  fprintf(stderr, "%d files tracked.\n", (int)FileInfo.size());
  unsigned NumOnceOnlyFiles = 0, MaxNumIncludes = 0, NumSingleIncludedFiles = 0;
  for (unsigned i = 0, e = FileInfo.size(); i != e; ++i) {
    if (MaxNumIncludes < FileInfo[i].NumIncludes)
      MaxNumIncludes = FileInfo[i].NumIncludes;
    NumSingleIncludedFiles += FileInfo[i].NumIncludes == 1;
  }
  fprintf(stderr, "  %d included exactly once.\n", NumSingleIncludedFiles);
  fprintf(stderr, "  %d max times a file is included.\n", MaxNumIncludes);

  fprintf(stderr, "  %d #include/#include_next/#import.\n", NumIncluded);
  fprintf(stderr, "    %d #includes skipped due to"
          " the multi-include optimization.\n", NumMultiIncludeFileOptzn);

}

/// CreateHeaderMap - This method returns a HeaderMap for the specified
/// FileEntry, uniquing them through the 'HeaderMaps' datastructure.
const HeaderMap *HeaderSearch::CreateHeaderMap(const FileEntry *FE) {
  // We expect the number of headermaps to be small, and almost always empty.
  // If it ever grows, use of a linear search should be re-evaluated.
  if (!HeaderMaps.empty()) {
    for (unsigned i = 0, e = HeaderMaps.size(); i != e; ++i)
      // Pointer equality comparison of FileEntries works because they are
      // already uniqued by inode.
      if (HeaderMaps[i].first == FE)
        return HeaderMaps[i].second;
  }

  if (const HeaderMap *HM = HeaderMap::Create(FE, FileMgr)) {
    HeaderMaps.push_back(std::make_pair(FE, HM));
    return HM;
  }

  return 0;
}

//===----------------------------------------------------------------------===//
// File lookup within a DirectoryLookup scope
//===----------------------------------------------------------------------===//

/// getName - Return the directory or filename corresponding to this lookup
/// object.
const char *DirectoryLookup::getName() const {
  if (isNormalDir())
    return getDir()->getName();
  assert(isHeaderMap() && "Unknown DirectoryLookup");
  return getHeaderMap()->getFileName();
}


/// LookupFile - Lookup the specified file in this search path, returning it
/// if it exists or returning null if not.
const FileEntry *DirectoryLookup::LookupFile(
    StringRef Filename,
    HeaderSearch &HS,
    SmallVectorImpl<char> *SearchPath,
    SmallVectorImpl<char> *RelativePath) const {

  SmallString<1024> TmpDir;
  if (isNormalDir()) {
    // Concatenate the requested file onto the directory.
    TmpDir = getDir()->getName();
    llvm::sys::path::append(TmpDir, Filename);
    if (SearchPath != NULL) {
      StringRef SearchPathRef(getDir()->getName());
      SearchPath->clear();
      SearchPath->append(SearchPathRef.begin(), SearchPathRef.end());
    }
    if (RelativePath != NULL) {
      RelativePath->clear();
      RelativePath->append(Filename.begin(), Filename.end());
    }
    
    return HS.getFileMgr().getFile(TmpDir.str(), /*openFile=*/true);
  }

  assert(isHeaderMap() && "Unknown directory lookup");
  const FileEntry * const Result = getHeaderMap()->LookupFile(
      Filename, HS.getFileMgr());
  if (Result) {
    if (SearchPath != NULL) {
      StringRef SearchPathRef(getName());
      SearchPath->clear();
      SearchPath->append(SearchPathRef.begin(), SearchPathRef.end());
    }
    if (RelativePath != NULL) {
      RelativePath->clear();
      RelativePath->append(Filename.begin(), Filename.end());
    }
  }
  return Result;
}

//===----------------------------------------------------------------------===//
// Header File Location.
//===----------------------------------------------------------------------===//


/// LookupFile - Given a "foo" or \<foo> reference, look up the indicated file,
/// return null on failure.  isAngled indicates whether the file reference is
/// for system \#include's or not (i.e. using <> instead of "").  CurFileEnt, if
/// non-null, indicates where the \#including file is, in case a relative search
/// is needed.
const FileEntry *HeaderSearch::LookupFile(
    StringRef Filename,
    bool isAngled,
    const DirectoryLookup *FromDir,
    const DirectoryLookup *&CurDir,
    const FileEntry *CurFileEnt,
    SmallVectorImpl<char> *SearchPath,
    SmallVectorImpl<char> *RelativePath,
    bool SkipCache)
{
    
  // If 'Filename' is absolute, check to see if it exists and no searching.
  if (llvm::sys::path::is_absolute(Filename)) {
    CurDir = 0;

    // If this was an #include_next "/absolute/file", fail.
    if (FromDir) return 0;

    if (SearchPath != NULL)
      SearchPath->clear();
    if (RelativePath != NULL) {
      RelativePath->clear();
      RelativePath->append(Filename.begin(), Filename.end());
    }
    // Otherwise, just return the file.
    return FileMgr.getFile(Filename, /*openFile=*/true);
  }

  // Unless disabled, check to see if the file is in the `includer's
  // directory.  This has to be based on CurFileEnt, not CurDir, because
  // CurFileEnt could be a `include of a subdirectory (`include "foo/bar.h") and
  // a subsequent include of "baz.h" should resolve to "whatever/foo/baz.h".
  // This search is not done for <> headers.
  if (CurFileEnt && !isAngled && !NoCurDirSearch) {
    SmallString<1024> TmpDir;
    // Concatenate the requested file onto the directory.
    // FIXME: Portability.  Filename concatenation should be in sys::Path.
    TmpDir += CurFileEnt->getDir()->getName();
    TmpDir.push_back('/');
    TmpDir.append(Filename.begin(), Filename.end());
    if (const FileEntry *FE = FileMgr.getFile(TmpDir.str(),/*openFile=*/true)) {
      // Leave CurDir unset.
      // This file is a system header or C++ unfriendly if the old file is.
      //
      // Note that we only use one of FromHFI/ToHFI at once, due to potential
      // reallocation of the underlying vector potentially making the first
      // reference binding dangling.
      HeaderFileInfo &FromHFI = getFileInfo(CurFileEnt);
      unsigned DirInfo = FromHFI.DirInfo;
      bool IndexHeaderMapHeader = FromHFI.IndexHeaderMapHeader;

      HeaderFileInfo &ToHFI = getFileInfo(FE);
      ToHFI.DirInfo = DirInfo;
      ToHFI.IndexHeaderMapHeader = IndexHeaderMapHeader;

      if (SearchPath != NULL) {
        StringRef SearchPathRef(CurFileEnt->getDir()->getName());
        SearchPath->clear();
        SearchPath->append(SearchPathRef.begin(), SearchPathRef.end());
      }
      if (RelativePath != NULL) {
        RelativePath->clear();
        RelativePath->append(Filename.begin(), Filename.end());
      }
      return FE;
    }
  }

  CurDir = 0;

  // If this is a system `include, ignore the user `include locs.
  unsigned i = isAngled ? AngledDirIdx : 0;

  // If this is a #include_next request, start searching after the directory the
  // file was found in.
  if (FromDir)
    i = FromDir-&SearchDirs[0];

  // Cache all of the lookups performed by this method.  Many headers are
  // multiply included, and the "pragma once" optimization prevents them from
  // being relex/pp'd, but they would still have to search through a
  // (potentially huge) series of SearchDirs to find it.
  std::pair<unsigned, unsigned> &CacheLookup =
    LookupFileCache.GetOrCreateValue(Filename).getValue();

  // If the entry has been previously looked up, the first value will be
  // non-zero.  If the value is equal to i (the start point of our search), then
  // this is a matching hit.
  if (!SkipCache && CacheLookup.first == i+1) {
    // Skip querying potentially lots of directories for this lookup.
    i = CacheLookup.second;
  } else {
    // Otherwise, this is the first query, or the previous query didn't match
    // our search start.  We will fill in our found location below, so prime the
    // start point value.
    CacheLookup.first = i+1;
  }

  // Check each directory in sequence to see if it contains this file.
  for (; i != SearchDirs.size(); ++i) {
    const FileEntry *FE =
      SearchDirs[i].LookupFile(Filename, *this, SearchPath, RelativePath);
    if (!FE) continue;

    CurDir = &SearchDirs[i];

    // This file is a system header or C++ unfriendly if the dir is.
    HeaderFileInfo &HFI = getFileInfo(FE);
    HFI.DirInfo = CurDir->getDirCharacteristic();

    // If the filename matches a known system header prefix, override
    // whether the file is a system header.
    for (unsigned j = SystemHeaderPrefixes.size(); j; --j) {
      if (Filename.startswith(SystemHeaderPrefixes[j-1].first)) {
        HFI.DirInfo = SystemHeaderPrefixes[j-1].second ? SrcMgr::C_System
                                                       : SrcMgr::C_User;
        break;
      }
    }

    // Remember this location for the next lookup we do.
    CacheLookup.second = i;
    return FE;
  }

  // If we are including a file with a quoted include "foo.h" from inside
  // a header in a framework that is currently being built, and we couldn't
  // resolve "foo.h" any other way, change the include to <Foo/foo.h>, where
  // "Foo" is the name of the framework in which the including header was found.
  if (CurFileEnt && !isAngled && Filename.find('/') == StringRef::npos) {
    HeaderFileInfo &IncludingHFI = getFileInfo(CurFileEnt);
    if (IncludingHFI.IndexHeaderMapHeader) {
      SmallString<128> ScratchFilename;
      ScratchFilename += '/';
      ScratchFilename += Filename;
      
      const FileEntry *Result = LookupFile(ScratchFilename, /*isAngled=*/true,
                                           FromDir, CurDir, CurFileEnt, 
                                           SearchPath, RelativePath );
      std::pair<unsigned, unsigned> &CacheLookup 
        = LookupFileCache.GetOrCreateValue(Filename).getValue();
      CacheLookup.second
        = LookupFileCache.GetOrCreateValue(ScratchFilename).getValue().second;
      return Result;
    }
  }

  // Otherwise, didn't find it. Remember we didn't find this.
  CacheLookup.second = SearchDirs.size();
  return 0;
}

/// \brief Helper static function to normalize a path for injection into
/// a synthetic header.
/*static*/ std::string
HeaderSearch::NormalizeDashIncludePath(StringRef File, FileManager &FileMgr) {
  // Implicit include paths should be resolved relative to the current
  // working directory first, and then use the regular header search
  // mechanism. The proper way to handle this is to have the
  // predefines buffer located at the current working directory, but
  // it has no file entry. For now, workaround this by using an
  // absolute path if we find the file here, and otherwise letting
  // header search handle it.
  SmallString<128> Path(File);
  llvm::sys::fs::make_absolute(Path);
  bool exists;
  if (llvm::sys::fs::exists(Path.str(), exists) || !exists)
    Path = File;
  else if (exists)
    FileMgr.getFile(File);

  return Lexer::Stringify(Path.str());
}

//===----------------------------------------------------------------------===//
// File Info Management.
//===----------------------------------------------------------------------===//

/// \brief Merge the header file info provided by \p OtherHFI into the current
/// header file info (\p HFI)
static void mergeHeaderFileInfo(HeaderFileInfo &HFI, 
                                const HeaderFileInfo &OtherHFI) {
  HFI.NumIncludes += OtherHFI.NumIncludes;
  
  if (!HFI.ControllingMacro && !HFI.ControllingMacroID) {
    HFI.ControllingMacro = OtherHFI.ControllingMacro;
    HFI.ControllingMacroID = OtherHFI.ControllingMacroID;
  }
  
  if (OtherHFI.External) {
    HFI.DirInfo = OtherHFI.DirInfo;
    HFI.External = OtherHFI.External;
    HFI.IndexHeaderMapHeader = OtherHFI.IndexHeaderMapHeader;
  }
  
  HFI.Resolved = true;
}
                                
/// getFileInfo - Return the HeaderFileInfo structure for the specified
/// FileEntry.
HeaderFileInfo &HeaderSearch::getFileInfo(const FileEntry *FE) {
  if (FE->getUID() >= FileInfo.size())
    FileInfo.resize(FE->getUID()+1);
  
  HeaderFileInfo &HFI = FileInfo[FE->getUID()];
  if (ExternalSource && !HFI.Resolved)
    mergeHeaderFileInfo(HFI, ExternalSource->GetHeaderFileInfo(FE));
  return HFI;
}

bool HeaderSearch::isFileMultipleIncludeGuarded(const FileEntry *File) {
  // Check if we've ever seen this file as a header.
  if (File->getUID() >= FileInfo.size())
    return false;

  // Resolve header file info from the external source, if needed.
  HeaderFileInfo &HFI = FileInfo[File->getUID()];
  if (ExternalSource && !HFI.Resolved)
    mergeHeaderFileInfo(HFI, ExternalSource->GetHeaderFileInfo(File));

  return HFI.ControllingMacro || HFI.ControllingMacroID;
}


bool HeaderSearch::ShouldEnterIncludeFile(const FileEntry *File){
  ++NumIncluded; // Count # of attempted #includes.

  // Get information about this file.
  HeaderFileInfo &FileInfo = getFileInfo(File);


  // Next, check to see if the file is wrapped with #ifndef guards.  If so, and
  // if the macro that guards it is defined, we know the #include has no effect.
  if (const IdentifierInfo *ControllingMacro
      = FileInfo.getControllingMacro(ExternalLookup))
    if (ControllingMacro->hasMacroDefinition()) {
      ++NumMultiIncludeFileOptzn;
      return false;
    }

  // Increment the number of times this file has been included.
  ++FileInfo.NumIncludes;

  return true;
}

size_t HeaderSearch::getTotalMemory() const {
  return SearchDirs.capacity()
    + llvm::capacity_in_bytes(FileInfo)
    + llvm::capacity_in_bytes(HeaderMaps)
    + LookupFileCache.getAllocator().getTotalMemory();
}

  