//===--- Targets.cpp - Implement -arch option and targets -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements construction of a TargetInfo object from a
// target triple.
//
//===----------------------------------------------------------------------===//

#include "vlang/Basic/TargetInfo.h"
#include "vlang/Diag/Diagnostic.h"
#include "vlang/Basic/LangOptions.h"
#include "vlang/Basic/MacroBuilder.h"
#include "vlang/Basic/TargetOptions.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/OwningPtr.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IR/Type.h"
#include "llvm/MC/MCSectionMachO.h"
#include "llvm/Support/ErrorHandling.h"
#include <algorithm>
using namespace vlang;

//===----------------------------------------------------------------------===//
// Defines specific to certain operating systems.
//===----------------------------------------------------------------------===//

namespace {
template<typename TgtInfo>
class OSTargetInfo : public TgtInfo {
protected:
  virtual void getOSDefines(const LangOptions &Opts, const llvm::Triple &Triple,
                            MacroBuilder &Builder) const=0;
public:
  OSTargetInfo(const std::string& triple) : TgtInfo(triple) {}
  virtual void getTargetDefines(const LangOptions &Opts,
                                MacroBuilder &Builder) const {
    TgtInfo::getTargetDefines(Opts, Builder);
    getOSDefines(Opts, TgtInfo::getTriple(), Builder);
  }

};
} // end anonymous namespace


//===----------------------------------------------------------------------===//
// Specific target implementations.
//===----------------------------------------------------------------------===//


//===----------------------------------------------------------------------===//
// Driver code
//===----------------------------------------------------------------------===//

static TargetInfo *AllocateTarget(const std::string &T) {
  llvm::Triple Triple(T);
  llvm::Triple::OSType os = Triple.getOS();

  switch (Triple.getArch()) {
  default:
    return NULL;
  }
}

/// CreateTargetInfo - Return the target info object for the specified target
/// triple.
TargetInfo *TargetInfo::CreateTargetInfo(DiagnosticsEngine &Diags,
                                         TargetOptions *Opts) {
  llvm::Triple Triple(Opts->Triple);

  // Construct the target
  OwningPtr<TargetInfo> Target(AllocateTarget(Triple.str()));
  if (!Target) {
    //Diags.Report(diag::err_target_unknown_triple) << Triple.str();
    //return 0;
  }
  Target->setTargetOpts(Opts);

  // Set the target CPU if specified.
  if (!Opts->CPU.empty() && !Target->setCPU(Opts->CPU)) {
    Diags.Report(diag::err_target_unknown_cpu) << Opts->CPU;
    return 0;
  }

  // Set the target ABI if specified.
  if (!Opts->ABI.empty() && !Target->setABI(Opts->ABI)) {
    Diags.Report(diag::err_target_unknown_abi) << Opts->ABI;
    return 0;
  }

  // Compute the default target features, we need the target to handle this
  // because features may have dependencies on one another.
  llvm::StringMap<bool> Features;
  Target->getDefaultFeatures(Features);

  // Apply the user specified deltas.
  // First the enables.
  for (std::vector<std::string>::const_iterator 
         it = Opts->FeaturesAsWritten.begin(),
         ie = Opts->FeaturesAsWritten.end();
       it != ie; ++it) {
    const char *Name = it->c_str();

    if (Name[0] != '+')
      continue;

    // Apply the feature via the target.
    if (!Target->setFeatureEnabled(Features, Name + 1, true)) {
      Diags.Report(diag::err_target_invalid_feature) << Name;
      return 0;
    }
  }

  // Then the disables.
  for (std::vector<std::string>::const_iterator 
         it = Opts->FeaturesAsWritten.begin(),
         ie = Opts->FeaturesAsWritten.end();
       it != ie; ++it) {
    const char *Name = it->c_str();

    if (Name[0] == '+')
      continue;

    // Apply the feature via the target.
    if (Name[0] != '-' ||
        !Target->setFeatureEnabled(Features, Name + 1, false)) {
      Diags.Report(diag::err_target_invalid_feature) << Name;
      return 0;
    }
  }

  // Add the features to the compile options.
  //
  // FIXME: If we are completely confident that we have the right set, we only
  // need to pass the minuses.
  Opts->Features.clear();
  for (llvm::StringMap<bool>::const_iterator it = Features.begin(),
         ie = Features.end(); it != ie; ++it)
    Opts->Features.push_back((it->second ? "+" : "-") + it->first().str());
  Target->HandleTargetFeatures(Opts->Features);

  return Target.take();
}
