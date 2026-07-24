// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_PASSES_H
#define JL_PASSES_H

#include "analyzer_annotations.h"
#include <llvm/IR/PassManager.h>
#include <llvm/Transforms/Scalar/LoopPassManager.h>

using namespace llvm;

// Function Passes
struct DemoteFloat16Pass : PassInfoMixin<DemoteFloat16Pass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct LateLowerGCPass : PassInfoMixin<LateLowerGCPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct AllocOptPass : PassInfoMixin<AllocOptPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};

struct PropagateJuliaAddrspacesPass : PassInfoMixin<PropagateJuliaAddrspacesPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
    static bool isRequired() { return true; }
};


struct GCInvariantVerifierPass : PassInfoMixin<GCInvariantVerifierPass> {
    bool Strong;
    GCInvariantVerifierPass(bool Strong = false) : Strong(Strong) {}

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct FinalLowerGCPass : PassInfoMixin<FinalLowerGCPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct ExpandAtomicModifyPass : PassInfoMixin<ExpandAtomicModifyPass> {
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM);
};


// Module Passes
struct CPUFeaturesPass : PassInfoMixin<CPUFeaturesPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct RemoveNIPass : PassInfoMixin<RemoveNIPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct MultiVersioningPass : PassInfoMixin<MultiVersioningPass> {
    bool external_use;
    MultiVersioningPass(bool external_use = false) : external_use(external_use) {}
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct RemoveJuliaAddrspacesPass : PassInfoMixin<RemoveJuliaAddrspacesPass> {
    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct RemoveAddrspacesPass : PassInfoMixin<RemoveAddrspacesPass> {
    std::function<unsigned(unsigned)> ASRemapper;
    RemoveAddrspacesPass();
    RemoveAddrspacesPass(std::function<unsigned(unsigned)> ASRemapper) : ASRemapper(std::move(ASRemapper)) {}
    ~RemoveAddrspacesPass() = default;

    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    static bool isRequired() { return true; }
};

struct LowerPTLSPass : PassInfoMixin<LowerPTLSPass> {
    bool imaging_mode;
    bool tls_getters;
    LowerPTLSPass(bool imaging_mode=false, bool tls_getters=false) : imaging_mode(imaging_mode), tls_getters(tls_getters) {}

    PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM);
    static bool isRequired() { return true; }
};

// Loop Passes
struct JuliaLICMPass : PassInfoMixin<JuliaLICMPass> {
    PreservedAnalyses run(Loop &L, LoopAnalysisManager &AM,
                          LoopStandardAnalysisResults &AR, LPMUpdater &U);
};

struct LowerSIMDLoopPass : PassInfoMixin<LowerSIMDLoopPass> {
    PreservedAnalyses run(Loop &L, LoopAnalysisManager &AM,
                          LoopStandardAnalysisResults &AR, LPMUpdater &U);
};

#define MODULE_MARKER_PASS(NAME) \
    struct NAME##MarkerPass : PassInfoMixin<NAME##MarkerPass> { \
        PreservedAnalyses run(Module &M, ModuleAnalysisManager &AM) { return PreservedAnalyses::all(); } \
        static bool isRequired() { return true; } \
    };

#define FUNCTION_MARKER_PASS(NAME) \
    struct NAME##MarkerPass : PassInfoMixin<NAME##MarkerPass> { \
        PreservedAnalyses run(Function &F, FunctionAnalysisManager &AM) { return PreservedAnalyses::all(); } \
        static bool isRequired() { return true; } \
    };

#define LOOP_MARKER_PASS(NAME) \
    struct NAME##MarkerPass : PassInfoMixin<NAME##MarkerPass> { \
        PreservedAnalyses run(Loop &L, LoopAnalysisManager &AM, \
                              LoopStandardAnalysisResults &AR, LPMUpdater &U) { \
            return PreservedAnalyses::all(); \
        } \
        static bool isRequired() { return true; } \
    };

// These are useful for debugging with --print-before/after
MODULE_MARKER_PASS(BeforeOptimization)
MODULE_MARKER_PASS(BeforeEarlySimplification)
MODULE_MARKER_PASS(AfterEarlySimplification)
MODULE_MARKER_PASS(BeforeEarlyOptimization)
MODULE_MARKER_PASS(AfterEarlyOptimization)
FUNCTION_MARKER_PASS(BeforeLoopOptimization)
LOOP_MARKER_PASS(BeforeLICM)
LOOP_MARKER_PASS(AfterLICM)
LOOP_MARKER_PASS(BeforeLoopSimplification)
LOOP_MARKER_PASS(AfterLoopSimplification)
FUNCTION_MARKER_PASS(AfterLoopOptimization)
FUNCTION_MARKER_PASS(BeforeScalarOptimization)
FUNCTION_MARKER_PASS(AfterScalarOptimization)
FUNCTION_MARKER_PASS(BeforeVectorization)
FUNCTION_MARKER_PASS(AfterVectorization)
MODULE_MARKER_PASS(BeforeIntrinsicLowering)
MODULE_MARKER_PASS(AfterIntrinsicLowering)
MODULE_MARKER_PASS(BeforeCleanup)
MODULE_MARKER_PASS(AfterCleanup)
MODULE_MARKER_PASS(AfterOptimization)

bool verifyLLVMIR(const Module &M);
bool verifyLLVMIR(const Function &F);
bool verifyLLVMIR(const Loop &L);

#endif
