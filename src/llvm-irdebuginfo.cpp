// This file is a part of Julia. License is MIT: https://julialang.org/license

#define DEBUG_TYPE "ir_debug_info"
#undef DEBUG

#include <llvm/Pass.h>
#include <llvm/CodeGen/MachineFunctionPass.h>
#include <llvm/CodeGen/MIRPrinter.h>

#include "support/dtypes.h" // for JL_DLLEXPORT
#include "llvm-version.h"
#include "julia_assert.h"

using namespace llvm;

void jl_markup_llvm_ir(llvm::raw_string_ostream &stream, Module &M, StringRef filename, std::vector<unsigned> &LineMap);


struct IRDebugInfoPass : public ModulePass {
    static char ID;
    StringRef OutputFilename = "irdebuginfo.ll";
    std::string ir;
    std::vector<unsigned> LineMap;

    public:
    IRDebugInfoPass() : ModulePass(ID)
    {
    }

    protected:
    void getAnalysisUsage(AnalysisUsage &AU) const override
    {
        ModulePass::getAnalysisUsage(AU);
        AU.setPreservesCFG();
    }

    private:
    bool runOnModule(Module &M) override;
};

bool IRDebugInfoPass::runOnModule(Module &M)
{
    llvm::raw_string_ostream stream(ir);
    jl_markup_llvm_ir(stream, M, OutputFilename, LineMap);
    return false;
}

char IRDebugInfoPass::ID = 0;

static RegisterPass<IRDebugInfoPass> X("IRDebugInfoPass", "IRDebugInfo Pass",
                                     false /* Only looks at CFG */,
                                     false /* Analysis Pass */);

JL_DLLEXPORT Pass *createIRDebugInfoPass(StringRef OutputFilename)
{
    auto pass = new IRDebugInfoPass();
    pass->OutputFilename = OutputFilename;
    return pass;
}

JL_DLLEXPORT std::string &getIRDebugPassOutput(Pass *P)
{
    return ((IRDebugInfoPass*)P)->ir;
}

JL_DLLEXPORT std::vector<unsigned> &getIRDebugPassLineMap(Pass *P)
{
    return ((IRDebugInfoPass*)P)->LineMap;
}



namespace llvm {
void initializeMIRDebugInfoPassPass(PassRegistry&);
}

struct MIRDebugInfoPass : public MachineFunctionPass {
    static char ID;
    StringRef OutputFilename = "irdebuginfo.mir";
    std::string mir;

    public:
    MIRDebugInfoPass() : MachineFunctionPass(ID)
    {
        initializeMIRDebugInfoPassPass(*PassRegistry::getPassRegistry());
    }

    protected:
    void getAnalysisUsage(AnalysisUsage &AU) const override
    {
        MachineFunctionPass::getAnalysisUsage(AU);
        AU.setPreservesCFG();
    }

    private:
    bool runOnMachineFunction(MachineFunction &F) override;
};

bool MIRDebugInfoPass::runOnMachineFunction(MachineFunction &F)
{
    llvm::raw_string_ostream stream(mir);
    printMIR(stream, *F.getFunction().getParent());
    printMIR(stream, F);
    return false;
}

char MIRDebugInfoPass::ID = 0;

INITIALIZE_PASS(MIRDebugInfoPass, "MIRDebugInfoPass", "MIRDebugInfo Pass",
                false /* Only looks at CFG */,
                false /* Analysis Pass */);

JL_DLLEXPORT Pass *createMIRDebugInfoPass(StringRef OutputFilename)
{
    auto pass = new MIRDebugInfoPass();
    pass->OutputFilename = OutputFilename;
    return pass;
}

JL_DLLEXPORT std::string &getMIRDebugPassOutput(Pass *P)
{
    return ((MIRDebugInfoPass*)P)->mir;
}
