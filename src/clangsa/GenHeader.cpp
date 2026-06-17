// This file is a part of Julia. License is MIT: https://julialang.org/license

// GenHeader: a clang-tidy check that emits, for a single translation unit, a
// header listing the externally visible declarations textually present in the
// main source file -- its non-static function prototypes and globals.
//
// It is implemented as a clang-tidy check (rather than a bare clang plugin) so
// that it can be driven through clang-tidy's `-p <build>` option, which reads
// the per-file compiler flags and include paths directly from the project's
// `compile_commands.json`. This avoids duplicating the build's flag logic in
// the Makefile -- each file is parsed exactly as it is compiled.
//
// Types are printed in canonical form using the names from the real headers
// (e.g. `struct _jl_value_t *`, `pthread_mutex_t`, `llvm::Module *`). The
// generated header does NOT redeclare those types: the master header
// (`all_prototypes.h`) `#include`s every real source header first, so the
// authoritative definitions are already in scope. Compiling that master header
// then checks each generated prototype against the real declaration, and across
// translation units.
//
// The only types the generated header declares itself are those defined in the
// main source file (file-local types not present in any header). Those are
// emitted as unique, per-file opaque tags (`struct jl_genh_<file>_<name>;`) so a
// file-local type from one translation unit can never be conflated with a
// same-named type from another.
//
// Static (internal-linkage) functions and variables are ignored, as are
// declarations coming from included headers (only the main file is scanned).
//
// Usage (see src/Makefile `gen-header-%` rules):
//   clang-tidy foo.c -p <builddir> --quiet \
//       -load libGenHeaderPlugin.so \
//       --checks='-*,julia-gen-header' \
//       --config="{CheckOptions: {julia-gen-header.OutputDir: <dir>}}"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/Type.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#include <cctype>
#include <string>
#include <vector>

using namespace clang;
using namespace clang::tidy;
using namespace clang::ast_matchers;

namespace {

// Rewrites types defined in the main source file so they cannot be conflated
// with same-named types from other translation units when the generated headers
// are combined: a file-local struct/union becomes a unique, per-file opaque tag,
// and a file-local enum becomes its underlying integer type. Types coming from
// #included headers are left untouched -- the real headers, included first in
// the master header, already define them.
class LocalTypeRewriter {
public:
    LocalTypeRewriter(ASTContext &Ctx, FileID MainID, StringRef Stem)
        : Ctx(Ctx), SM(Ctx.getSourceManager()), MainID(MainID) {
        Prefix = "jl_genh_";
        for (char C : Stem)
            Prefix += std::isalnum((unsigned char)C) ? C : '_';
        Prefix += '_';
    }

    // Forward declarations for the synthesized opaque tags, in first-seen order.
    std::vector<std::string> ForwardDecls;

    // Return QT in canonical form with any file-local tags substituted.
    QualType rewrite(QualType QT) {
        if (QT.isNull())
            return QT;
        QT = QT.getCanonicalType();
        SplitQualType Split = QT.split();
        const Type *T = Split.Ty;
        QualType R(T, 0);
        switch (T->getTypeClass()) {
        case Type::Pointer:
            R = Ctx.getPointerType(rewrite(cast<PointerType>(T)->getPointeeType()));
            break;
        case Type::LValueReference:
            R = Ctx.getLValueReferenceType(
                rewrite(cast<ReferenceType>(T)->getPointeeType()));
            break;
        case Type::RValueReference:
            R = Ctx.getRValueReferenceType(
                rewrite(cast<ReferenceType>(T)->getPointeeType()));
            break;
        case Type::ConstantArray: {
            const auto *AT = cast<ConstantArrayType>(T);
            R = Ctx.getConstantArrayType(rewrite(AT->getElementType()),
                                         AT->getSize(), AT->getSizeExpr(),
                                         AT->getSizeModifier(),
                                         AT->getIndexTypeCVRQualifiers());
            break;
        }
        case Type::IncompleteArray: {
            const auto *AT = cast<IncompleteArrayType>(T);
            R = Ctx.getIncompleteArrayType(rewrite(AT->getElementType()),
                                           AT->getSizeModifier(),
                                           AT->getIndexTypeCVRQualifiers());
            break;
        }
        case Type::Atomic:
            R = Ctx.getAtomicType(rewrite(cast<AtomicType>(T)->getValueType()));
            break;
        case Type::FunctionProto: {
            const auto *FP = cast<FunctionProtoType>(T);
            llvm::SmallVector<QualType, 16> Params;
            for (QualType P : FP->getParamTypes())
                Params.push_back(rewrite(P));
            R = Ctx.getFunctionType(rewrite(FP->getReturnType()), Params,
                                    FP->getExtProtoInfo());
            break;
        }
        case Type::Record: {
            const RecordDecl *RD = cast<RecordType>(T)->getDecl();
            if (isLocal(RD))
                R = localRecord(RD);
            break;
        }
        case Type::Enum: {
            const EnumDecl *ED = cast<EnumType>(T)->getDecl();
            if (isLocal(ED))
                R = ED->getIntegerType().getCanonicalType();
            break;
        }
        default:
            // Header-defined types (named tags, anonymous typedefs, template
            // specializations, ...) keep their canonical spelling and are
            // resolved by the real headers.
            break;
        }
        return Ctx.getQualifiedType(R, Split.Quals);
    }

private:
    // A tag is file-local when its definition (or, lacking one, its declaration)
    // is written in the main source file rather than an included header.
    bool isLocal(const TagDecl *TD) const {
        const TagDecl *Def = TD->getDefinition();
        SourceLocation L = (Def ? Def : TD)->getLocation();
        return L.isValid() && SM.getFileID(SM.getExpansionLoc(L)) == MainID;
    }

    QualType localRecord(const RecordDecl *RD) {
        RD = cast<RecordDecl>(RD->getCanonicalDecl());
        auto It = Cache.find(RD);
        if (It != Cache.end())
            return It->second;
        std::string Base = RD->getNameAsString();
        if (Base.empty())
            if (const TypedefNameDecl *TD = RD->getTypedefNameForAnonDecl())
                Base = TD->getNameAsString();
        if (Base.empty())
            Base = "anon";
        std::string Name = Prefix + Base;
        RecordDecl *S = RecordDecl::Create(
            Ctx, RD->getTagKind(), Ctx.getTranslationUnitDecl(),
            SourceLocation(), SourceLocation(), &Ctx.Idents.get(Name));
        QualType QT = Ctx.getRecordType(S);
        Cache[RD] = QT;
        ForwardDecls.push_back(std::string(S->getKindName()) + " " + Name + ";");
        return QT;
    }

    ASTContext &Ctx;
    SourceManager &SM;
    FileID MainID;
    std::string Prefix;
    llvm::DenseMap<const RecordDecl *, QualType> Cache;
};

class GenHeaderCheck : public ClangTidyCheck {
public:
    GenHeaderCheck(StringRef Name, ClangTidyContext *Context)
        : ClangTidyCheck(Name, Context),
          OutputDir(Options.get("OutputDir", ".")) {}

    void storeOptions(ClangTidyOptions::OptionMap &Opts) override {
        Options.store(Opts, "OutputDir", OutputDir);
    }

    void registerMatchers(MatchFinder *Finder) override {
        Finder->addMatcher(translationUnitDecl().bind("tu"), this);
    }

    void check(const MatchFinder::MatchResult &Result) override {
        const auto *TU = Result.Nodes.getNodeAs<TranslationUnitDecl>("tu");
        if (!TU)
            return;
        ASTContext &Ctx = *Result.Context;
        SourceManager &SM = Ctx.getSourceManager();
        FileID MainID = SM.getMainFileID();

        llvm::SmallVector<const VarDecl *, 32> Vars;
        llvm::SmallVector<const FunctionDecl *, 128> Funcs;
        for (const Decl *D : TU->decls())
            collectDecl(D, SM, MainID, Vars, Funcs);

        // Determine the output path from the main file name and OutputDir.
        auto MainFile = SM.getFileEntryRefForID(MainID);
        StringRef InName = MainFile ? MainFile->getName() : StringRef("output");
        llvm::SmallString<256> OutPath(OutputDir);
        llvm::sys::fs::create_directories(OutputDir);
        llvm::sys::path::append(OutPath,
                                llvm::sys::path::stem(InName) + ".h");

        std::error_code EC;
        llvm::raw_fd_ostream OS(OutPath, EC, llvm::sys::fs::OF_Text);
        if (EC) {
            diag(SM.getLocForStartOfFile(MainID),
                 "genheader: cannot open output file '%0': %1")
                << OutPath.str() << EC.message();
            return;
        }

        emit(OS, Ctx, MainID, Vars, Funcs, InName);
    }

private:
    // Process one top-level declaration, descending into `extern "C" { ... }`
    // (and similar) blocks, whose members are nested inside a LinkageSpecDecl
    // rather than appearing directly among the translation unit's declarations.
    void collectDecl(const Decl *D, SourceManager &SM, FileID MainID,
                     llvm::SmallVectorImpl<const VarDecl *> &Vars,
                     llvm::SmallVectorImpl<const FunctionDecl *> &Funcs) {
        if (D->isImplicit())
            return;
        if (const auto *LSD = dyn_cast<LinkageSpecDecl>(D)) {
            for (const Decl *Inner : LSD->decls())
                collectDecl(Inner, SM, MainID, Vars, Funcs);
            return;
        }
        // Only consider declarations textually written in the main file.
        SourceLocation Loc = SM.getExpansionLoc(D->getLocation());
        if (Loc.isInvalid() || SM.getFileID(Loc) != MainID)
            return;

        if (const auto *VD = dyn_cast<VarDecl>(D)) {
            if (shouldEmit(VD))
                Vars.push_back(VD);
        }
        else if (const auto *FD = dyn_cast<FunctionDecl>(D)) {
            // Skip C++ methods/templates; we only handle free functions.
            if (isa<CXXMethodDecl>(FD) || FD->getDescribedFunctionTemplate())
                return;
            if (shouldEmit(FD))
                Funcs.push_back(FD);
        }
    }

    // External-linkage, non-static functions/globals only.
    static bool shouldEmit(const FunctionDecl *FD) {
        return FD->getFormalLinkage() == Linkage::External &&
               FD->getStorageClass() != SC_Static;
    }
    static bool shouldEmit(const VarDecl *VD) {
        if (!VD->hasGlobalStorage() || VD->isStaticDataMember())
            return false;
        if (VD->getStorageClass() == SC_Static)
            return false;
        return VD->getFormalLinkage() == Linkage::External;
    }

    void emit(llvm::raw_ostream &OS, ASTContext &Ctx, FileID MainID,
              ArrayRef<const VarDecl *> Vars,
              ArrayRef<const FunctionDecl *> Funcs, StringRef InName) {
        PrintingPolicy Policy = Ctx.getPrintingPolicy();
        Policy.TerseOutput = true;          // suppress function bodies
        Policy.PolishForDeclaration = true; // drop inline definitions, etc.
        Policy.SuppressInitializers = true;
        Policy.IncludeTagDefinition = false;
        Policy.AnonymousTagLocations = false;
        Policy.SuppressTagKeyword = false; // always print `struct X`/`enum X`

        // Rewrite every emitted type up front so all file-local opaque tags are
        // discovered before we print the forward declarations that precede them.
        LocalTypeRewriter RW(Ctx, MainID, llvm::sys::path::stem(InName));
        llvm::SmallVector<QualType, 32> VarTypes;
        llvm::SmallVector<QualType, 128> FuncTypes;
        for (const VarDecl *VD : Vars)
            VarTypes.push_back(RW.rewrite(VD->getType()));
        for (const FunctionDecl *FD : Funcs)
            FuncTypes.push_back(RW.rewrite(FD->getType()));

        OS << "// Auto-generated by the genheader clang-tidy check. Do not edit.\n";
        OS << "// Source: " << InName << "\n\n";

        const bool isC = llvm::sys::path::extension(InName) == ".c";
        if (isC)
            OS << "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n";

        // Forward declarations for file-local types (unique per file).
        OS << "// --- file-local types ---\n";
        for (const std::string &FD : RW.ForwardDecls)
            OS << FD << "\n";

        // In a C++ header (a .c header is wrapped in `extern "C"` wholesale
        // above), individually preserve the C language linkage of declarations
        // that were written `extern "C"`, so their names keep C linkage.
        auto emitDecl = [&](bool ExternC, auto Print) {
            if (ExternC)
                OS << "extern \"C\" { ";
            Print();
            OS << ";";
            if (ExternC)
                OS << " }";
            OS << "\n";
        };

        OS << "\n// --- globals ---\n";
        for (size_t I = 0; I < Vars.size(); ++I) {
            const VarDecl *VD = Vars[I];
            emitDecl(!isC && VD->isExternC(), [&] {
                OS << "extern ";
                VarTypes[I].print(OS, Policy, VD->getName());
            });
        }

        OS << "\n// --- prototypes ---\n";
        for (size_t I = 0; I < Funcs.size(); ++I) {
            const FunctionDecl *FD = Funcs[I];
            emitDecl(!isC && FD->isExternC(), [&] {
                FuncTypes[I].print(OS, Policy, FD->getName());
            });
        }

        if (isC)
            OS << "\n#ifdef __cplusplus\n}\n#endif\n";
    }

    std::string OutputDir;
};

class GenHeaderModule : public ClangTidyModule {
public:
    void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
        CheckFactories.registerCheck<GenHeaderCheck>("julia-gen-header");
    }
};

} // namespace

namespace clang {
namespace tidy {

// Register the GenHeaderModule using this statically initialized variable.
static ClangTidyModuleRegistry::Add<::GenHeaderModule>
    X("julia-genheader-module", "Adds the julia-gen-header check.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the GenHeaderModule.
volatile int GenHeaderModuleAnchorSource = 0;

} // namespace tidy
} // namespace clang
