// This file is a part of Julia. License is MIT: https://julialang.org/license

// GenHeader: a clang-tidy check that emits, for a single translation unit, a
// "canonical" header summarizing the externally visible declarations textually
// present in the main source file.
//
// It is implemented as a clang-tidy check (rather than a bare clang plugin) so
// that it can be driven through clang-tidy's `-p <build>` option, which reads
// the per-file compiler flags and include paths directly from the project's
// `compile_commands.json`. This avoids duplicating the build's flag logic in
// the Makefile -- each file is parsed exactly as it is compiled.
//
// For each input file (e.g. `foo.c`) it produces a header containing, in order:
//   1. the type declarations referenced by the emitted prototypes/globals,
//      collected at the top: named struct/union forward declarations, typedefs
//      (e.g. `typedef struct A_ A;`) and full enum definitions;
//   2. all file-scope global variables without an initializer, as `extern`;
//   3. all non-static function prototypes (bodies removed), covering both
//      external declarations and definitions found in the file.
//
// Static (internal-linkage) functions and variables are ignored, as are
// declarations coming from included headers (only the main file is scanned).
//
// The intent is that a master header which `#include`s every generated header
// can be compiled to verify that all prototypes are mutually compatible: a
// function or global declared with conflicting signatures in two translation
// units will produce a redeclaration error.
//
// Usage (see src/Makefile `gen-header-%` rules):
//   clang-tidy foo.c -p <builddir> --quiet \
//       -load libGenHeaderPlugin.so \
//       --checks='-*,julia-gen-header' \
//       --config="{CheckOptions: {julia-gen-header.OutputDir: <dir>}}"

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/Type.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "llvm/ADT/SmallPtrSet.h"
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

// Collects, in dependency order, the typedefs and tag types (struct/union/enum)
// transitively referenced by the declarations we emit, so the generated header
// is self-contained.
class TypeCollector {
public:
    explicit TypeCollector(ASTContext &Ctx) : Ctx(Ctx) {}

    // Ordered, deduplicated outputs.
    std::vector<const TypedefNameDecl *> Typedefs;
    std::vector<const TagDecl *> Tags;

    void addTypedef(const TypedefNameDecl *TD) {
        const Decl *Key = TD->getCanonicalDecl();
        if (!VisitedTypedefs.insert(Key).second)
            return;
        // Emit dependencies of the underlying type before the typedef itself.
        collect(TD->getUnderlyingType());
        Typedefs.push_back(TD);
    }

    void addTag(const TagDecl *TD) {
        TD = TD->getCanonicalDecl();
        // Anonymous tags cannot be referred to by name; they are handled inline
        // by whatever typedef wraps them, but we still pull in the field types.
        if (!TD->getIdentifier()) {
            if (VisitedTags.insert(TD).second)
                collectMembers(TD);
            return;
        }
        if (!VisitedTags.insert(TD).second)
            return;
        Tags.push_back(TD);
    }

    void collect(QualType QT) {
        if (QT.isNull())
            return;
        const Type *T = QT.getTypePtr();
        switch (T->getTypeClass()) {
        case Type::Builtin:
            break;
        case Type::Pointer:
            collect(cast<PointerType>(T)->getPointeeType());
            break;
        case Type::BlockPointer:
            collect(cast<BlockPointerType>(T)->getPointeeType());
            break;
        case Type::LValueReference:
        case Type::RValueReference:
            collect(cast<ReferenceType>(T)->getPointeeType());
            break;
        case Type::MemberPointer: {
            const auto *MP = cast<MemberPointerType>(T);
            collect(MP->getPointeeType());
            if (const Type *Cls = MP->getClass())
                collect(QualType(Cls, 0));
            break;
        }
        case Type::ConstantArray:
        case Type::IncompleteArray:
        case Type::VariableArray:
        case Type::DependentSizedArray:
            collect(cast<ArrayType>(T)->getElementType());
            break;
        case Type::Vector:
        case Type::ExtVector:
            collect(cast<VectorType>(T)->getElementType());
            break;
        case Type::Complex:
            collect(cast<ComplexType>(T)->getElementType());
            break;
        case Type::Atomic:
            collect(cast<AtomicType>(T)->getValueType());
            break;
        case Type::FunctionProto: {
            const auto *FP = cast<FunctionProtoType>(T);
            collect(FP->getReturnType());
            for (QualType P : FP->getParamTypes())
                collect(P);
            break;
        }
        case Type::FunctionNoProto:
            collect(cast<FunctionNoProtoType>(T)->getReturnType());
            break;
        case Type::Paren:
            collect(cast<ParenType>(T)->getInnerType());
            break;
        case Type::Decayed:
            collect(cast<DecayedType>(T)->getDecayedType());
            break;
        case Type::Adjusted:
            collect(cast<AdjustedType>(T)->getAdjustedType());
            break;
        case Type::Attributed:
            collect(cast<AttributedType>(T)->getModifiedType());
            break;
        case Type::Elaborated:
            collect(cast<ElaboratedType>(T)->getNamedType());
            break;
        case Type::Typedef:
            addTypedef(cast<TypedefType>(T)->getDecl());
            break;
        case Type::Record:
            addTag(cast<RecordType>(T)->getDecl());
            break;
        case Type::Enum:
            addTag(cast<EnumType>(T)->getDecl());
            break;
        default:
            // Any other sugar (TypeOf, Decltype, Using, MacroQualified, ...):
            // peel one layer toward the canonical type and try again.
            if (!QT.isCanonical()) {
                QualType Desugared = QT.getSingleStepDesugaredType(Ctx);
                if (Desugared.getTypePtr() != T)
                    collect(Desugared);
                else
                    collect(QT.getCanonicalType());
            }
            break;
        }
    }

private:
    // For an anonymous record/enum we print the full definition, so make sure
    // the types of its members are available too.
    void collectMembers(const TagDecl *TD) {
        if (const auto *RD = dyn_cast<RecordDecl>(TD)) {
            if (RD->isThisDeclarationADefinition())
                for (const FieldDecl *FD : RD->fields())
                    collect(FD->getType());
        }
        else if (const auto *ED = dyn_cast<EnumDecl>(TD)) {
            collect(ED->getIntegerType());
        }
    }

    ASTContext &Ctx;
    llvm::SmallPtrSet<const Decl *, 64> VisitedTypedefs;
    llvm::SmallPtrSet<const TagDecl *, 64> VisitedTags;
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

        TypeCollector Collector(Ctx);
        llvm::SmallVector<const VarDecl *, 32> Vars;
        llvm::SmallVector<const FunctionDecl *, 128> Funcs;

        for (const Decl *D : TU->decls())
            collectDecl(D, SM, MainID, Collector, Vars, Funcs);

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

        emit(OS, Ctx, Collector, Vars, Funcs, InName);
    }

private:
    // Process one top-level declaration, descending into `extern "C" { ... }`
    // (and similar) blocks, whose members are nested inside a LinkageSpecDecl
    // rather than appearing directly among the translation unit's declarations.
    void collectDecl(const Decl *D, SourceManager &SM, FileID MainID,
                     TypeCollector &Collector,
                     llvm::SmallVectorImpl<const VarDecl *> &Vars,
                     llvm::SmallVectorImpl<const FunctionDecl *> &Funcs) {
        if (D->isImplicit())
            return;
        if (const auto *LSD = dyn_cast<LinkageSpecDecl>(D)) {
            for (const Decl *Inner : LSD->decls())
                collectDecl(Inner, SM, MainID, Collector, Vars, Funcs);
            return;
        }
        // Only consider declarations textually written in the main file.
        SourceLocation Loc = SM.getExpansionLoc(D->getLocation());
        if (Loc.isInvalid() || SM.getFileID(Loc) != MainID)
            return;

        if (const auto *TD = dyn_cast<TypedefNameDecl>(D)) {
            Collector.addTypedef(TD);
        }
        else if (const auto *Tag = dyn_cast<TagDecl>(D)) {
            if (Tag->isThisDeclarationADefinition() && Tag->getIdentifier())
                Collector.addTag(Tag);
        }
        else if (const auto *VD = dyn_cast<VarDecl>(D)) {
            if (!shouldEmit(VD))
                return;
            Collector.collect(VD->getType());
            Vars.push_back(VD);
        }
        else if (const auto *FD = dyn_cast<FunctionDecl>(D)) {
            // Skip C++ methods/templates; we only handle free functions.
            if (isa<CXXMethodDecl>(FD) || FD->getDescribedFunctionTemplate())
                return;
            if (!shouldEmit(FD))
                return;
            Collector.collect(FD->getType());
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

    static bool isAnonymousTagUnderlying(QualType QT) {
        const TagDecl *TD = QT.getCanonicalType()->getAsTagDecl();
        return TD && !TD->getIdentifier();
    }

    // Sanitize a name into an include-guard-safe identifier.
    static std::string guardName(StringRef Kind, StringRef Name) {
        std::string G = "JL_GENH_";
        G += Kind;
        G += "_";
        for (char C : Name)
            G += (std::isalnum((unsigned char)C) ? C : '_');
        return G;
    }

    void emit(llvm::raw_ostream &OS, ASTContext &Ctx, TypeCollector &Collector,
              ArrayRef<const VarDecl *> Vars,
              ArrayRef<const FunctionDecl *> Funcs, StringRef InName) {
        PrintingPolicy Terse = Ctx.getPrintingPolicy();
        Terse.TerseOutput = true;          // suppress function/tag bodies
        Terse.PolishForDeclaration = true; // drop inline definitions, etc.
        Terse.SuppressInitializers = true;
        Terse.IncludeTagDefinition = false;
        Terse.AnonymousTagLocations = false;

        PrintingPolicy Full = Terse;
        Full.TerseOutput = false;
        Full.IncludeTagDefinition = true;

        OS << "// Auto-generated by the genheader clang-tidy check. Do not edit.\n";
        OS << "// Source: " << InName << "\n\n";

        const bool isC = llvm::sys::path::extension(InName) == ".c";
        if (isC)
            OS << "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n";

        // 1. Types (forward declarations, typedefs, enum definitions).
        OS << "// --- types ---\n";
        for (const TagDecl *TD : Collector.Tags) {
            if (const auto *ED = dyn_cast<EnumDecl>(TD)) {
                // Enums cannot be portably forward-declared; emit the full
                // definition once, guarded against repeated inclusion.
                std::string G = guardName("enum", ED->getName());
                OS << "#ifndef " << G << "\n#define " << G << "\n";
                ED->print(OS, Full);
                OS << ";\n#endif\n";
            }
            else {
                // struct/union/class forward declaration (repeatable).
                OS << TD->getKindName() << " " << TD->getName() << ";\n";
            }
        }
        for (const TypedefNameDecl *TD : Collector.Typedefs) {
            const bool anon = isAnonymousTagUnderlying(TD->getUnderlyingType());
            std::string G = guardName("typedef", TD->getName());
            OS << "#ifndef " << G << "\n#define " << G << "\n";
            TD->print(OS, anon ? Full : Terse);
            OS << ";\n#endif\n";
        }

        // 2. Globals.
        OS << "\n// --- globals ---\n";
        for (const VarDecl *VD : Vars) {
            OS << "extern ";
            VD->getType().print(OS, Terse, VD->getName());
            OS << ";\n";
        }

        // 3. Prototypes.
        OS << "\n// --- prototypes ---\n";
        for (const FunctionDecl *FD : Funcs) {
            FD->print(OS, Terse);
            OS << ";\n";
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
