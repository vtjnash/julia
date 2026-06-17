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
// All types are expanded to their canonical form (typedefs are not emitted), so
// the only type declarations needed are the underlying tags. For each input file
// (e.g. `foo.c`) it produces a header containing, in order:
//   1. the tag types referenced by the emitted prototypes/globals, collected at
//      the top: named struct/union forward declarations and full enum
//      definitions. An anonymous tag named only through a typedef
//      (`typedef struct {} A;`) is given that name and bound with a typedef so
//      the canonical references to it resolve;
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
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/NestedNameSpecifier.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/Type.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Basic/SourceManager.h"
#include "clang-tidy/ClangTidyCheck.h"
#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "llvm/ADT/DenseSet.h"
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

// Collects, in dependency order, the tag types (struct/union/enum) transitively
// referenced by the declarations we emit, so the generated header is
// self-contained. Typedefs are not collected: every type is expanded to its
// canonical form, so only the underlying tags need to be declared.
class TypeCollector {
public:
    explicit TypeCollector(ASTContext &Ctx) : Ctx(Ctx) {}

    // Ordered, deduplicated outputs.
    std::vector<const TagDecl *> Tags;
    std::vector<const ClassTemplateDecl *> Templates;

    void addTag(const TagDecl *TD) {
        TD = TD->getCanonicalDecl();
        // A truly anonymous tag (no name and not named via a typedef) cannot be
        // forward-declared; pull in its field types but emit nothing for it.
        // An anonymous tag named through a typedef (`typedef struct {} A;`) is
        // forward-declarable using that typedef name, so fall through.
        if (!TD->getIdentifier() && !TD->getTypedefNameForAnonDecl()) {
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
        // Expand to the canonical type so typedefs and other sugar disappear;
        // we only need to declare the underlying tags.
        QT = QT.getCanonicalType();
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
            if (const CXXRecordDecl *Cls = MP->getMostRecentCXXRecordDecl())
                addTag(Cls);
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
        case Type::Record: {
            const auto *RD = cast<RecordType>(T)->getDecl();
            if (const auto *Spec =
                    dyn_cast<ClassTemplateSpecializationDecl>(RD)) {
                // A template specialization (e.g. `unique_ptr<Module>`) is
                // referenced by name; forward-declare its primary template and
                // collect the types used as template arguments.
                addTemplate(Spec->getSpecializedTemplate());
                for (const TemplateArgument &A :
                     Spec->getTemplateArgs().asArray())
                    collectTemplateArg(A);
            } else {
                addTag(RD);
            }
            break;
        }
        case Type::Enum:
            addTag(cast<EnumType>(T)->getDecl());
            break;
        default:
            // Canonical types carry no typedef/elaborated/other sugar, so any
            // remaining class references no tag we need to declare.
            break;
        }
    }

    void addTemplate(const ClassTemplateDecl *CTD) {
        if (!CTD)
            return;
        CTD = cast<ClassTemplateDecl>(CTD->getCanonicalDecl());
        if (VisitedTemplates.insert(CTD).second)
            Templates.push_back(CTD);
    }

    void collectTemplateArg(const TemplateArgument &A) {
        switch (A.getKind()) {
        case TemplateArgument::Type:
            collect(A.getAsType());
            break;
        case TemplateArgument::Pack:
            for (const TemplateArgument &E : A.pack_elements())
                collectTemplateArg(E);
            break;
        default:
            // Integral/expression/template arguments reference no tag we must
            // declare for the reference to compile.
            break;
        }
    }

private:
    // A truly anonymous tag is printed inline (with its body) wherever it is
    // used, so make sure the types of its members are declared too.
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
    llvm::DenseSet<const TagDecl *> VisitedTags;
    llvm::DenseSet<const ClassTemplateDecl *> VisitedTemplates;
};

// True when D is declared directly at file scope (the translation unit or an
// `extern "C"`/`extern "C++"` block), i.e. not inside a namespace or record.
static bool isFileScope(const Decl *D) {
    for (const DeclContext *DC = D->getDeclContext(); DC; DC = DC->getParent()) {
        if (DC->isTranslationUnit())
            return true;
        if (DC->isExternCContext() || isa<LinkageSpecDecl>(DC))
            continue;
        return false;
    }
    return true;
}

// True for a struct/union named only through a typedef whose canonical type we
// can print elaborated (`struct X`) so a plain forward declaration resolves it.
static bool isElaboratableAnonRecord(const RecordDecl *RD) {
    return !RD->getIdentifier() && RD->getTypedefNameForAnonDecl() &&
           isFileScope(RD);
}

// True if TD can be declared at file scope, i.e. it is nested only in namespaces
// (or the translation unit / an `extern "C"` block) and not inside a record. A
// tag nested in a record can only be named as `Outer::Inner`, which requires
// Outer to be complete and so cannot be used with a mere forward declaration.
static bool tagDeclarableAtFileScope(const TagDecl *TD) {
    for (const DeclContext *DC = TD->getDeclContext();
         DC && !DC->isTranslationUnit(); DC = DC->getParent()) {
        if (DC->isExternCContext() || isa<LinkageSpecDecl>(DC))
            continue;
        if (isa<NamespaceDecl>(DC))
            continue;
        return false;
    }
    return true;
}

// Build the nested-name-specifier for the named namespaces enclosing D (e.g.
// `std::`), skipping inline and anonymous namespaces. Sets Ok=false (and returns
// null) if D is nested in a record, where no namespace qualifier applies.
static NestedNameSpecifier *namespaceNNS(ASTContext &Ctx, const Decl *D,
                                         bool &Ok) {
    Ok = true;
    llvm::SmallVector<const NamespaceDecl *, 4> Names;
    for (const DeclContext *DC = D->getDeclContext();
         DC && !DC->isTranslationUnit(); DC = DC->getParent()) {
        if (DC->isExternCContext() || isa<LinkageSpecDecl>(DC))
            continue;
        const auto *ND = dyn_cast<NamespaceDecl>(DC);
        if (!ND) {
            Ok = false;
            return nullptr;
        }
        if (ND->isAnonymousNamespace() || ND->isInline())
            continue;
        Names.push_back(ND);
    }
    NestedNameSpecifier *NNS = nullptr;
    for (const NamespaceDecl *ND : llvm::reverse(Names))
        NNS = NestedNameSpecifier::Create(Ctx, NNS, ND);
    return NNS;
}

static TemplateArgument elaborateTemplateArg(ASTContext &Ctx,
                                             const TemplateArgument &A);

// Rebuild QT in canonical form, but print references to file-scope struct/union
// types named only through a typedef using their elaborated `struct X` spelling.
// The type printer would otherwise render such a type as the bare typedef name,
// which a plain forward declaration does not provide; `struct X` lets a
// `struct X;` forward declaration suffice (so no typedef need be emitted).
//
// Enums nested in a record (e.g. `Outer::Kind`) are replaced by their underlying
// integer type, since naming them would require Outer to be complete. Template
// specializations are rebuilt so this substitution also applies to their
// arguments (e.g. `std::initializer_list<Outer::Kind>`).
static QualType elaborateAnonTags(ASTContext &Ctx, QualType QT) {
    QT = QT.getCanonicalType();
    SplitQualType Split = QT.split();
    const Type *T = Split.Ty;
    QualType R(T, 0);
    switch (T->getTypeClass()) {
    case Type::Pointer:
        R = Ctx.getPointerType(
            elaborateAnonTags(Ctx, cast<PointerType>(T)->getPointeeType()));
        break;
    case Type::LValueReference:
        R = Ctx.getLValueReferenceType(
            elaborateAnonTags(Ctx, cast<ReferenceType>(T)->getPointeeType()));
        break;
    case Type::RValueReference:
        R = Ctx.getRValueReferenceType(
            elaborateAnonTags(Ctx, cast<ReferenceType>(T)->getPointeeType()));
        break;
    case Type::ConstantArray: {
        const auto *AT = cast<ConstantArrayType>(T);
        R = Ctx.getConstantArrayType(elaborateAnonTags(Ctx, AT->getElementType()),
                                     AT->getSize(), AT->getSizeExpr(),
                                     AT->getSizeModifier(),
                                     AT->getIndexTypeCVRQualifiers());
        break;
    }
    case Type::IncompleteArray: {
        const auto *AT = cast<IncompleteArrayType>(T);
        R = Ctx.getIncompleteArrayType(elaborateAnonTags(Ctx, AT->getElementType()),
                                       AT->getSizeModifier(),
                                       AT->getIndexTypeCVRQualifiers());
        break;
    }
    case Type::Atomic:
        R = Ctx.getAtomicType(
            elaborateAnonTags(Ctx, cast<AtomicType>(T)->getValueType()));
        break;
    case Type::FunctionProto: {
        const auto *FP = cast<FunctionProtoType>(T);
        llvm::SmallVector<QualType, 16> Params;
        for (QualType P : FP->getParamTypes())
            Params.push_back(elaborateAnonTags(Ctx, P));
        R = Ctx.getFunctionType(elaborateAnonTags(Ctx, FP->getReturnType()),
                                Params, FP->getExtProtoInfo());
        break;
    }
    case Type::Record: {
        const RecordDecl *RD = cast<RecordType>(T)->getDecl();
        if (const auto *CTSD = dyn_cast<ClassTemplateSpecializationDecl>(RD)) {
            // Rebuild the specialization so nested-enum substitution reaches its
            // template arguments (e.g. `initializer_list<Outer::Kind>`), keeping
            // the namespace qualifier so the reference resolves.
            bool Ok;
            NestedNameSpecifier *NNS =
                namespaceNNS(Ctx, CTSD->getSpecializedTemplate(), Ok);
            if (Ok) {
                llvm::SmallVector<TemplateArgument, 8> Args;
                for (const TemplateArgument &A : CTSD->getTemplateArgs().asArray())
                    Args.push_back(elaborateTemplateArg(Ctx, A));
                llvm::SmallVector<TemplateArgument, 8> Canon;
                for (const TemplateArgument &A : Args)
                    Canon.push_back(Ctx.getCanonicalTemplateArgument(A));
                QualType TST = Ctx.getTemplateSpecializationType(
                    TemplateName(CTSD->getSpecializedTemplate()), Args, Canon);
                R = NNS ? Ctx.getElaboratedType(ElaboratedTypeKeyword::None, NNS,
                                                TST)
                        : TST;
            }
        }
        else if (isElaboratableAnonRecord(RD)) {
            R = Ctx.getElaboratedType(
                TypeWithKeyword::getKeywordForTagTypeKind(RD->getTagKind()),
                nullptr, QualType(T, 0));
        }
        break;
    }
    case Type::Enum: {
        const EnumDecl *ED = cast<EnumType>(T)->getDecl();
        // A nested enum (`Outer::Kind`) cannot be named with Outer only
        // forward-declared; use its underlying integer type instead.
        if (!tagDeclarableAtFileScope(ED))
            R = ED->getIntegerType().getCanonicalType();
        break;
    }
    default:
        // Any other type: keep its canonical form (no anonymous tag to elaborate
        // that we can usefully forward-declare).
        break;
    }
    return Ctx.getQualifiedType(R, Split.Quals);
}

// Apply elaborateAnonTags to the type(s) inside a template argument.
static TemplateArgument elaborateTemplateArg(ASTContext &Ctx,
                                             const TemplateArgument &A) {
    switch (A.getKind()) {
    case TemplateArgument::Type:
        return TemplateArgument(elaborateAnonTags(Ctx, A.getAsType()));
    case TemplateArgument::Pack: {
        llvm::SmallVector<TemplateArgument, 8> Elts;
        for (const TemplateArgument &E : A.pack_elements())
            Elts.push_back(elaborateTemplateArg(Ctx, E));
        return TemplateArgument::CreatePackCopy(Ctx, Elts);
    }
    default:
        return A;
    }
}

// Emit the named namespaces enclosing D as `namespace a { namespace b {` into
// Open and the matching closers into Close (inline and anonymous namespaces are
// skipped). Returns false if D is nested in a context that cannot be reopened at
// file scope (e.g. a record), meaning it cannot be forward-declared here.
static bool namespaceWrappers(const Decl *D, std::string &Open,
                              std::string &Close) {
    llvm::SmallVector<StringRef, 4> Names;
    for (const DeclContext *DC = D->getDeclContext();
         DC && !DC->isTranslationUnit(); DC = DC->getParent()) {
        if (DC->isExternCContext() || isa<LinkageSpecDecl>(DC))
            continue;
        const auto *ND = dyn_cast<NamespaceDecl>(DC);
        if (!ND)
            return false;
        if (ND->isAnonymousNamespace() || ND->isInline())
            continue;
        Names.push_back(ND->getName());
    }
    for (StringRef N : llvm::reverse(Names)) {
        Open += "namespace ";
        Open += N;
        Open += " { ";
        Close += "} ";
    }
    return true;
}

static void printDummyTemplateParams(llvm::raw_ostream &OS,
                                     const TemplateParameterList *TPL,
                                     const PrintingPolicy &PP);

// Print a single template parameter, keeping only its kind (and `...` for a
// pack). No name or default argument is emitted: a default cannot be repeated
// across declarations, and references print all arguments (see
// SuppressDefaultTemplateArgs) so the arity already matches.
static void printDummyTemplateParam(llvm::raw_ostream &OS, const NamedDecl *P,
                                    const PrintingPolicy &PP) {
    if (const auto *TTP = dyn_cast<TemplateTypeParmDecl>(P)) {
        OS << "class";
        if (TTP->isParameterPack())
            OS << "...";
    }
    else if (const auto *NTTP = dyn_cast<NonTypeTemplateParmDecl>(P)) {
        NTTP->getType().print(OS, PP);
        if (NTTP->isParameterPack())
            OS << "...";
    }
    else if (const auto *TTPD = dyn_cast<TemplateTemplateParmDecl>(P)) {
        printDummyTemplateParams(OS, TTPD->getTemplateParameters(), PP);
        OS << "class";
        if (TTPD->isParameterPack())
            OS << "...";
    }
    else {
        OS << "class";
    }
}

// Print a `template <...>` parameter list with dummy (unnamed) parameters.
static void printDummyTemplateParams(llvm::raw_ostream &OS,
                                     const TemplateParameterList *TPL,
                                     const PrintingPolicy &PP) {
    OS << "template <";
    bool First = true;
    for (const NamedDecl *P : *TPL) {
        if (!First)
            OS << ", ";
        First = false;
        printDummyTemplateParam(OS, P, PP);
    }
    OS << "> ";
}

// Emit a self-contained enum definition: `enum [TagName] [: underlying] { ... }`.
// The fixed underlying type (if any) is printed canonically so it does not depend
// on a typedef such as `uint8_t`, and each enumerator is given its explicit
// integer value so the definition stands alone.
static void printEnumBody(llvm::raw_ostream &OS, const EnumDecl *ED,
                          StringRef TagName, const PrintingPolicy &PP) {
    OS << "enum";
    if (!TagName.empty())
        OS << " " << TagName;
    if (ED->isFixed())
        OS << " : " << ED->getIntegerType().getCanonicalType().getAsString(PP);
    OS << " { ";
    for (const EnumConstantDecl *EC : ED->enumerators()) {
        llvm::SmallString<16> Val;
        EC->getInitVal().toString(Val);
        OS << EC->getName() << " = " << Val << ", ";
    }
    OS << "}";
}

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

        if (const auto *Tag = dyn_cast<TagDecl>(D)) {
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
        Terse.SuppressTagKeyword = false;        // always print `struct X`/`enum X`
        Terse.SuppressDefaultTemplateArgs = false; // print all template args so
                                                   // they match the dummy,
                                                   // default-free declarations

        PrintingPolicy Full = Terse;
        Full.TerseOutput = false;
        Full.IncludeTagDefinition = true;

        OS << "// Auto-generated by the genheader clang-tidy check. Do not edit.\n";
        OS << "// Source: " << InName << "\n\n";

        const bool isC = llvm::sys::path::extension(InName) == ".c";
        if (isC)
            OS << "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n";

        // 1. Types (forward declarations and enum definitions). Typedefs are
        // not emitted; every type below is printed in canonical form. Each
        // declaration is wrapped in its enclosing namespaces so the qualified
        // references below resolve.
        OS << "// --- types ---\n";
        for (const TagDecl *TD : Collector.Tags) {
            std::string Open, Close;
            if (!namespaceWrappers(TD, Open, Close))
                continue; // nested in a record: cannot forward-declare here

            // An anonymous tag named only through a typedef (`typedef struct {}
            // A;`) has no tag name. A file-scope struct/union is referenced via
            // its elaborated `struct A` spelling, so we adopt that name for the
            // tag and forward-declare it. Otherwise (enums, or non-file-scope)
            // bind the typedef name, which is how those are referenced.
            StringRef Name = TD->getName();
            const TypedefNameDecl *Anon =
                Name.empty() ? TD->getTypedefNameForAnonDecl() : nullptr;
            if (Anon)
                Name = Anon->getName();

            if (const auto *ED = dyn_cast<EnumDecl>(TD)) {
                // Enums cannot be portably forward-declared; emit the full
                // definition once, guarded against repeated inclusion.
                std::string G = guardName("enum", Name);
                OS << "#ifndef " << G << "\n#define " << G << "\n" << Open;
                if (Anon) {
                    OS << "typedef ";
                    printEnumBody(OS, ED, /*TagName=*/"", Full);
                    OS << " " << Name;
                } else {
                    printEnumBody(OS, ED, Name, Full);
                }
                OS << ";" << Close << "\n#endif\n";
            }
            else if (Anon && !isElaboratableAnonRecord(cast<RecordDecl>(TD))) {
                // Anonymous struct/union we cannot elaborate (not file scope):
                // name it and bind the typedef so its bare-name references work.
                OS << Open << "typedef " << TD->getKindName() << " " << Name
                   << " " << Name << ";" << Close << "\n";
            }
            else {
                // Named tag, or a file-scope anonymous struct/union adopting its
                // typedef name: a forward declaration (repeatable) suffices.
                OS << Open << TD->getKindName() << " " << Name << ";" << Close
                   << "\n";
            }
        }

        // Class templates whose specializations are referenced: forward-declare
        // the primary template with dummy parameters so `Tmpl<Args>` references
        // resolve. With no default arguments the declaration is repeatable, so
        // no include guard is needed.
        for (const ClassTemplateDecl *CTD : Collector.Templates) {
            std::string Open, Close;
            if (!namespaceWrappers(CTD, Open, Close))
                continue;
            OS << Open;
            printDummyTemplateParams(OS, CTD->getTemplateParameters(), Terse);
            OS << CTD->getTemplatedDecl()->getKindName() << " " << CTD->getName()
               << ";" << Close << "\n";
        }

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

        // 2. Globals.
        OS << "\n// --- globals ---\n";
        for (const VarDecl *VD : Vars) {
            emitDecl(!isC && VD->isExternC(), [&] {
                OS << "extern ";
                elaborateAnonTags(Ctx, VD->getType()).print(OS, Terse,
                                                            VD->getName());
            });
        }

        // 3. Prototypes.
        OS << "\n// --- prototypes ---\n";
        for (const FunctionDecl *FD : Funcs) {
            emitDecl(!isC && FD->isExternC(), [&] {
                elaborateAnonTags(Ctx, FD->getType()).print(OS, Terse,
                                                            FD->getName());
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
