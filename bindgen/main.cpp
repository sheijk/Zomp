//===- PrintFunctionNames.cpp ---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Example clang plugin which simply prints the names of all the top-level decls
// in the input file.
//
//===----------------------------------------------------------------------===//

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/AST.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

namespace {

std::string zompTypeName(const Type* t)
{
    if(t==0)
    {
        return "type_was_null";
    }

    if ( const BuiltinType* bt = dyn_cast<BuiltinType>(t) )
    {
        LangOptions opt;
        return bt->getName(opt);
    }
    else
    {
        return "unsupported_type";
    }
}
    
class PrintFunctionsConsumer : public ASTConsumer
{
    ASTContext* m_context;
    SourceManager* m_src_manager;
    FileID m_main_file_id;

public:
    PrintFunctionsConsumer() : m_context(0), m_src_manager(0)
    {
    }

    virtual void Initialize(ASTContext &context)
    {
        m_context = &context;
        m_src_manager = &context.getSourceManager();
        m_main_file_id = m_src_manager->getMainFileID();
    }
        
    virtual void HandleTopLevelDecl(DeclGroupRef DG)
    {
        for (DeclGroupRef::iterator i = DG.begin(), e = DG.end(); i != e; ++i)
        {
            const Decl *D = *i;

            const TranslationUnitDecl* tu = D->getTranslationUnitDecl();
            if( !tu ) {
                continue;
            }

            const SourceLocation& loc = D->getLocation();
            if( m_src_manager->getFileID(loc) != m_main_file_id)
            {
                continue;

                // PresumedLoc orig_loc = m_src_manager->getPresumedLoc(loc);
                // llvm::outs() << "// by including "
                //              << orig_loc.getFilename()
                //              << ": ";
            }

            handleAs<VarDecl>(D) ||
                handleAs<FunctionDecl>(D) ||
                handleAs<TypedefDecl>(D) ||
                handleUnknown(D);
        }
    }

private:

    template<typename T>
    bool handleAs(const Decl* D)
    {
        const T* typed_decl = dyn_cast<T>(D);
        if(typed_decl)
        {
            return handle(typed_decl);
        }
        else
        {
            return false;
        }
    }

    bool handle(const FunctionDecl* func_decl)
    {
        bool ignore = func_decl->isCXXClassMember()
            || func_decl->isCXXInstanceMember();

        if(ignore) {
            return true;
        }

        llvm::outs() << "nativeFn ";
        llvm::outs() << zompTypeName(func_decl->getResultType().getTypePtrOrNull()) << " ";
        llvm::outs() << func_decl->getNameAsString() << "(";
        bool first_param = true;
        for( FunctionDecl::param_const_iterator param_i = func_decl->param_begin(),
                 pend = func_decl->param_end( );
             param_i != pend;
             ++param_i, first_param = false )
        {
            ParmVarDecl* param = *param_i;

            if( !first_param ) {
                llvm::outs() << ", ";
            }

            QualType typesrc = param->getTypeSourceInfo()->getType();
            const Type* type = typesrc.getTypePtrOrNull();

            llvm::outs() << zompTypeName(type);

            if( IdentifierInfo* id = param->getIdentifier() )
            {
                llvm::outs() << " "
                             << param->getNameAsString();
            }

            if ( param->hasDefaultArg() )
            {
                llvm::outs() << " /* = ... */";
            }
        }
        llvm::outs() << ")\n";
        return true;
    }

    bool handle(const VarDecl* var_decl)
    {
        llvm::outs() << "// ignored var decl "
                     << var_decl->getNameAsString()
                     << "\n";
        return true;
    }

    bool handle(const TypedefDecl* type_decl)
    {
        return false;
        // llvm::outs() << "nativeType "
        //              << type_decl->getName()
        //              << "\n";
    }

    bool handleUnknown(const Decl* D)
    {
        if(const NamedDecl* n = dyn_cast<NamedDecl>(D))
        {
            llvm::outs() << "// ignored " << n->getNameAsString() << "\n";
        }
        else
        {
            llvm::outs() << "// ignored nameless declaration\n";
        }

        return true;
    }
};
    
class PrintFunctionNamesAction : public PluginASTAction
{
protected:
    ASTConsumer *CreateASTConsumer(CompilerInstance &CI, llvm::StringRef) {
        return new PrintFunctionsConsumer();
    }
        
    bool ParseArgs(const CompilerInstance &CI,
        const std::vector<std::string>& args) {
        for (unsigned i = 0, e = args.size(); i != e; ++i) {
            llvm::errs() << "PrintFunctionNames arg = " << args[i] << "\n";
                
            // Example error handling.
            if (args[i] == "-an-error") {
                Diagnostic &D = CI.getDiagnostics();
                unsigned DiagID = D.getCustomDiagID(
                    Diagnostic::Error, "invalid argument '" + args[i] + "'");
                D.Report(DiagID);
                return false;
            }
        }
        if (args.size() && args[0] == "help")
            PrintHelp(llvm::errs());
            
        return true;
    }
    void PrintHelp(llvm::raw_ostream& ros) {
        ros << "Help for PrintFunctionNames plugin goes here\n";
    }
        
};
    
} // anonymous namespace

static FrontendPluginRegistry::Add<PrintFunctionNamesAction>
X("print-fns", "print function names");