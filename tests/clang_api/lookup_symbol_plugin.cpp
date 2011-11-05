///
/// A simple clang plugin which will translate the given C file into Zomp
/// definitions
///

#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/AST.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/FileManager.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;

namespace {

/** Visitor which will handle every top level declaration */
class GenBindingsConsumer : public ASTConsumer
{
    ASTContext* m_context;
    SourceManager* m_src_manager;
    FileID m_main_file_id;
    const llvm::StringRef m_searched_name;

public:
    GenBindingsConsumer(llvm::StringRef symbol_name) :
        m_context(0),
        m_src_manager(0),
        m_searched_name( symbol_name ) 
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
        for( DeclGroupRef::iterator i = DG.begin(), e = DG.end(); i != e; ++i )
        {
            const Decl *D = *i;

            if( const NamedDecl* nd = dyn_cast<NamedDecl>(D) )
            {
                if( m_searched_name == nd->getName() )
                {
                    SourceLocation loc = nd->getLocation();
                    const char* file_name = m_src_manager->getBufferName( loc );
                    unsigned int line = m_src_manager->getPresumedLineNumber( loc );

                    llvm::outs() << file_name << ":" << line << "\n";
                }
            }
        }
    }
};

/** The plugin class. Will instantiate GenBindingsConsumer and run it */
class GenBindingsAction : public PluginASTAction
{
    llvm::StringRef m_searched_name;

protected:
    ASTConsumer *CreateASTConsumer(CompilerInstance &CI, llvm::StringRef)
    {
        return new GenBindingsConsumer( m_searched_name );
    }

    bool ParseArgs(
        const CompilerInstance &CI,
        const std::vector<std::string>& args )
    {
        if( args.size() && args[0] == "help" )
        {
            PrintHelp(llvm::errs());
            return true;
        }

        if( args.size() == 1 )
        {
            m_searched_name = args[0];

            return true;
        }
        else
        {
            Diagnostic &D = CI.getDiagnostics();
            unsigned DiagID = D.getCustomDiagID(
                Diagnostic::Error,
                "You need to set the symbol to look for using "
                " -lookup-symbol symbol-name" );
            D.Report(DiagID);
            return false;
        }
    }

    void PrintHelp(llvm::raw_ostream& ros)
    {
        ros << "Translates declarations of a C file into Zomp declarations.\n"
            "This makes it easy to use C libaries from Zomp.";
    }
};
    
} // anonymous namespace

static FrontendPluginRegistry::Add<GenBindingsAction> X("lookup-symbol", "Lookup a symbol");

