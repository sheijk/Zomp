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

static std::string errorType(const char* msg)
{
    return std::string("error_t(\"") + msg + "\")"; 
}

static std::string zompTypeName(const Type* t)
{
    if( t == 0 )
    {
        return "type_was_null";
    }

    if( const BuiltinType* bt = dyn_cast<BuiltinType>(t) )
    {
        switch(bt->getKind())
        {
        case BuiltinType::Void: return "void";
        case BuiltinType::UShort: return "c_ushort";
        case BuiltinType::UInt: return "c_uint";
        case BuiltinType::ULong: return "c_ulong";
        case BuiltinType::ULongLong: return "c_ulong_long";
        case BuiltinType::UInt128: return "u128";
        case BuiltinType::Short: return "c_short";
        case BuiltinType::Int: return "c_int";
        case BuiltinType::Long: return "c_long";
        case BuiltinType::LongLong: return "c_long_long";
        case BuiltinType::Int128: return "s128";
        case BuiltinType::Float: return "float";
        case BuiltinType::Double: return "double";
        case BuiltinType::LongDouble: return "c_long_double";

        case BuiltinType::Char_U: return "c_implicit_uchar";
        case BuiltinType::Char_S: return "c_implicit_schar";
        case BuiltinType::UChar: return "c_uchar";
        case BuiltinType::SChar: return "c_schar";

        case BuiltinType::WChar_U:
        case BuiltinType::Char16:
        case BuiltinType::Char32:
        case BuiltinType::WChar_S:

        case BuiltinType::NullPtr:
        case BuiltinType::Dependent:
        case BuiltinType::Overload:

        case BuiltinType::ObjCId:
        case BuiltinType::ObjCClass:
        case BuiltinType::ObjCSel:
            
        default:
            return "UnsupportedBuiltinType";
        }
    }
    else if( const PointerType* pt = dyn_cast<PointerType>(t) )
    {
        QualType base_type = pt->getPointeeType();
        return zompTypeName(base_type.getTypePtrOrNull()) + "*";
    }
    else if( const ArrayType* at = dyn_cast<ArrayType>(t) )
    {
        assert( at );
        return errorType( "bindgen does not support array types, yet" );
    }
    else if( const FunctionType* ft = dyn_cast<FunctionType>(t) )
    {
        assert( ft );
        return errorType("bindgen does not support function types, yet" );
    }
    else if( const TypedefType* tt = dyn_cast<TypedefType>(t) )
    {
        return tt->getDecl()->getName();
        // return zompTypeName(
            // tt->getDecl()->getCanonicalDecl()->getUnderlyingType().getTypePtrOrNull() );
    }
    else if( const EnumType* et = dyn_cast<EnumType>(t) )
    {
        return et->getDecl()->getNameAsString();
    }
    else if( const RecordType* rt = dyn_cast<RecordType>(t) )
    {
        return rt->getDecl()->getNameAsString();
    }
    else
    {
        return errorType("type not understood by bindgen");
    }
}

static std::string zompTypeName( const QualType& qual_type )
{
    return zompTypeName( qual_type.getTypePtrOrNull() );
}

/** Visitor which will handle every top level declaration */
class GenBindingsConsumer : public ASTConsumer
{
    ASTContext* m_context;
    SourceManager* m_src_manager;
    FileID m_main_file_id;

public:
    GenBindingsConsumer() : m_context(0), m_src_manager(0)
    {
    }

    virtual void Initialize(ASTContext &context)
    {
        m_context = &context;
        m_src_manager = &context.getSourceManager();
        m_main_file_id = m_src_manager->getMainFileID();

        const char* main_file_name = m_src_manager->getFileEntryForID( m_main_file_id )->getName();
        llvm::outs() << "///\n";
        llvm::outs() << "/// Zomp bindings for " << main_file_name << "\n";
        llvm::outs() << "///\n";
        llvm::outs() << "\n";
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
            }

            handleAs<VarDecl>(D) ||
                handleAs<FunctionDecl>(D) ||
                handleAs<TypedefDecl>(D) ||
                handleAs<RecordDecl>(D) ||
                handleAs<EnumDecl>(D) ||
                handleUnknown(D);
        }
    }

private:

    enum HandlingResult { Handled, CouldNotHandle };

    template<typename T>
    bool handleAs(const Decl* D)
    {
        const T* typed_decl = dyn_cast<T>(D);
        if(typed_decl)
        {
            const bool result = handle(typed_decl) == Handled;
            // llvm::outs() << "\n";
            return result;
        }
        else
        {
            return false;
        }
    }

    HandlingResult handle(const FunctionDecl* func_decl)
    {
        bool ignore = func_decl->isCXXClassMember()
            || func_decl->isCXXInstanceMember()
            || func_decl->isVariadic()
            || func_decl->isMain();

        if(ignore) {
            return CouldNotHandle;
        }

        llvm::outs() << "nativeFn ";
        llvm::outs() << zompTypeName( func_decl->getResultType() ) << " ";
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

            if( param->getIdentifier() )
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

        return Handled;
    }

    HandlingResult handle(const VarDecl* var_decl)
    {
        llvm::outs() << "nativeVar "
            << zompTypeName( var_decl->getTypeSourceInfo()->getType() )
            << " "
            << var_decl->getNameAsString()
            << "\n";

        return Handled;
    }

    bool handle(const TypedefDecl* type_decl)
    {
        QualType typesrc = type_decl->getUnderlyingType();
        const Type* type = typesrc.getTypePtrOrNull();

        llvm::outs() << "nativeType "
                     << type_decl->getName()
                     << " "
                     << zompTypeName(type)
                     << "\n";

        return Handled;
    }

    bool handle(const RecordDecl* record_decl)
    {
        if( record_decl->isAnonymousStructOrUnion() )
        {
            llvm::outs() << "// ignoring anonymous struct\n";
            return CouldNotHandle;
        }

        if( record_decl->isDefinition() )
        {
            llvm::outs() << "nativeStruct " << record_decl->getName() << ":\n";

            typedef RecordDecl::field_iterator FieldIter;
            for( FieldIter fi = record_decl->field_begin(), fi_end = record_decl->field_end();
                 fi != fi_end;
                 ++fi )
            {
                const FieldDecl& field = **fi;
                if( field.isBitField() )
                {
                    llvm::outs() << "  // ignored bitfield, not supported\n";
                }
                else
                {
                    llvm::outs()
                        << "  "
                        << zompTypeName( field.getType() )
                        << " "
                        << field.getName()
                        << "\n";
                }
            }

            llvm::outs() << "end\n";
        }
        else
        {
            llvm::outs() << "nativeStruct " << record_decl->getName() << "\n";
        }

        return Handled;
    }

    bool handle(const EnumDecl* enum_decl)
    {
        if( enum_decl->isComplete() )
        {
            llvm::outs()
                << "nativeEnum "
                << enum_decl->getName() << " "
                << zompTypeName( enum_decl->getPromotionType() )
                << ":\n";

            typedef EnumDecl::enumerator_iterator EnumIter;
            for( EnumIter variant = enum_decl->enumerator_begin(), vend = enum_decl->enumerator_end();
                 variant != vend;
                 ++variant )
            {
                EnumConstantDecl* ecd = *variant;

                llvm::outs()
                    << "  "
                    << ecd->getName() << " "
                    << ecd->getInitVal()
                    << "\n";
            }
        
            llvm::outs() << "end\n";
        }
        else
        {
            llvm::outs()
                << "nativeEnum "
                << enum_decl->getName()
                << "\n";
        }

        return Handled;
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

        return Handled;
    }
};

/** The plugin class. Will instantiate GenBindingsConsumer and run it */
class GenBindingsAction : public PluginASTAction
{
protected:
    ASTConsumer *CreateASTConsumer(CompilerInstance &CI, llvm::StringRef)
    {
        return new GenBindingsConsumer();
    }
        
    bool ParseArgs(
        const CompilerInstance &CI,
        const std::vector<std::string>& args )
    {
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

    void PrintHelp(llvm::raw_ostream& ros)
    {
        ros << "Translates declarations of a C file into Zomp declarations.\n"
            "This makes it easy to use C libaries from Zomp.";
    }
};
    
} // anonymous namespace

static FrontendPluginRegistry::Add<GenBindingsAction> X("gen-zomp-bindings", "Generate Zomp bindings");

