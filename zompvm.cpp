#include <cstdio>
#include <iostream>

#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/ModuleProvider.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include "llvm/Assembly/Parser.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/CallingConv.h"

#include "llvm/PassManager.h"
#include "llvm/Target/TargetData.h"
#include "llvm/LinkAllPasses.h"

#include <dlfcn.h>

using std::printf;
using namespace llvm;

namespace {
  void debugMessage(const char* msg) {
    printf( "[DEBUG] %s", msg );
  }
}

namespace {
  /// will run the code
  static ExecutionEngine* executionEngine = 0;

  static Module* llvmModule = 0;
  static Module* macroModule = 0;
  static ExistingModuleProvider* moduleProvider = 0;
  //   FunctionPassManager* passManager = 0;

  static bool verifyCode = true;

  static Function* simpleAst = NULL;
  static Function* addChild = NULL;
//   static Function* astId = NULL;
//   static Function* astChildCount = NULL;
//   static Function* getChild = NULL;

  static void loadLLVMFunctions()
  {
    PointerType* PointerTy_cstring = PointerType::get(IntegerType::get(8));

    std::vector<const Type*>StructTy_ast_fields;
    llvmModule->addTypeName("cstring", PointerTy_cstring);
  
    StructTy_ast_fields.push_back(PointerTy_cstring);
    StructTy_ast_fields.push_back(IntegerType::get(32));
    PATypeHolder StructTy_ast_fwd = OpaqueType::get();
    PointerType* PointerTy_astp = PointerType::get(StructTy_ast_fwd);
    llvmModule->addTypeName("astp", PointerTy_astp);
  
    PointerType* PointerTy_0 = PointerType::get(PointerTy_astp);
    StructTy_ast_fields.push_back(PointerTy_0);
    StructType* StructTy_ast = StructType::get(StructTy_ast_fields, /*isPacked=*/false);
    llvmModule->addTypeName("ast", StructTy_ast);
    cast<OpaqueType>(StructTy_ast_fwd.get())->refineAbstractTypeTo(StructTy_ast);
    StructTy_ast = cast<StructType>(StructTy_ast_fwd.get());

    std::vector<const Type*>FuncTy_80_args;
    FuncTy_80_args.push_back(PointerTy_astp);
    FuncTy_80_args.push_back(PointerTy_astp);
    ParamAttrsList *FuncTy_80_PAL = 0;
    FunctionType* FuncTy_80 = FunctionType::get(
      Type::VoidTy,
      FuncTy_80_args,
      false,
      FuncTy_80_PAL);

    { // simpleAst decl
      std::vector<const Type*>FuncTy_73_args;
      FuncTy_73_args.push_back(PointerTy_cstring);
      ParamAttrsList *FuncTy_73_PAL = 0;
      FunctionType* FuncTy_73 = FunctionType::get(
        PointerTy_astp, FuncTy_73_args, false, FuncTy_73_PAL);
  
      simpleAst = new Function(
        FuncTy_73,
        GlobalValue::ExternalLinkage,
        "simpleAst", llvmModule); 
      simpleAst->setCallingConv(CallingConv::C);
    }

    { // addChild decl
      addChild = new Function(
        FuncTy_80, GlobalValue::ExternalLinkage, "addChild", llvmModule); 
      addChild->setCallingConv(CallingConv::C);
    }

    { // astId decl
    }

    { // astChildCount decl
    }

    { // getChild decl
    }
    
//     { // simpleAst def
//       Constant* const_int32_117 = Constant::getNullValue(IntegerType::get(32));
//       ConstantInt* const_int32_174 = ConstantInt::get(APInt(32,  "1", 10));
//       ConstantInt* const_int32_187 = ConstantInt::get(APInt(32,  "2", 10));
//       Constant* const_ptr_188 = Constant::getNullValue(PointerTy_cstring);
      
//       Function::arg_iterator args = simpleAst->arg_begin();
//       Value* ptr_name = args++;
//       ptr_name->setName("name");
    
//       BasicBlock* label_404 = new BasicBlock("",simpleAst,0);
    
//       // Block  (label_404)
//       AllocaInst* ptr_a = new AllocaInst(PointerTy_astp, "a", label_404);
//       MallocInst* ptr_temp123 = new MallocInst(StructTy_ast, "temp123", label_404);
//       StoreInst* void_405 = new StoreInst(ptr_temp123, ptr_a, false, label_404);
//       LoadInst* ptr_temp125 = new LoadInst(ptr_a, "temp125", false, label_404);
//       std::vector<Value*> ptr_temp124_indices;
//       ptr_temp124_indices.push_back(const_int32_117);
//       ptr_temp124_indices.push_back(const_int32_117);
//       Instruction* ptr_temp124 = new GetElementPtrInst(ptr_temp125, ptr_temp124_indices.begin(), ptr_temp124_indices.end(), "temp124", label_404);
//       StoreInst* void_406 = new StoreInst(ptr_name, ptr_temp124, false, label_404);
//       LoadInst* ptr_temp127 = new LoadInst(ptr_a, "temp127", false, label_404);
//       std::vector<Value*> ptr_temp126_indices;
//       ptr_temp126_indices.push_back(const_int32_117);
//       ptr_temp126_indices.push_back(const_int32_174);
//       Instruction* ptr_temp126 = new GetElementPtrInst(ptr_temp127, ptr_temp126_indices.begin(), ptr_temp126_indices.end(), "temp126", label_404);
//       StoreInst* void_407 = new StoreInst(const_int32_117, ptr_temp126, false, label_404);
//       LoadInst* ptr_temp129 = new LoadInst(ptr_a, "temp129", false, label_404);
//       std::vector<Value*> ptr_temp128_indices;
//       ptr_temp128_indices.push_back(const_int32_117);
//       ptr_temp128_indices.push_back(const_int32_187);
//       Instruction* ptr_temp128 = new GetElementPtrInst(ptr_temp129, ptr_temp128_indices.begin(), ptr_temp128_indices.end(), "temp128", label_404);
//       CastInst* ptr_temp130 = new BitCastInst(const_ptr_188, PointerTy_0, "temp130", label_404);
//       StoreInst* void_408 = new StoreInst(ptr_temp130, ptr_temp128, false, label_404);
//       LoadInst* ptr_temp131 = new LoadInst(ptr_a, "temp131", false, label_404);
//       new ReturnInst(ptr_temp131, label_404);
    
//     }
  }
  
  static void assureModuleExists() {
    if (llvmModule == 0) {
      llvmModule = new Module("llvm_module.bc");
    }

    if( macroModule == 0 ) {
      macroModule = new Module("llvm_macro_module.bc");
    }

    if( simpleAst == 0 ) {
      loadLLVMFunctions();
    }
  }
}

extern "C" {
  void zompHello() {
    printf( "hello, testmessage\n" );
  }

  int blah(int a, int b, int c) {
    printf( "blah( %d, %d, %d )\n", a, b, c );
    return 99;
  }

  void zompVerifyCode(bool doit) {
    verifyCode = doit;
  }
  
  bool zompDoesVerifyCode() {
    return verifyCode;
  }
  
}


llvm::GenericValue runFunctionWithArgs(
  const char* name,
  const std::vector<const Type*>& argTypes,
  const std::vector<GenericValue>& args)
{
  FunctionType* voidType = FunctionType::get( Type::VoidTy, argTypes, false, NULL );

  Function* func = llvmModule->getFunction( name );

  if( func == NULL ) {
    printf( "Function %s not found in module\n", name );
    fflush( stdout );
      
    func = new Function( voidType, GlobalVariable::ExternalLinkage, name, NULL );
  }

//   std::vector<GenericValue> args;
  GenericValue retval = executionEngine->runFunction( func, args );

  fflush( stdout );
  fflush( stderr );

  return retval;
}

/**
 * Will run the function `name` with the arguments added using
 * zompAdd*Arg previously
 */
llvm::GenericValue runFunction(const char* name) {
  std::vector<const Type*> noparams;
  std::vector<GenericValue> noargs;
  return runFunctionWithArgs( name, noparams, noargs );
}

namespace
{
  template<typename Target, typename Source>
  union CastUnion
  {
    Target target;
    Source source;
  };

  template<typename Target, typename Source>
  static Target bitcast(Source source)
  {
    CastUnion<Target, Source> u;
    u.source = source;
    return u.target;
  }

  static GenericValue ptrValue(void* p) {
    GenericValue v;
    v.PointerVal = p;
    return v;
  }

  static GenericValue ptrValue(int i) {
    GenericValue v;
    v.PointerVal = bitcast<void*>(i);
    return v;
  }

//   static GenericValue intValue(int i) {
//     GenericValue v;
//     v.IntVal = i;
//     return v;
//   }
}

extern "C" {

//   void setupOptimizerPasses() {
//     // Set up the optimizer pipeline.  Start with registering info about how the
//     // target lays out data structures.
//     passManager->add(new TargetData( *executionEngine->getTargetData()));
//     // Do simple "peephole" optimizations and bit-twiddling optzns.
//     passManager->add(createInstructionCombiningPass());
//     // Reassociate expressions.
//     passManager->add(createReassociatePass());
//     // Eliminate Common SubExpressions.
//     passManager->add(createGVNPass());
//     // Simplify the control flow graph (deleting unreachable blocks, etc).
//     passManager->add(createCFGSimplificationPass());
//   }
  
  bool zompInit() {
//     printf( "Initializing ZompVM\n" );
    fflush( stdout );
  
    assureModuleExists();
    moduleProvider = new ExistingModuleProvider( llvmModule );
    executionEngine = ExecutionEngine::create( moduleProvider, false );
//     passManager = new FunctionPassManager( moduleProvider );
//     setupOptimizerPasses();

    return true;
  }
  
  void zompShutdown() {
//     printf( "Shutting down ZompVM\n" );
    fflush( stdout );
  }

  bool zompSendCode(const char* code, const char* module) {
    bool errorsOccurred = false;
    
    ParseError errorInfo;

    Module* targetModule = llvmModule;
    if( std::string(module) == "compiletime" ) {
      targetModule = macroModule;
    }

    Module* parsedModule = ParseAssemblyString( code, targetModule, &errorInfo );
    std::string errorMessage;
    if( parsedModule == NULL ) {
      printf( "Parsed module is NULL\n" );
      fflush( stdout );
      
      errorsOccurred = true;
    }
//     else if( true == verifyModule(*parsedModule, PrintMessageAction, &errorMessage) ) {
    else if( verifyCode && true == verifyModule(*targetModule, PrintMessageAction, &errorMessage) ) {
      printf( "Parsed module did not verify: %s\n", errorMessage.c_str() );
      fflush( stdout );
      fflush( stderr );

      errorsOccurred = true;
    }

    if( errorInfo.getRawMessage() != "none" ) {
      fprintf( stderr, "[LLVM] %s\n", errorInfo.getMessage().c_str() );
      fflush( stderr );

      errorsOccurred = true;
    }

//     // run optimizations
//     if( ! errorsOccurred && parsedModule ) {
//       llvm::Module::iterator currentFunc = parsedModule->begin();
//       const llvm::Module::iterator funcsEnd = parsedModule->end();

//       for( ; currentFunc != funcsEnd; ++currentFunc) {
//         passManager->run( *currentFunc );
//       }
//     }

    return !errorsOccurred;
  }
  
//   bool zompSendCodeNewVar(const char* code) {
//     printf( "Creating new var\n" );
//     fflush( stdout );
//     return zompSendCode( code );
//   }
  
//   bool zompSendCodeNewFunc(const char* code) {
//     printf( "Create new function\n" );
//     fflush( stdout );
//     return zompSendCode( code );
//   }

  bool zompRemoveFunctionBody(const char* functionName) {
    Function* func = llvmModule->getFunction( functionName );

    if( func != NULL ) {
      func->deleteBody();

      return true;
    }

    return false;
  }

  bool zompRecompileAndRelinkFunction(const char* funcName) {
    Function* func = llvmModule->getFunction( funcName );

    if( func != NULL ) {
      executionEngine->recompileAndRelinkFunction( func );

      return true;
    }

    return false;
  }
  
  
//   bool zompSendCodeModifyFunc(const char* code) {
//     printf( "Modifying existing function\n" );
//     fflush( stdout );
//     return zompSendCode( code );
    
//     ParseError errorInfo;

    
//     Module* module = ParseAssemblyString( code, llvmModule, &errorInfo );
// //     Module* module = ParseAssemblyString( code, NULL, &errorInfo );

//     if( module != NULL ) {
//       std::cout
//         << "--- We just constructed this LLVM module ---\n\n"
//         << *module << "\n"
//         << "--------------------------------------------\n\n";
//       std::cout.flush();
//     }
    
//     if( errorInfo.getRawMessage() != "none" ) {
//       fprintf( stderr, "[LLVM] %s\n", errorInfo.getMessage().c_str() );
//       fflush( stderr );

//       return false;
//     }

//     return true;
//   }

  bool zompLoadFile(const char* filename) {
    return false;
  }


  void zompRunFunction(const char* functionName) {
    runFunction( functionName );
  }

  int zompRunFunctionInt(const char* functionName) {
    GenericValue result = runFunction( functionName );
    return result.IntVal.getLimitedValue();
  }

  std::vector<const Type*> argTypes;
  std::vector<GenericValue> argValues;

  void zompResetArgs() {
    argTypes.clear();
    argValues.clear();
  }

  void zompAddIntArg(int arg) {
    argTypes.push_back( Type::Int32Ty );
    GenericValue intval;
    intval.IntVal = APInt( 32, arg );
    argValues.push_back( intval );
  }

  void zompAddPointerArg(int ptr) {
    argTypes.push_back( PointerType::get(OpaqueType::get()) );
    argValues.push_back( ptrValue(ptr) );
  }

  int zompRunFunctionIntWithArgs(const char* functionName) {
    GenericValue result = runFunctionWithArgs( functionName, argTypes, argValues );
    return result.IntVal.getLimitedValue();
  }

  int ptrToCamlInt(void* ptr) {
    int addr = bitcast<int>( ptr );

    if( addr & 0x8000 != 0 ) {
      printf( "Warning: address has highest bit set, replaced with 0" );
      fflush( stdout );
      exit( 123 );
    }

    return addr;
  }
  
  int zompRunFunctionPointerWithArgs(const char* functionName) {
    GenericValue result = runFunctionWithArgs( functionName, argTypes, argValues );
    return ptrToCamlInt( result.PointerVal );
  }

  const char* zompRunFunctionStringWithArgs(const char* functionName) {
    GenericValue result = runFunctionWithArgs( functionName, argTypes, argValues );
    static std::string buffer;
    buffer = (const char*) result.PointerVal;
    return buffer.c_str();
  }

  const char* zompRunFunctionString(const char* functionName) {
    GenericValue result = runFunction( functionName );

    static std::string buffer;

    buffer = (const char*) result.PointerVal;
    return buffer.c_str();
  }

  bool zompRunFunctionBool(const char* functionName) {
    GenericValue result = runFunction( functionName );

    return result.IntVal != 0;
  }
  

  void zompPrintModuleCode() {
    std::cout
      << "--- We just constructed this LLVM module ---\n\n"
      << *llvmModule << "\n"
      << "--------------------------------------------\n\n";
  
    /*
    std::string llvmCode = llvmModule->getModuleInlineAsm();
    
    const char* header = "--- Inline ASM of module ---";
    const char* decls =  "------- LLVM Symbols -------";
    const char* hline =  "----------------------------";
    printf( "%s\n%s\n%s\n", header, llvmCode.c_str(), decls );

    const Module::global_iterator globalsEnd = llvmModule->global_end();
    for(Module::global_iterator global = llvmModule->global_begin(); global != globalsEnd; ++global) {
      global->print( cout );
    }

    const Module::iterator functionsEnd = llvmModule->end();
    for(Module::iterator func = llvmModule->begin(); func != functionsEnd; ++func) {
      std::string name = func->getName();
      std::string rettypeName = "???";
      std::string arguments = "???";
      std::string impl = func->isDeclaration() ? ";" : " { ... }";
      
      printf( "%s %s(%s)%s\n", rettypeName.c_str(), name.c_str(), arguments.c_str(), impl.c_str() );
//       func->print( cout );
    }

    printf( "%s\n", hline );
    */
  }

  int zompLoadLib(const char* name) {
    void* handle = dlopen( name, RTLD_LAZY );
//     void* handle = dlopen( "dlltest.dylib", RTLD_LAZY );

    if( handle == NULL ) {
      printf( "Could not load dll '%s': %s\n", name, dlerror() );
      fflush( stdout );
    }

    return reinterpret_cast<int>( handle );
  }

  bool zompCheckNativeSymbol(const char* name) {
    return dlsym( NULL, name ) != NULL ? true : false;
  }


//   struct Ast
//   {
//     char* id;
//     int childCount;
//     Ast** childs;
//   };

//   char* macroAstId(int ast) {
//     if( ast != 0 ) {
//       return (reinterpret_cast<Ast*>(ast))->id;
//     }
//     else {
//       return "NULL";
//     }
//   }

//   int macroAstChildCount(int ast) {
//     if( ast != 0 ) {
//       return (reinterpret_cast<Ast*>(ast))->childCount;
//     }
//     else {
//       return 0;
//     }
//   }

//   int macroAstChild(int ast, int num) {
//     if( ast != 0 && num < macroAstChildCount(ast) ) {
//       return reinterpret_cast<int>( (reinterpret_cast<Ast*>(ast))->childs + num );
//     }
//     else {
//       return 0;
//     }
//   }
  
// (func cstring macroAstId ((int macroCurrentAst)) (
//   (getField (cast (ptr ast) macroCurrentAst) id) ))

// (func int macroAstChildCount ((int macroCurrentAst)) (
//   (getField (cast (ptr ast) macroCurrentAst) childCount) ))

// (func int macroAstChild ((int treeaddr) (int num)) (
//   (var (ptr ast) tree (cast (ptr ast) treeaddr))
//   (var (ptr ast) child (load (ptradd (getField tree childs) num)))
//   (var int i (cast int child))
//   (ret i)
//   ))


  static bool validIdChar(char c) {
    return (c >= 'a' && c <= 'z')
      || (c >= '0' && c <= '9')
      || (c >= 'A' && c <= 'Z')
      || (c == '_')
      || (c == ':')
      || (c == '=')
      || (c == '!')
      || (c == '<')
      || (c == '>')
      || (c == '*')
      || (c == '/')
      || (c == '+')
      || (c == '-')
      || (c == '.')
      || (c == '\\')
      || (c == '\'')
      || (c == ';');
  }
  
  void checkId(const char* id, const char* func) {
    bool valid = true;

    const int length = strlen(id);

    if( length >= 2 && ((id[0] == '"' && id[length-1] == '"') || (id[0] == '\'' && id[length-1] == '\'')) ) {
      const unsigned char* ptr = (unsigned char*)id;
      while( *ptr != '\0' ) {
        if( *ptr++ > 127 ) {
          valid = false;
        }
      }
    }
    else {
      const char* ptr = id;
      while( *ptr != '\0' ) {
        valid &= validIdChar(*ptr++);
      }
    }
      
    if( ! valid ) {
      printf("Found invalid id '%s' in '%s'\n", id, func);
    }
  }

  int zompSimpleAst(char* name) {
    checkId(name, "zompSimpleAst");
    
    std::vector<GenericValue> args;
    
    args.push_back( ptrValue(name) );

    GenericValue retval = executionEngine->runFunction( simpleAst, args );

    return ptrToCamlInt( retval.PointerVal );
  }

  void zompAddChild(int parent, int child) {
    std::vector<GenericValue> args;

    args.push_back( ptrValue(parent) );
    args.push_back( ptrValue(child) );

    executionEngine->runFunction( addChild, args );
  }

  static std::set<void*> registeredAsts;

  void registerAst(void* ast) {
    registeredAsts.insert(ast);
  }

  void checkAst(void* ast, const char* func) {
    if( registeredAsts.find(ast) == registeredAsts.end() ) {
      printf( "Warning: found unregistered ast in %s\n", func );
    }
  }
  
  
//   typedef std::map<int, void*> IdToAstMapping;
//   IdToAstMapping astIdTable;
//   typedef std::map<void*, int> AstToIdMapping;
//   AstToIdMapping idAstTable;
//   static int lastAstId = 0;

//   int addAstToTable(void* ast) {
//     ++lastAstId;
    
//     astIdTable.insert( std::make_pair(lastAstId, ast) );
//     idAstTable.insert( std::make_pair(ast, lastAstId) );
//     return lastAstId;
//   }

//   int findAstId(void* ast) {
//     AstToIdMapping::iterator iter = idAstTable.find(ast);
//     if( iter != idAstTable.end() ) {
//       return iter->second;
//     }
//     else {
//       return 0;
//     }
//   }

//   void* findAstById(int id) {
//     IdToAstMapping::iterator iter = astIdTable.find(id);
//     if( iter != astIdTable.end() ) {
//       return iter->second;
//     }
//     else {
//       return NULL;
//     }
//   }

//   int zompSimpleAst(char* name) {
//     std::vector<GenericValue> args;
    
//     args.push_back( ptrValue(name) );

//     GenericValue retval = executionEngine->runFunction( simpleAst, args );

//     return addAstToTable(retval.PointerVal);
//   }

//   void zompAddChild(int parent, int child) {
//     void* parentPtr = findAstById(parent);
//     void* childPtr = findAstById(child);

//     if( parentPtr != NULL && childPtr != NULL ) {
//       std::vector<GenericValue> args;

//       args.push_back( ptrValue(parentPtr) );
//       args.push_back( ptrValue(childPtr) );
    
//       executionEngine->runFunction( addChild, args );
//     }
//     else {
//       printf( "Warning: zompAddChild invoked with invalid ast ids" );
//       fflush(stdout);
//     }
//   }

//   GenericValue call1(Function* func, void* arg0) {
//     std::vector<GenericValue> args;
//     args.push_back( ptrValue(arg0) );
//     return executionEngine->runFunction(func, args);
//   }

//   GenericValue call2(Function* func, void* arg0, int arg1) {
//     std::vector<GenericValue> args;
//     args.push_back( ptrValue(arg0) );
//     args.push_back( intValue(arg1) );
//     return executionEngine->runFunction(func, args);
//   }

//   const char* zompAstId (int ast) {
//     void* astPtr = findAstById(ast);

//     if( astPtr != NULL ) {
//       return (const char*) call1(astId, astPtr).PointerVal;
//     }
//     else {
//       printf( "Warning: zompAstId called with invalid ast id" );
//       fflush( stdout );
//       return "compiler:error:invalidAstId(zompAstId)";
//     }
//   }
  
//   const int zompAstChildCount (int ast) {
//     void* astPtr = findAstById(ast);

//     if( astPtr != NULL ) {
//       return call1(astChildCount, astPtr).IntVal.getLimitedValue();
//     }
//     else {
//       printf( "Warning: zompAstChildCount called with invalid ast id" );
//       fflush( stdout );
//       return 0;
//     }
//   }
  
//   int zompAstChild (int ast, int num) {
//     void* astPtr = findAstById(ast);
//     if( astPtr != NULL ) {
//       void* childId = call2(getChild, astPtr, num).PointerVal;
//       return findAstId( childId );
//     }
//     else {
//       printf( "Warning: zompAstChild called with invalid ast id" );
//       fflush( stdout );
//       return 0;
//     }
//   }
    

  void zompRunMacro() {
  }
  
} // extern "C"




