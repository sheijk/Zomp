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

#include <dlfcn.h>

using std::printf;
using namespace llvm;

namespace {
  void debugMessage(const char* msg) {
    printf( "[DEBUG] %s", msg );
  }
}

namespace {
  // will run the code
  ExecutionEngine* executionEngine = 0;

  Module* llvmModule = 0;
  Module* macroModule = 0;
  ExistingModuleProvider* moduleProvider = 0;
  
  void assureModuleExists() {
    if (llvmModule == 0) {
//       debugMessage("Creating new llvm module\n");
      llvmModule = new Module("llvm_module.bc");
    }

    if( macroModule == 0 ) {
      macroModule = new Module("llvm_macro_module.bc");
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

llvm::GenericValue runFunction(const char* name) {
  std::vector<const Type*> noparams;
  std::vector<GenericValue> noargs;
  return runFunctionWithArgs( name, noparams, noargs );
}

extern "C" {
#include "zompvm.h"

  bool zompInit() {
//     printf( "Initializing ZompVM\n" );
    fflush( stdout );
  
    assureModuleExists();
    moduleProvider = new ExistingModuleProvider( llvmModule );
    executionEngine = ExecutionEngine::create( moduleProvider, false );

    return true;
  }
  
  void zompShutdown() {
//     printf( "Shutting down ZompVM\n" );
    fflush( stdout );
  }

  bool zompSendCode(const char* code, const char* module) {
    ParseError errorInfo;

    Module* targetModule = llvmModule;
    if( std::string(module) == "compiletime" ) {
      targetModule = macroModule;
    }
  
    ParseAssemblyString( code, targetModule, &errorInfo );

    if( errorInfo.getRawMessage() != "none" ) {
      fprintf( stderr, "[LLVM] %s\n", errorInfo.getMessage().c_str() );
      fflush( stderr );

      return false;
    }

    return true;
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

  int zompRunFunctionIntWithArgs(const char* functionName) {
    GenericValue result = runFunctionWithArgs( functionName, argTypes, argValues );
    return result.IntVal.getLimitedValue();
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
  
} // extern "C"


