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
  ExistingModuleProvider* moduleProvider = 0;
  
  void assureModuleExists() {
    if (llvmModule == 0) {
      debugMessage("Creating new llvm module\n");
      llvmModule = new Module("llvm_module.bc");
    }
  }
}

extern "C" {
  void zompHello() {
    printf("hello, testmessage\n");
  }
}

extern "C" {
#include "zompvm.h"

  bool zompInit() {
    printf( "Initializing ZompVM\n" );
    fflush( stdout );
  
    assureModuleExists();
    moduleProvider = new ExistingModuleProvider( llvmModule );
    executionEngine = ExecutionEngine::create( moduleProvider, false );

    return true;
  }
  
  void zompShutdown() {
    printf( "Shutting down ZompVM\n" );
    fflush( stdout );
  }

  bool zompSendCode(const char* code) {
    ParseError errorInfo;
  
    Module* module = ParseAssemblyString( code, llvmModule, &errorInfo );

    if( errorInfo.getRawMessage() != "none" ) {
      fprintf( stderr, "[LLVM] %s\n", errorInfo.getMessage().c_str() );
      fflush( stderr );

      return false;
    }

    return true;
  }

  bool zompLoadFile(const char* filename) {
    return false;
  }

  void zompRunFunction(const char* name) {
    std::vector<const Type*> noargs;
    FunctionType* voidType = FunctionType::get( Type::VoidTy, noargs, false, NULL );

    Function* func = llvmModule->getFunction( name );

    if( func == NULL ) {
      printf( "Function %s not found in module\n", name );
      fflush( stdout );
      
      func = new Function( voidType, GlobalVariable::ExternalLinkage, name, NULL );
    }

    std::vector<GenericValue> args;
    executionEngine->runFunction( func, args );

    fflush( stdout );
    fflush( stderr );
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
  
} // extern "C"


