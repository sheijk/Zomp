#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>

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
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/PassManager.h"
#include "llvm/Target/TargetData.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/System/TimeValue.h"

#include "zomputils.h"

#include "zompvm.h"

extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
}

#include <signal.h>

using std::printf;
using namespace llvm;

///-----------------------------------------------------------------------------
/// {{{Native Callbacks

/**
 * These are callbacks to the parts of the zomp compiler/runtime implemented
 * in OCaml. You can find the OCaml counter parts in ZompVM.Callable
 */
namespace ZompCallbacks {
  value* isBoundCB = NULL;
  value* lookupCB = NULL;

  void init() {
    isBoundCB = caml_named_value("isBound");
    lookupCB = caml_named_value("lookup");
  }

  bool areValid() {
    return
      isBoundCB != NULL &&
      lookupCB != NULL;
  }

  extern "C" {

    bool isBound(char* name) {
      ZMP_ASSERT(ZompCallbacks::isBoundCB != NULL,);

      value result = caml_callback(*ZompCallbacks::isBoundCB, caml_copy_string(name));
      return Bool_val(result);
    }

    enum { // also defined in zompvm.ml
      ZOMP_SYMBOL_UNDEFINED = 0,
      ZOMP_SYMBOL_VAR = 1,
      ZOMP_SYMBOL_FUNC = 2,
      ZOMP_SYMBOL_MACRO = 3,
      ZOMP_SYMBOL_TYPEDEF = 4,
      ZOMP_SYMBOL_LABEL = 5
    };

    int zompLookup(char* name) {
      ZMP_ASSERT( ZompCallbacks::lookupCB != NULL,);
      value result = caml_callback(*ZompCallbacks::lookupCB, caml_copy_string(name));
      return Int_val(result);
    }

  } // extern "C"
}

///}}}


namespace {
  struct Stats {
    typedef uint64_t milliseconds;

    milliseconds parsingTimeMS;
    milliseconds verifyTimeMS;
    milliseconds optimizingTimeMS;

    Stats() {
      parsingTimeMS = 0;
      verifyTimeMS = 0;
      optimizingTimeMS = 0;
    }

    void print() {
      printf("\nZompVM stats:\n");
      log(parsingTimeMS, "Parsing");
      log(verifyTimeMS, "Verifying");
      log(optimizingTimeMS, "Optimizing");
      fflush(stdout);
    }

  private:
    void log(uint64_t time, const char* name) {
      printf("  %f - %s LLVM code\n", float(time) / 1000.0f, name);
    }
  };

  static Stats stats;

  class Scope_time_adder {
    typedef llvm::sys::TimeValue TimeValue;
    uint64_t& timeVar;
    TimeValue startTime;

  public:
    Scope_time_adder(uint64_t& var) :
      timeVar(var),
      startTime(TimeValue::now())
    {}

    ~Scope_time_adder() {
      TimeValue endTime = TimeValue::now();
      timeVar += (endTime - startTime).msec();
    }
  };
}

///-----------------------------------------------------------------------------
/// Virtual machine and execution environment

static PointerType *getPointerType(const Type *ElementType) {
  return PointerType::get(ElementType, 0);
}

namespace {
  /// will run the code
  static ExecutionEngine* executionEngine = 0;

  static Module* llvmModule = 0;
  static Module* macroModule = 0;
  static ExistingModuleProvider* moduleProvider = 0;
  static FunctionPassManager* functionPassManager = 0;
  static PassManager* modulePassManager = 0;

  namespace vmoptions {
    static bool verifyCode = true;
    static bool optimizeFunctions = false;
  }

  static Function* simpleAst = NULL;
  static Function* addChild = NULL;
  static Function* macroAstId = NULL;
  static Function* macroAstChildCount = NULL;
  static Function* macroAstChild = NULL;

  static void loadLLVMFunctions()
  {
    PointerType* cstringPtr = getPointerType(IntegerType::get(8));
    llvmModule->addTypeName("cstring", cstringPtr);

    // see definition in stdlib.zomp
    std::vector<const Type*> astStructFields;
    // id
    astStructFields.push_back(cstringPtr);
    // child count
    astStructFields.push_back(IntegerType::get(32));
    PATypeHolder astFwd = OpaqueType::get();
    PointerType* astPtr = getPointerType(astFwd);
    llvmModule->addTypeName("astp", astPtr);

    PointerType* astPtrPtr = getPointerType(astPtr);
    // childs
    astStructFields.push_back(astPtrPtr);
    StructType* ast = StructType::get(astStructFields, /*isPacked=*/false);
    llvmModule->addTypeName("ast", ast);
    cast<OpaqueType>(astFwd.get())->refineAbstractTypeTo(ast);
    ast = cast<StructType>(astFwd.get());

    std::vector<const Type*>FuncTy_80_args;
    FuncTy_80_args.push_back(astPtr);
    FuncTy_80_args.push_back(astPtr);
    // ParamAttrsList *FuncTy_80_PAL = 0;
    FunctionType* FuncTy_80 = FunctionType::get(
      Type::VoidTy,
      FuncTy_80_args,
      false);
      // FuncTy_80_PAL);

    { // simpleAst decl
      std::vector<const Type*>FuncTy_73_args;
      FuncTy_73_args.push_back(cstringPtr);
      // ParamAttrsList *FuncTy_73_PAL = 0;
      FunctionType* FuncTy_73 = FunctionType::get(astPtr, FuncTy_73_args, false);
        // FuncTy_73_PAL);

      simpleAst = Function::Create(
        FuncTy_73,
        GlobalValue::ExternalLinkage,
        "ast:fromString", llvmModule);
      simpleAst->setCallingConv(CallingConv::C);
    }

    { // addChild decl
      addChild = Function::Create(
        FuncTy_80, GlobalValue::ExternalLinkage, "ast:addChild", llvmModule);
      addChild->setCallingConv(CallingConv::C);
    }

    { // macroAstId decl
      std::vector<const Type*>FuncTy_61_args;
      // FuncTy_61_args.push_back(IntegerType::get(32));
      FuncTy_61_args.push_back(astPtr);
      // ParamAttrsList *FuncTy_61_PAL = 0;
      FunctionType* FuncTy_61 = FunctionType::get(
        /*Result=*/cstringPtr,
        /*Params=*/FuncTy_61_args,
        /*isVarArg=*/false);
        // /*ParamAttrs=*/FuncTy_61_PAL);
      macroAstId = Function::Create(
        FuncTy_61,
        GlobalValue::ExternalLinkage,
        "macroAstId", llvmModule);
      macroAstId->setCallingConv(CallingConv::C);
    }

    { // macroAstChildCount decl
      std::vector<const Type*>macroAstChildCountArgs;
      // macroAstChildCountArgs.push_back(IntegerType::get(32));
      macroAstChildCountArgs.push_back(astPtr);
      // ParamAttrsList *FuncTy_59_PAL = 0;
      FunctionType* FuncTy_59 = FunctionType::get(
        // /*Result=*/IntegerType::get(32),
        /*Result=*/IntegerType::get(32),
        /*Params=*/macroAstChildCountArgs,
        /*isVarArg=*/false);
        // /*ParamAttrs=*/FuncTy_59_PAL);

      macroAstChildCount = Function::Create(
        FuncTy_59,
        GlobalValue::ExternalLinkage,
        "macroAstChildCount", llvmModule);
      macroAstChildCount->setCallingConv(CallingConv::C);
    }

    { // macroAstChild decl
      std::vector<const Type*>FuncTy_105_args;
      // FuncTy_105_args.push_back(IntegerType::get(32));
      FuncTy_105_args.push_back(astPtr);
      FuncTy_105_args.push_back(IntegerType::get(32));
      // ParamAttrsList *FuncTy_105_PAL = 0;
      FunctionType* FuncTy_105 = FunctionType::get(
        // /*Result=*/IntegerType::get(32),
        /*Result=*/astPtr,
        /*Params=*/FuncTy_105_args,
        /*isVarArg=*/false);
        // /*ParamAttrs=*/FuncTy_105_PAL);

      macroAstChild = Function::Create(
        FuncTy_105,
        GlobalValue::ExternalLinkage,
        "macroAstChild", llvmModule); 
      macroAstChild->setCallingConv(CallingConv::C);
    }

//     { // simpleAst def
//       Constant* const_int32_117 = Constant::getNullValue(IntegerType::get(32));
//       ConstantInt* const_int32_174 = ConstantInt::get(APInt(32,  "1", 10));
//       ConstantInt* const_int32_187 = ConstantInt::get(APInt(32,  "2", 10));
//       Constant* const_ptr_188 = Constant::getNullValue(cstringPtr);

//       Function::arg_iterator args = simpleAst->arg_begin();
//       Value* ptr_name = args++;
//       ptr_name->setName("name");

//       BasicBlock* label_404 = new BasicBlock("",simpleAst,0);

//       // Block  (label_404)
//       AllocaInst* ptr_a = new AllocaInst(astPtr, "a", label_404);
//       MallocInst* ptr_temp123 = new MallocInst(ast, "temp123", label_404);
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
//       CastInst* ptr_temp130 = new BitCastInst(const_ptr_188, astPtrPtr, "temp130", label_404);
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

///-----------------------------------------------------------------------------
/// primitives called from OCaml code

extern "C" {
  void zompHello() {
    printf( "hello, testmessage\n" );
  }

  int blah(int a, int b, int c) {
    printf( "blah( %d, %d, %d )\n", a, b, c );
    return 99;
  }

  void zompVerifyCode(bool doit) {
    vmoptions::verifyCode = doit;
  }

  bool zompDoesVerifyCode() {
    return vmoptions::verifyCode;
  }

  bool zompOptimizeFunction () {
    return vmoptions::optimizeFunctions;
  }

  void zompSetOptimizeFunction (bool optimize) {
    vmoptions::optimizeFunctions = optimize;
  }

  const char* float2string(double d) {
    // see WriteConstantInt in llvm lib/VMCore/AsmPrinter.cpp for more details
    APFloat apfloat((float)d);
    d = apfloat.convertToFloat();

    std::string str = utohexstr(DoubleToBits(d));
    const unsigned int BUFFER_SIZE = 1000;
    static char buffer[BUFFER_SIZE];
    buffer[0] = '0';
    buffer[1] = 'x';
    strncpy(buffer+2, str.c_str(), BUFFER_SIZE-2);

    return buffer;
  }

  void zompPrintStats() {
    stats.print();
  }
} // extern C

llvm::GenericValue runFunctionWithArgs(
  const char* name,
  const std::vector<const Type*>& argTypes,
  const std::vector<GenericValue>& args)
{
  FunctionType* voidType = FunctionType::get( Type::VoidTy, argTypes, false);

  Function* func = llvmModule->getFunction( name );

  if( func == NULL ) {
    printf( "Function %s not found in module\n", name );
    fflush( stdout );

    func = Function::Create( voidType, GlobalVariable::ExternalLinkage, name, NULL );
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
  static std::vector<const Type*> noparams;
  static std::vector<GenericValue> noargs;
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

static void addPass(PassManager& pm, Pass* p) {
  pm.add(p);
}

/// ripped from LLVM's tools/opt/opt.cpp
static void addStandardCompilePasses(PassManager &PM) {
  PM.add(createVerifierPass());                  // Verify that input is correct

  addPass(PM, createLowerSetJmpPass());          // Lower llvm.setjmp/.longjmp

  // If the -strip-debug command line option was specified, do it.
  // if (StripDebug)
  addPass(PM, createStripSymbolsPass(true));

  // if (DisableOptimizations) return;

  addPass(PM, createRaiseAllocationsPass());     // call %malloc -> malloc inst
  addPass(PM, createCFGSimplificationPass());    // Clean up disgusting code
  addPass(PM, createPromoteMemoryToRegisterPass());// Kill useless allocas
  addPass(PM, createGlobalOptimizerPass());      // Optimize out global vars
  addPass(PM, createGlobalDCEPass());            // Remove unused fns and globs
  addPass(PM, createIPConstantPropagationPass());// IP Constant Propagation
  addPass(PM, createDeadArgEliminationPass());   // Dead argument elimination
  addPass(PM, createInstructionCombiningPass()); // Clean up after IPCP & DAE
  addPass(PM, createCFGSimplificationPass());    // Clean up after IPCP & DAE

  addPass(PM, createPruneEHPass());              // Remove dead EH info
  addPass(PM, createFunctionAttrsPass());        // Deduce function attrs

  // if (!DisableInline)
  addPass(PM, createFunctionInliningPass());   // Inline small functions
  addPass(PM, createArgumentPromotionPass());    // Scalarize uninlined fn args

  addPass(PM, createSimplifyLibCallsPass());     // Library Call Optimizations
  addPass(PM, createInstructionCombiningPass()); // Cleanup for scalarrepl.
  addPass(PM, createJumpThreadingPass());        // Thread jumps.
  addPass(PM, createCFGSimplificationPass());    // Merge & remove BBs
  addPass(PM, createScalarReplAggregatesPass()); // Break up aggregate allocas
  addPass(PM, createInstructionCombiningPass()); // Combine silly seq's
  addPass(PM, createCondPropagationPass());      // Propagate conditionals

  addPass(PM, createTailCallEliminationPass());  // Eliminate tail calls
  addPass(PM, createCFGSimplificationPass());    // Merge & remove BBs
  addPass(PM, createReassociatePass());          // Reassociate expressions
  addPass(PM, createLoopRotatePass());
  addPass(PM, createLICMPass());                 // Hoist loop invariants
  addPass(PM, createLoopUnswitchPass());         // Unswitch loops.
  addPass(PM, createLoopIndexSplitPass());       // Index split loops.
  // FIXME : Removing instcombine causes nestedloop regression.
  addPass(PM, createInstructionCombiningPass());
  addPass(PM, createIndVarSimplifyPass());       // Canonicalize indvars
  addPass(PM, createLoopDeletionPass());         // Delete dead loops
  addPass(PM, createLoopUnrollPass());           // Unroll small loops
  addPass(PM, createInstructionCombiningPass()); // Clean up after the unroller
  addPass(PM, createGVNPass());                  // Remove redundancies
  addPass(PM, createMemCpyOptPass());            // Remove memcpy / form memset
  addPass(PM, createSCCPPass());                 // Constant prop with SCCP

  // Run instcombine after redundancy elimination to exploit opportunities
  // opened up by them.
  addPass(PM, createInstructionCombiningPass());
  addPass(PM, createCondPropagationPass());      // Propagate conditionals

  addPass(PM, createDeadStoreEliminationPass()); // Delete dead stores
  addPass(PM, createAggressiveDCEPass());        // Delete dead instructions
  addPass(PM, createCFGSimplificationPass());    // Merge & remove BBs
  addPass(PM, createStripDeadPrototypesPass());  // Get rid of dead prototypes
  addPass(PM, createDeadTypeEliminationPass());  // Eliminate dead types
  addPass(PM, createConstantMergePass());        // Merge dup global constants
}

static void setupOptimizerPasses() {
  ZMP_ASSERT(functionPassManager,);
  // Set up the optimizer pipeline.  Start with registering info about how the
  // target lays out data structures.
  functionPassManager->add(new TargetData( *executionEngine->getTargetData()));
  functionPassManager->add(createCFGSimplificationPass());
  functionPassManager->add(createScalarReplAggregatesPass());
  functionPassManager->add(createInstructionCombiningPass());

  ZMP_ASSERT(modulePassManager,);
  modulePassManager->add(new TargetData(llvmModule));
  addStandardCompilePasses(*modulePassManager);
}

extern "C" {

  void zompOptimizeFunctions() {
    Scope_time_adder profile(stats.optimizingTimeMS);
    // run optimizations
    const llvm::Module::iterator funcsEnd = llvmModule->end();

    for(llvm::Module::iterator currentFunc = llvmModule->begin();
        currentFunc != funcsEnd;
        ++currentFunc)
    {
      if( ! currentFunc->isDeclaration() ) {
        // verifyFunction( *currentFunc );
        functionPassManager->run( *currentFunc );
        // verifyFunction( *currentFunc );
      }
    }

    modulePassManager->run(*llvmModule);

    // recompile all functions
    for(llvm::Module::iterator currentFunc = llvmModule->begin();
        currentFunc != funcsEnd;
        ++currentFunc)
    {
      if( ! currentFunc->isDeclaration() ) {
        // no deleteBody in this case, we do not produce a new one
        executionEngine->recompileAndRelinkFunction( currentFunc );
      }
    }
  }

  /// functionality to request the running program in the toplevel to pause it's
  /// execution (return from main) so a recompilation can happen
  static bool zompDidReqestPause = false;

  bool zompRequestedPause() {
    return zompDidReqestPause;
  }

  void zompSetRequestPause(bool request) {
    zompDidReqestPause = request;
  }

  static const int requestPauseSignal = SIGINT;

  void requestPauseSignalHandler(int signalNumber) {
    ZMP_ASSERT(requestPauseSignal == signalNumber,);

    zompDidReqestPause = true;
  }

  static void initPausingSignalHandler() {
    if( signal(requestPauseSignal, requestPauseSignalHandler) == SIG_IGN ) {
      fprintf(stderr,
              "Failed to install signal handler. "
              "Requesting pause functionality will not be available");
      fflush(stderr);
    }
  }

  bool zompInit() {
    assureModuleExists();
    moduleProvider = new ExistingModuleProvider( llvmModule );
    executionEngine = ExecutionEngine::create( moduleProvider, false );
    functionPassManager = new FunctionPassManager( moduleProvider );
    modulePassManager = new PassManager();
    setupOptimizerPasses();

    // value* closure_f = NULL;
    // closure_f = caml_named_value("helloCallback");
    // caml_callback(*closure_f, Val_unit);
    //
    // value* printString = caml_named_value("printString");
    // caml_callback(*printString, caml_copy_string("This is ZompVM"));
    //
    // value* getTrue = caml_named_value("getTrue");
    // value result = caml_callback(*getTrue, Val_unit);
    // std::cout << "Received bool from OCaml: " << Bool_val(result) << std::endl;

    ZompCallbacks::init();
    ZMP_ASSERT( ZompCallbacks::areValid(), );

    initPausingSignalHandler();

    return true;
  }

  void zompShutdown() {
    fflush( stdout );
  }

  bool zompSendCode(const char* code, const char* module) {
    bool errorsOccurred = false;

    ParseError errorInfo;

    Module* targetModule = llvmModule;
    if( std::string(module) == "compiletime" ) {
      targetModule = macroModule;
    }

    using llvm::sys::TimeValue;

    Module* parsedModule = NULL;
    {
      Scope_time_adder profile(stats.parsingTimeMS);
      parsedModule = ParseAssemblyString( code, targetModule, errorInfo );
    }

    std::string errorMessage;
    if( parsedModule == NULL ) {
      printf( "Parsed module is NULL\n" );
      fflush( stdout );

      errorsOccurred = true;
    }
    else {
      if( vmoptions::verifyCode ) {
        Scope_time_adder prof(stats.verifyTimeMS);
        bool isValid = ! verifyModule(
            *targetModule, PrintMessageAction, &errorMessage);

        if( ! isValid ) {
          printf( "Parsed module did not verify: %s\n", errorMessage.c_str() );
          fflush( stdout );
          fflush( stderr );

          errorsOccurred = true;
        }
      }
    }

    if( errorInfo.getRawMessage() != "none" ) {
      int line, column;
      errorInfo.getErrorLocation(line, column);
      fprintf( stderr, "%s:%d: error [LLVM]  %s\n",
               errorInfo.getFilename().c_str(),
               line,
               errorInfo.getRawMessage().c_str() );
      fflush( stderr );

      errorsOccurred = true;
    }

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
      if( vmoptions::optimizeFunctions ) {
        Scope_time_adder profile(stats.optimizingTimeMS);

        llvm::Module::iterator currentFunc = llvmModule->begin();
        const llvm::Module::iterator funcsEnd = llvmModule->end();

        for( ; currentFunc != funcsEnd; ++currentFunc) {
          if( ! currentFunc->isDeclaration() ) {
            // verifyFunction( *currentFunc );
            functionPassManager->run( *currentFunc );
            // verifyFunction( *currentFunc );
          }
        }
      }

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

  void zompAddPointerArg(void* ptr) {
    argTypes.push_back( getPointerType(OpaqueType::get()) );
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

    return ptrToInt(ptr);
  }

  void* zompRunFunctionPointerWithArgs(const char* functionName) {
    GenericValue result = runFunctionWithArgs( functionName, argTypes, argValues );
    return result.PointerVal;
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

  void zompWriteLLVMCodeToFile(const char* fileName) {
    std::ofstream file(fileName);
    file << *llvmModule;
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
      || (c == '%')
      || (c == '+')
      || (c == '-')
      || (c == '.')
      || (c == '\\')
      || (c == '\'')
      || (c == ';')
      || (c == '&')
      || (c == '|')
      || (c == '[')
      || (c == ']');
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

  static std::vector<void*> macroArgs;

  void zompResetMacroArgs() {
    macroArgs.clear();
  }

  void zompAddMacroArg(void* ptr) {
    macroArgs.push_back(ptr);
  }

  void* zompAddressOfMacroFunction(const char* name) {
    PATypeHolder astFwd = OpaqueType::get();
    PointerType* ast_ptr = getPointerType(astFwd);
    std::vector<const Type*> args;
    args.push_back(ast_ptr);
    FunctionType* macroFuncType = FunctionType::get(ast_ptr, args, false);

    Function* macroFunc = llvmModule->getFunction( name );

    if( macroFunc == NULL ) {
      printf( "Macro function %s not found in module\n", name );
      fflush( stdout );

      Function* macroFunc = Function::Create(macroFuncType,
                                             GlobalValue::ExternalLinkage,
                                             name, llvmModule);
      macroFunc->setCallingConv(CallingConv::C);
    }

    void* funcAddr = (void*) executionEngine->getPointerToFunction(macroFunc);
    ZMP_ASSERT(funcAddr,);

    return funcAddr;
  }

  void* zompCallMacro(void* macroAddress) {
    void* (*macroFunc)(void**) = (void* (*)(void**))macroAddress;
    void** macroArguments = &macroArgs[0];
    void* resultAst = macroFunc(macroArguments);
    return resultAst;
  }

// improves compiliation time of realistic programs by two
#define ZOMP_CACHED_FUNCS

#ifdef ZOMP_CACHED_FUNCS

  void* zompSimpleAst(const char* name) {
    checkId(name, "zompSimpleAst");

    static void* (*simpleAstF)(const void*) =
      (void* (*)(const void*)) executionEngine->getPointerToFunction(simpleAst);
    ZMP_ASSERT( simpleAstF, );

    return simpleAstF(name);
  }

  void zompAddChild(void* parent, void* child) {
    static void (*addChildF)(void*, void*) =
      (void (*)(void*,void*)) executionEngine->getPointerToFunction(addChild);
    ZMP_ASSERT( addChildF, );

    addChildF( parent, child );
  }

  const char* zompAstId(void* ast) {
    static char* (*macroAstIdF)(void*) =
      (char* (*)(void*)) executionEngine->getPointerToFunction(macroAstId);
    ZMP_ASSERT( macroAstIdF != NULL ,);

    return macroAstIdF(ast);
  }

  int zompAstChildCount(void* ast) {
    static int (*macroAstChildCountF)(void*) =
      (int (*)(void*)) executionEngine->getPointerToFunction(macroAstChildCount);
    ZMP_ASSERT( macroAstChildCountF != NULL ,);

    return macroAstChildCountF(ast);
  }

  void* zompAstChild(void* ast, int num) {
    static void* (*macroAstChildF)(void*, int) =
      (void* (*)(void*, int)) executionEngine->getPointerToFunction(macroAstChild);
    ZMP_ASSERT( macroAstChildF != NULL ,);

    return macroAstChildF(ast, num);
  }

  bool zompAstIsNull(void* ast) {
    return ast == NULL;
  }

#else

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

#endif

  static std::set<void*> registeredvoids;

  void registervoid(void* ast) {
    registeredvoids.insert(ast);
  }

  void checkvoid(void* ast, const char* func) {
    if( registeredvoids.find(ast) == registeredvoids.end() ) {
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


  // void zompRunMacro() {
  // }

} // extern "C"

