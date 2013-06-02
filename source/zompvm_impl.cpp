///
/// Virtual machine implementation. Contains code interacting with
/// LLVM. Provides services to evaluate LLVM code, create and redefine
/// functions, types and globals
///

#include "zomputils.h"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <map>

#include <curl/curl.h>

#pragma warning(push, 0)
#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/Type.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
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
#include "llvm/Target/TargetSelect.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/TimeValue.h"
#include "llvm/Support/SourceMgr.h"
#include "Llvm/LLVMContext.h"
#pragma warning(pop)

#include "zompvm_caml.h"
#include "zompvm_impl.h"

#include <signal.h>
#if !defined(ZOMP_WINDOWS)
#include <execinfo.h>
#endif

using std::printf;
using namespace llvm;

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

namespace
{
  class RemoteVM
  {
    CURL* curl_;
    std::vector<char> uri_;

  public:
    RemoteVM() {
    }

    ~RemoteVM() {
      disconnect();
    }

    bool connect(const char* address) {
      curl_ = curl_easy_init();

      const size_t addressLength = strlen(address);
      uri_.resize(addressLength + 1);
      memcpy(&uri_[0], address, addressLength + 1);

      return curl_ != NULL;
    }

    void disconnect() {
      if(curl_) {
        curl_easy_cleanup(curl_);
        curl_ = NULL;
      }
    }

    void getUrl(const char* url) {
      if(curl_) {
        enum { BUFFER_LEN = 1024 };
        char buffer[BUFFER_LEN];
        snprintf(buffer, BUFFER_LEN, "%s%s", &uri_[0], url);
        buffer[BUFFER_LEN - 1] = '\0';
        curl_easy_setopt(curl_, CURLOPT_URL, buffer);
        printf("requesting URI %s\n", buffer);
        fflush(stdout);

        CURLcode result = curl_easy_perform(curl_);

        if(result != 0) {
          fprintf(stderr, "error: could not finish transfer %s (error %d)\n", url, result);
        }
      }
    }

    // void sendCode(const char* code) {
    //   CURLcode result = curl_easy_perform(curl_);
    //   if(result != 0) {
    //     fprintf(stderr, "error: could not transfer code\n---\n%s\n---\n", code);
    //   }
    // }
  };

  static RemoteVM remoteVM;

  static LLVMContext* context = 0;
  /// will run the code
  static ExecutionEngine* executionEngine = 0;

  static Module* llvmModule = 0;
  static Module* macroModule = 0;
  static FunctionPassManager* functionPassManager = 0;
  static PassManager* modulePassManager = 0;

  namespace vmoptions {
    static bool verifyCode = true;
    static bool optimizeFunctions = false;
  }

  static Function* makeLLVMFunction(
    const Type* returnType,
    const char* name,
    const Type* argType0 = 0,
    const Type* argType1 = 0,
    const Type* argType2 = 0,
    const Type* argType3 = 0)
  {
    std::vector<const Type*> argTypes;
    if(argType0) {
      argTypes.push_back(argType0);
    }
    if(argType1) {
      argTypes.push_back(argType1);
    }
    if(argType2) {
      argTypes.push_back(argType2);
    }
    if(argType3) {
      argTypes.push_back(argType3);
    }

    FunctionType* funcType = FunctionType::get(returnType, argTypes, false);

    Function* func = Function::Create(
      funcType, GlobalValue::ExternalLinkage, name, llvmModule);
    func->setCallingConv(CallingConv::C);

    return func;
  }

  static Function* simpleAst = NULL;
  static Function* addChild = NULL;
  static Function* macroAstId = NULL;
  static Function* macroAstChildCount = NULL;
  static Function* macroAstChild = NULL;
  static Function* astSetLocation = NULL;
  static Function* astFile = NULL;
  static Function* astLine = NULL;
  static Function* astColumn = NULL;

  static void loadLLVMFunctions()
  {
    const PointerType* cstringType = Type::getInt8PtrTy(*context);
    llvmModule->addTypeName("cstring", cstringType);
    const Type* voidType = Type::getVoidTy(*context);
    const Type* i32Type = Type::getInt32Ty(*context);

    PointerType* astPtrType = 0;

    /// Make ast type. Also needs to be updated in expander.ml and prelude.zomp
    {
      PATypeHolder astFwd = OpaqueType::get(*context);
      astPtrType = getPointerType(astFwd);
      llvmModule->addTypeName("astp", astPtrType);
    
      // see definition in prelude.zomp
      std::vector<const Type*> astStructFields;
      // id
      astStructFields.push_back(cstringType);
      // child count
      astStructFields.push_back(Type::getInt32Ty(*context));
      PointerType* astPtrPtr = getPointerType(astPtrType);
      // childs
      astStructFields.push_back(astPtrPtr);
      // file
      astStructFields.push_back(getPointerType(Type::getInt8Ty(*context)));
      // line
      astStructFields.push_back(Type::getInt32Ty(*context));
      // column
      astStructFields.push_back(Type::getInt32Ty(*context));

      StructType* astType = StructType::get(*context, astStructFields, /*isPacked=*/false);
      llvmModule->addTypeName("ast", astType);
      cast<OpaqueType>(astFwd.get())->refineAbstractTypeTo(astType);
      astType = cast<StructType>(astFwd.get());
    }

    simpleAst = makeLLVMFunction(astPtrType, "ast:fromString", cstringType);
    addChild = makeLLVMFunction(voidType, "ast:addChild", astPtrType, astPtrType);
    macroAstId = makeLLVMFunction(cstringType, "macroAstId", astPtrType);
    macroAstChildCount = makeLLVMFunction(i32Type, "macroAstChildCount", astPtrType);
    macroAstChild = makeLLVMFunction(astPtrType, "macroAstChild", astPtrType, i32Type);

    astSetLocation = makeLLVMFunction(
      voidType,
      "ast:setLocation",
      astPtrType, cstringType, i32Type, i32Type);

    astFile = makeLLVMFunction(cstringType, "ast:file", astPtrType);
    astLine = makeLLVMFunction(i32Type, "ast:line", astPtrType);
    astColumn = makeLLVMFunction(i32Type, "ast:column", astPtrType);
  }

  static void assureModuleExists() {
    if (llvmModule == 0) {
      llvmModule = new Module("llvm_module.bc", *context);
    }

    if( macroModule == 0 ) {
      macroModule = new Module("llvm_macro_module.bc", *context);
    }

    if( simpleAst == 0 ) {
      loadLLVMFunctions();
    }
  }
}

LLVMContext* zompLLVMContext()
{
    return context;
}

llvm::Module* zompLLVMModule()
{
    return llvmModule;
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
  FunctionType* voidType = FunctionType::get(
    Type::getVoidTy(*context),
    argTypes,
    false);

  Function* func = llvmModule->getFunction( name );

  if( func == NULL ) {
    printf( "Function %s not found in module\n", name );
    fflush( stdout );

    func = Function::Create( voidType, GlobalVariable::ExternalLinkage, name, NULL );
  }

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

  // addPass(PM, createRaiseAllocationsPass());     // call %malloc -> malloc inst
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
  // addPass(PM, createCondPropagationPass());      // Propagate conditionals

  addPass(PM, createTailCallEliminationPass());  // Eliminate tail calls
  addPass(PM, createCFGSimplificationPass());    // Merge & remove BBs
  addPass(PM, createReassociatePass());          // Reassociate expressions
  addPass(PM, createLoopRotatePass());
  addPass(PM, createLICMPass());                 // Hoist loop invariants
  addPass(PM, createLoopUnswitchPass());         // Unswitch loops.
  // addPass(PM, createLoopIndexSplitPass());       // Index split loops.
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
  // addPass(PM, createCondPropagationPass());      // Propagate conditionals

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
    if( signal(requestPauseSignal, requestPauseSignalHandler) == SIG_ERR ) {
      fprintf(stderr,
        "Failed to install signal handler. "
        "Requesting pause functionality will not be available");
      fflush(stderr);
    }
  }

#if !defined(ZOMP_WINDOWS)
  static const char* signalName(int signalNumber) {
    switch(signalNumber) {
    case SIGHUP:
      return "SIGHUP";
    case SIGINT:
      return "SIGINT";
    case SIGQUIT:
      return "SIGQUIT";
    case SIGILL:
      return "SIGILL";
    case SIGTRAP:
      return "SIGTRAP";
    case SIGABRT:
      return "SIGABRT";
    case SIGBUS:
      return "SIGBUS";
    case SIGSEGV:
      return "SIGSEGV";
    default:
      return "Unknown signal";
    }
  }

  static void onCrash(int signalNumber) {
    static bool first = true;
    if( first ) {
      first = false;
      fprintf(stderr, "Received signal %s. Callstack:\n", signalName(signalNumber));

      const int max_stack_size = 128;
      void* callstack[max_stack_size];
      int frame_count = backtrace(callstack, max_stack_size);
      backtrace_symbols_fd(callstack, frame_count, STDERR_FILENO);
      fflush(stderr);

      // do not receive signals like SIGBUS multiple times
      signal(signalNumber, SIG_DFL);
    }
    else {
      fprintf(stderr, "Received signal %s.\n", signalName(signalNumber));
      fflush(stderr);
    }
  }

  static void initCrashSignalHandler() {
    int signals_to_handle[] = { SIGHUP, SIGINT, SIGQUIT, SIGILL, SIGTRAP, SIGABRT, SIGBUS, SIGSEGV };
    for( size_t sig_num = 0; sig_num < sizeof(signals_to_handle)/sizeof(int); ++sig_num ) {
      int sig = signals_to_handle[sig_num];
      if( signal(sig, onCrash) == SIG_ERR ) {
        fprintf(stderr, "Failed to install signal handler for %s "
                "No callstacks will be printed on crashes", signalName(sig));
        fflush(stderr);
      }
    }
    printf("Installed crash handlers\n");
    fflush(stdout);
  }
#endif

  bool zompInit() {
    context = &llvm::getGlobalContext();

    assureModuleExists();

    // returning true means failure here
    if( InitializeNativeTarget() ) {
        fprintf(stderr,"Failed to initialize LLVM native target\n");
        return false;
    }

    std::string errorMessage;
    executionEngine = ExecutionEngine::createJIT( llvmModule, &errorMessage );
    if( !executionEngine ) {
        fprintf(stderr, "Failed to init LLVM execution engine\n");
        return false;
    }

    functionPassManager = new FunctionPassManager( llvmModule );
    modulePassManager = new PassManager();
    // setupOptimizerPasses();

    zompInitCamlCallbacks();
    ZMP_ASSERT( zompCamlCallbacksValid(), );

    initPausingSignalHandler();
    /// causes some sort of problem, don't remember which one exactly..
    //if( false ) {
    //  initCrashSignalHandler();
    //}

    return true;
  }

  void zompShutdown() {
    fflush( stdout );
  }

  bool zompConnectToRemoteVM(const char* uri) {
    return remoteVM.connect(uri);
  }

  void zompDisconnectRemoteVM() {
    remoteVM.disconnect();
  }

  void zompSendToRemoteVM(const char* uri) {
    remoteVM.getUrl(uri);
  }

  bool zompSendCode(const char* code, const char* module) {
    bool errorsOccurred = false;

    // ParseError errorInfo;
    SMDiagnostic errorInfo;

    Module* targetModule = llvmModule;
    if( std::string(module) == "compiletime" ) {
      targetModule = macroModule;
    }

    Module* parsedModule = NULL;
    {
      Scope_time_adder profile(stats.parsingTimeMS);
      parsedModule = ParseAssemblyString( code, targetModule, errorInfo, *context );
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

    if( !errorInfo.getMessage().empty() ) {
      int line = errorInfo.getLineNo();
      // int column = errorInfo.getColumnNo();
      fprintf( stderr, "%s:%d: error [LLVM]  %s\n",
               errorInfo.getFilename().c_str(),
               line,
               errorInfo.getMessage().c_str() );
      fflush( stderr );

      errorsOccurred = true;
    }

    return !errorsOccurred;
  }

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

  bool zompLoadFile(const char* filename) {
    return false;
  }


  void zompRunFunction(const char* functionName) {
    runFunction( functionName );
  }

  int zompRunFunctionInt(const char* functionName) {
    GenericValue result = runFunction( functionName );
    return (int)result.IntVal.getLimitedValue();
  }

  static std::vector<const Type*> argTypes;
  static std::vector<GenericValue> argValues;

  void zompResetArgs() {
    argTypes.clear();
    argValues.clear();
  }

  void zompAddIntArg(int arg) {
    argTypes.push_back( Type::getInt32Ty(*context) );
    GenericValue intval;
    intval.IntVal = APInt( 32, arg );
    argValues.push_back( intval );
  }

  void zompAddPointerArg(void* ptr) {
    argTypes.push_back( getPointerType(OpaqueType::get(*context)) );
    argValues.push_back( ptrValue(ptr) );
  }

  int zompRunFunctionIntWithArgs(const char* functionName) {
    GenericValue result = runFunctionWithArgs( functionName, argTypes, argValues );
    return (int)result.IntVal.getLimitedValue();
  }

  int ptrToCamlInt(void* ptr) {
    int addr = bitcast<int>( ptr );

    if( ( addr & 0x8000 ) != 0 ) {
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
    std::cout << "--- We just constructed this LLVM module ---\n\n" << std::flush;
    llvmModule->print(outs(), NULL);
    outs().flush();
    std::cout << "--------------------------------------------\n\n" << std::flush;
  }

  void zompWriteLLVMCodeToFile(const char* fileName) {
    std::string errorInfo;
    raw_fd_ostream file(fileName, errorInfo);
    llvmModule->print(file, NULL);
    ZMP_ASSERT(errorInfo.empty(),);
  }

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
      || (c == '^')
      || (c == '|')
      || (c == '[')
      || (c == ']')
      || (c == '{')
      || (c == '}');
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
    PATypeHolder astFwd = OpaqueType::get(*context);
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

  /**
   * Takes ownership of a bunch of strings and provides fast lookup. This
   * assumes that the number of strings is low and most candidates will already
   * be contained in the storage.
   */
  class StringStorage
  {
    typedef std::vector<char*> StringArray;
    StringArray strings;
    std::string name;

    const char* find(const char* cstr) const
    {
      for( size_t i = 0, size = strings.size(); i < size; ++i ) {
        if( strings[i] == cstr ) {
          return cstr;
        }
        else if( strcmp(cstr, strings[i]) == 0 ) {
          return strings[i];
        }
      }

      return 0;
    }

  public:
    explicit StringStorage(const std::string& name) : name(name)
    {
    }

    ~StringStorage()
    {
      for( size_t i = 0, size = strings.size(); i < size; ++i ) {
        std::free(strings[i]);
        strings[i] = 0;
      }
    }

    /// Returns a string with the same content than the given one that is owned
    /// by the class.
    const char* internalize(const char* cstr)
    {
      const char* internal = find(cstr);
      if(internal) {
        return internal;
      }
      else {
        char* copy = strdup(cstr);
        strings.push_back(copy);
        return copy;
      }
    }

    /// Returns true if this is a pointer to a string owned by the storage.
    /// Will return false if this does not point to the first char of a string
    /// owned by this storage.
    bool isInternalized(const char* cstr) const
    {
      return find(cstr) != 0;
    }

    void dump()
    {
      printf("Content of %s:\n", name.c_str());
      for( size_t i = 0, size = strings.size(); i < size; ++i ) {
        printf("  %s\n", strings[i]);
      }
    }
  };

  static StringStorage fileNames("file names");

  void zompSetAstLocation(void* ast, const char* file, int line, int column) {
    static void (*setLocationF)(void*, const void*, int, int) =
      (void (*)(void*, const void*, int, int)) executionEngine->getPointerToFunction(astSetLocation);
    ZMP_ASSERT( setLocationF, );

    const char* internalFile = fileNames.internalize(file);
    setLocationF(ast, internalFile, line, column);
  }

  const char* zompAstFile(void* ast) {
    typedef const char* (*AstFileFunc)(void* ast);
    static AstFileFunc astFileF = (AstFileFunc) executionEngine->getPointerToFunction(astFile);
    ZMP_ASSERT( astFileF, );

    if(ast) {
      const char* file = astFileF(ast);
      if(file == 0) {
        return "";
      }
      else {
        ZMP_ASSERT(
          file[0] == '\0' ||
          fileNames.isInternalized(file),
          printf("file = '%s'\n", file); fileNames.dump());
        return file;
      }
    }
    else {
      return "";
    }
  }

  int zompAstLine(void* ast) {
    typedef int (*AstLineFunc)(void* ast);
    static AstLineFunc astLineF = (AstLineFunc) executionEngine->getPointerToFunction(astLine);
    ZMP_ASSERT( astLineF, );

    if(ast)
      return astLineF(ast);
    else
      return -1;
  }

  int zompAstColumn(void* ast) {
    typedef int (*AstColumnFunc)(void* ast);
    static AstColumnFunc astColumnF = (AstColumnFunc) executionEngine->getPointerToFunction(astColumn);
    ZMP_ASSERT( astColumnF, );

    if(ast)
      return astColumnF(ast);
    else
      return 0;
  }

  bool zompAstIsNull(void* ast) {
    return ast == NULL;
  }

  bool zompIsDebugBuild() {
#ifdef ZOMP_DEBUG
    return true;
#else
    return false;
#endif
  }

#define ZOMP_TO_STRING2(x) #x
#define ZOMP_TO_STRING(x) ZOMP_TO_STRING2(x)

  const char* zompBuildInfo() {
    const char* build_info = ""
#ifdef ZOMP_DEBUG
      "Debug"
#else
      "Release"
#endif
      ", "
#if defined(ZOMP_WINDOWS)
      "MSVC version = " ZOMP_TO_STRING(_MSC_VER)
#else
      "GCC version = " __VERSION__ ", "
#ifdef __OPTIMIZE__
      "optimization on"
#  ifdef __OPTIMIZE_SIZE__
      " for size"
#  endif
#else
      "optimization off"
#endif
#endif
      "";

    return build_info;
  }

  static std::set<void*> registeredvoids;

  void registervoid(void* ast) {
    registeredvoids.insert(ast);
  }

  void checkvoid(void* ast, const char* func) {
    if( registeredvoids.find(ast) == registeredvoids.end() ) {
      printf( "Warning: found unregistered ast in %s\n", func );
    }
  }

#if !defined(ZOMP_WINDOWS)
  /// ripped from LLVM's compiler-rt project (lib/eprintf.c)
  __attribute__((visibility("hidden")))
  void __eprintf(const char* format, const char* assertion_expression,
    const char* line, const char* file)
  {
    fprintf(stderr, format, assertion_expression, line, file);
    fflush(stderr);
    abort();
  }
#endif
} // extern "C"

