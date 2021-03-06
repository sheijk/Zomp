#ifndef ZOMPVM_H_20070929_INCLUDED
#define ZOMPVM_H_20070929_INCLUDED

#include "zomputils.h"

#ifdef __cplusplus
namespace llvm {
    class LLVMContext;
    class Module;
}

llvm::LLVMContext* zompLLVMContext();
llvm::Module* zompLLVMModule();

extern "C" {
#endif

#define ZMP_CONST const

bool zompInit();
void zompShutdown();
bool zompSendCode(ZMP_CONST char* code, ZMP_CONST char* module);
bool zompLoadFile(ZMP_CONST char* filename);
void zompRunFunction(ZMP_CONST char* name);
void zompPrintModuleCode();
void zompPrintFunctionCode(const char* name);
void zompPrintGlobalVarCode(const char* name);
void zompVerifyCode(bool on);
bool zompDoesVerifyCode();
bool zompRemoveFunctionBody(ZMP_CONST char* functionName);
bool zompRecompileAndRelinkFunction(ZMP_CONST char* funcName);

void zompResetArgs();
void zompAddIntArg(int arg);

int zompRunFunctionInt(ZMP_CONST char* functionName);
int zompRunFunctionIntWithArgs(ZMP_CONST char* functionName);

void zompAddPointerArg(void* ptr);
void* zompRunFunctionPointerWithArgs(ZMP_CONST char* functionName);
ZMP_CONST char* zompRunFunctionString(ZMP_CONST char* functionName);
ZMP_CONST char* zompRunFunctionStringWithArgs(ZMP_CONST char* functionName);
bool zompRunFunctionBool(ZMP_CONST char* functionName);
void* zompSimpleAst(ZMP_CONST char* name);
void zompAddChild(void* parent, void* child);
ZMP_CONST char* zompAstId(void* ast);
int zompAstChildCount(void* ast);
void* zompAstChild(void* ast, int num);
void zompResetMacroArgs();
void zompAddMacroArg(void* ptr);
void* zompAddressOfMacroFunction(ZMP_CONST char* name);
void* zompCallMacro(void* macroAddress);
ZMP_CONST char* float2string(double d);
void zompPrintTimingStats();

// from runtime.c
void* zompLoadLib(ZMP_CONST char* name);

#ifdef __cplusplus
} // extern "C"
#endif

#endif

