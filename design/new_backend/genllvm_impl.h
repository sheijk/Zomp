#IFNDEF BACKEND_LLVM_H_20140725_INCLUDED
#define BACKEND_LLVM_H_20140725_INCLUDED

#include "zomputils.h"

/// Query Zomp compiler

/// Types

struct ZompType;
struct ZompIntType;
struct ZompBasicType; // Types.integralType
struct ZompFloatType;
struct ZompDoubleType;
struct ZompPointerType;
struct ZompArrayType;
struct ZompRecordType;

const char* zomTypeName(ZompType*);

int zompTypeIntBits(ZompIntType*);

/// Values

struct ZompValue;

/// Source Locations

struct ZompFile;

/// Functions

struct ZompFunction;

/// Variables

struct ZompVariable;

/// All objects created by functions in this file are owned by the ZompBackend
/// (with the exception of the backend itself, of course).

struct ZompNativeBackend;

ZompBackend* zompCreateLlvmBackend(...);
void zompDeleteLlvmBackend(ZompBackend*);

void zompEmitTypedef(ZompBackend*, const char* name, Type* target);
void zompEmitGlobalVar(ZompBackend*, ZompVariable*);

// struct ZompStructType;
// ZompStructType* zompEmitTypeStruct(ZompBackend*, const char* name);
// void zompAddStructMember(ZompBackend*, ZompStructType*, const char* name, ZompType* type);

// struct GlobalVar;
// GlobalVar* zompEmitGlobalVar();
// 
// struct FuncDecl;
// FuncDecl* zompEmitFuncDecl();

struct ZompFunction;
ZompFunction* zompCreateFunction(ZompBackend*, ZompSourceLocation*, FunctionType*, const char* name);

struct ZompVariable;
ZompVariable* zompAddLocalVariable(ZompFunction*, const char* name, Type* type);

struct ZompBasicBlock;
ZompBasicBlock* zompCreateBasicBlock(ZompFunction*, ZompSourceLocation*, const char* name);

struct ZompExpression;

ZompExpression* zompIntConstant(ZompFunction*, int value);
ZompExpression* zompFloatConstant(ZompFunction*, float value);
// ...

void zompAssignVar(ZompFunction*, ZompVariable*, ZompExpression*);

void zompBranch(ZompFunction*, ZompVariable*, ZompBasicBlock*, ZompBasicBlock*);
void ZompFunctionCall

// evtl. als stack machine?

#endif
