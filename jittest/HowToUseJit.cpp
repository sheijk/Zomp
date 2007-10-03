//===-- examples/HowToUseJIT/HowToUseJIT.cpp - An example use of the JIT --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file was developed by Valery A. Khamenya and is distributed under the
// University of Illinois Open Source License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This small program provides an example of how to quickly build a small
//  module with two functions and execute it with the JIT.
//
// Goal:
//  The goal of this snippet is to create in the memory
//  the LLVM module consisting of two functions as follow:
//
// int add1(int x) {
//   return x+1;
// }
//
// int foo() {
//   return add1(10);
// }
//
// then compile the module via JIT, then execute the `foo'
// function and return result to a driver, i.e. to a "host program".
//
// Some remarks and questions:
//
// - could we invoke some code using noname functions too?
//   e.g. evaluate "foo()+foo()" without fears to introduce
//   conflict of temporary function name with some real
//   existing function name?
//
//===----------------------------------------------------------------------===//

#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/ModuleProvider.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"
#include <iostream>
using namespace llvm;

Function* createFoo(const std::string& name, Module* M, Function* Add1F) {  
  // Now we going to create function `foo', which returns an int and takes no
  // arguments.
  Function *FooF =
    cast<Function>(M->getOrInsertFunction(name, Type::Int32Ty, (Type *)0));

  // Add a basic block to the FooF function.
  BasicBlock* BB = new BasicBlock("EntryBlock", FooF);
  
  // Get pointers to the constant `10'.
  Value *Ten = ConstantInt::get(Type::Int32Ty, 10);

  // Pass Ten to the call call:
  CallInst *Add1CallRes = new CallInst(Add1F, Ten, "add1", BB);
  Add1CallRes->setTailCall(true);

  // Create the return instruction and add it to the basic block.
  new ReturnInst(Add1CallRes, BB);

//   std::vector<const Type*> noargs;
//   FunctionType* funcType = FunctionType::get( Type::Int32Ty, noargs, false, NULL );
//   Function *otherF = new Function( funcType, GlobalVariable::InternalLinkage, name, M );
  Function* otherF = M->getFunction(name);
  
  return otherF;
}

Function* createAdd(Module* M) {
  // Create the add1 function entry and insert this entry into module M.  The
  // function will have a return type of "int" and take an argument of "int".
  // The '0' terminates the list of argument types.
  Function *Add1F =
    cast<Function>(M->getOrInsertFunction("add1", Type::Int32Ty, Type::Int32Ty,
                                          (Type *)0));

  // Add a basic block to the function. As before, it automatically inserts
  // because of the last argument.
  BasicBlock *BB = new BasicBlock("EntryBlock", Add1F);

  // Get pointers to the constant `1'.
  Value *One = ConstantInt::get(Type::Int32Ty, 1);

  // Get pointers to the integer argument of the add1 function...
  assert(Add1F->arg_begin() != Add1F->arg_end()); // Make sure there's an arg
  Argument *ArgX = Add1F->arg_begin();  // Get the arg
  ArgX->setName("AnArg");            // Give it a nice symbolic name for fun.

  // Create the add instruction, inserting it into the end of BB.
  Instruction *Add = BinaryOperator::createAdd(One, ArgX, "addresult", BB);

  // Create the return instruction and add it to the basic block
  new ReturnInst(Add, BB);

  // Now, function add1 is ready.
  return Add1F;
}

Function* getNative(const std::string& name) {
  std::vector<const Type*> arglist;
  arglist.push_back( Type::Int32Ty );
  FunctionType* funcType = FunctionType::get( Type::Int32Ty, arglist, false );
  Function* func = new Function( funcType, GlobalValue::ExternalLinkage, name, NULL );

  return func;
}

void call(ExecutionEngine* EE, Function* func) {
  // Call the `foo' function with no arguments:
  std::vector<GenericValue> noargs;
  GenericValue gv = EE->runFunction(func, noargs);

  // Import result of execution:
  std::cout << "Calling " << func->getName() << ", returned " << gv.IntVal.toStringUnsigned(10) << "\n";
}

extern "C" {
  int plus5(int x) {
    return x + 5;
  }
}

int main() {
  // Create some module to put our function into it.
  Module *M = new Module("test");
  
  // Now we create the JIT.
  ExistingModuleProvider* MP = new ExistingModuleProvider(M);
  ExecutionEngine* EE = ExecutionEngine::create(MP, false);

  Function* nativePlus5 = getNative("plus5");
  Function* callNative = createFoo("foo", M, nativePlus5);
  
  Function* jittedAdd1 = createAdd(M);
  Function* callJitted = createFoo("fooJit", M, jittedAdd1);

  std::cout
    << "--- We just constructed this LLVM module ---\n\n"
    << *M << "\n"
    << "--------------------------------------------\n\n";

  call(EE, callNative);
  call(EE, callJitted);

  return 0;
}


