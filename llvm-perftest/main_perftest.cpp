/**
 * Program to test the performance of llvm calling JITted functions
 */

#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Instructions.h"
#include "llvm/ModuleProvider.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/ExecutionEngine/GenericValue.h"

#include <iostream>
#include "time.h"

using std::cout;
using std::endl;
using namespace llvm;

// define i32 @add1(i32 %AnArg) {
// EntryBlock:
// 	%addresult = add i32 1, %AnArg		; <i32> [#uses=1]
// 	ret i32 %addresult
// }

int nativeAdd1(int x) {
    return x + 1;
}

int nativeFoo() {
    return nativeAdd1(10);
}

class Application
{
private:
    Module* module_;
    Function* add1_;
    Function* foo_;
    ExistingModuleProvider* moduleProvider_;
    ExecutionEngine* executionEngine_;

    Function* buildAdd1(const std::string& name) {
        Function *Add1F =
            cast<Function>(module_->getOrInsertFunction(
                    "add1",
                    Type::Int32Ty,
                    Type::Int32Ty,
                    (Type *)0));

        BasicBlock *BB = new BasicBlock("EntryBlock", Add1F);

        Value *One = ConstantInt::get(Type::Int32Ty, 1);

        assert(Add1F->arg_begin() != Add1F->arg_end()); // Make sure there's an arg
        Argument *ArgX = Add1F->arg_begin();  // Get the arg
        ArgX->setName("AnArg");            // Give it a nice symbolic name for fun.

        Instruction *Add = BinaryOperator::createAdd(One, ArgX, "addresult", BB);

        new ReturnInst(Add, BB);

        return Add1F;
    }

    Function* buildFoo() {
        Function *FooF =
            cast<Function>(module_->getOrInsertFunction("foo", Type::Int32Ty, (Type *)0));

        BasicBlock* BB = new BasicBlock("EntryBlock", FooF);

        Value *Ten = ConstantInt::get(Type::Int32Ty, 10);

        CallInst *Add1CallRes = new CallInst(add1_, Ten, "add1", BB);
        Add1CallRes->setTailCall(true);

        new ReturnInst(Add1CallRes, BB);

        return FooF;
    }

public:
    Application() {
    }

    virtual ~Application() {
    }

    int run() {
        module_ = new Module("LLVMPerfTest");
        add1_ = buildAdd1("add1");
        foo_ = buildFoo();
        moduleProvider_ = new ExistingModuleProvider(module_);
        executionEngine_ = ExecutionEngine::create(moduleProvider_, false);

        int sum = 0;

        const int64_t count = 50000000LL;
        // const int64_t count = 10;

        std::vector<GenericValue> noargs;

        // GenericValue argument;
        // // add signed 32 bit int = 10
        // argument.IntVal = APInt(32, 10, true);
        // noargs.push_back(argument);

        int (*jitFoo)() = (int (*)())executionEngine_->getPointerToFunction(foo_);
        assert( jitFoo != NULL );

        for( int64_t i = 0; i < count; ++i ) {
            // GenericValue result = executionEngine_->runFunction(add1_, noargs);
            // sum += (int)result.IntVal.getLimitedValue();

            // GenericValue result = executionEngine_->runFunction(foo_, noargs);
            // sum += (int)result.IntVal.getLimitedValue();

            sum += nativeFoo();

            // sum += jitFoo();
        }

        std::cout << " sum = " << sum << std::endl;

        return 0;
    }
};

int main(int argc, char const *argv)
{
    std::cout << "Testing performance of method calls" << std::endl;

    Application app;
    return app.run();
}

/*/

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

int main() {
    // Create some module to put our function into it.
    Module *M = new Module("test");

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


    // Now we going to create function `foo', which returns an int and takes no
    // arguments.
    Function *FooF =
        cast<Function>(M->getOrInsertFunction("foo", Type::Int32Ty, (Type *)0));

    // Add a basic block to the FooF function.
    BB = new BasicBlock("EntryBlock", FooF);

    // Get pointers to the constant `10'.
    Value *Ten = ConstantInt::get(Type::Int32Ty, 10);

    // Pass Ten to the call call:
    CallInst *Add1CallRes = new CallInst(Add1F, Ten, "add1", BB);
    Add1CallRes->setTailCall(true);

    // Create the return instruction and add it to the basic block.
    new ReturnInst(Add1CallRes, BB);

    // Now we create the JIT.
    ExistingModuleProvider* MP = new ExistingModuleProvider(M);
    ExecutionEngine* EE = ExecutionEngine::create(MP, false);

    std::cout << "We just constructed this LLVM module:\n\n" << *M;
    std::cout << "\n\nRunning foo: " << std::flush;

    // Call the `foo' function with no arguments:
    std::vector<GenericValue> noargs;
    GenericValue gv = EE->runFunction(FooF, noargs);

    // Import result of execution:
    std::cout << "Result: " << gv.IntVal.toStringUnsigned(10) << "\n";
    return 0;
}

/**/

