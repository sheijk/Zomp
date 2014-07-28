///
/// A version of the interface that only uses calls from OCaml into C code but
/// no calls back. This would be a first steps towards having the majority of
/// the compiler in 'native' code and let the OCaml part be the legacy system.
///

struct Backend;
Backend* createBackend();
void destroyBackend();

struct Type;
struct IntType;
struct RealType;
struct ArrayType;
// ...

IntType* getIntType(Backend*, int bits);
RealType* getFloatType(Backend*);
RealType* getDoubleType(Backend*);

struct Value;
// ...

Value* createIntValue(Backend*, IntType*, int64 value);
Value* createRealValue(Backend*, RealType*, double value);
Value* createUndef(Backend*, Type*);
// ...

struct SourceLocation;
SourceLocation createSourceLocation(Backend*, const char* file, int line, int column);

struct Variable;
Variable* createGlobalVariable(Backend*, Type*, const char* name, Value* initial, SourceLocation*);

void createTypedef(Backend*, Type*, const char* name, SourceLocation*);

struct Function;
Function* createFunction(Backend*, Type*, const char* name, SourceLocation*);

struct BasicBlock;
BasicBlock* createBasicBlock(Function*, const char* name, SourceLocation*);
void setEmitTarget(Function*, BasicBlock*);

void addLocalVariable(Function*, Type*, const char* name, SourceLocation*);

struct Expression;
Expression* emitConstant(Function*, Value*, SourceLocation*);
void emitAssignVar(Function*, Variable*, Expression*, SourceLocation*);

void emitJump(Function*, BasicBlock*, SourceLocation*);
void emitBranch(Function*, Expression*, BasicBlock* onTrue, BasicBlock* onFalse, SourceLocation*);

// ...

