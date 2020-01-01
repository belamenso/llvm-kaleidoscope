#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    tok_identifier = -4, //-> IdentifierStr
    tok_number = -5, //-> NumVal
};

static std::string IdentifierStr;
static double NumVal;

static int gettok() {
    static int LastChar = ' ';
    while (isspace(LastChar))
        LastChar = getchar();
    
    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        return tok_identifier;
    }

    if (isdigit(LastChar) || LastChar == '.') {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number; // TODO: 1.2.3
    }

    if (LastChar == '#') {
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) return gettok();
    }

    if (LastChar == EOF) return tok_eof;

    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

class ExprAST {
public:
    virtual ~ExprAST() {}
    virtual Value* codegen() = 0;
};

class NumberExprAST: public ExprAST {
    double Val;

public:
    NumberExprAST(double v): Val(v) {}
    virtual Value* codegen();
};

class VariableExprAST: public ExprAST {
    std::string Name;

public:
    VariableExprAST(const std::string &n): Name(n) {}
    virtual Value* codegen();
};

class BinaryExprAST: public ExprAST {
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;
public:
    BinaryExprAST(char op,
            std::unique_ptr<ExprAST> LHS,
            std::unique_ptr<ExprAST> RHS)
        : Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    virtual Value* codegen();
};

class CallExprAST: public ExprAST {
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;

public:
    CallExprAST(const std::string &Callee,
            std::vector<std::unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
    virtual Value* codegen();
};

class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &name, std::vector<std::string> Args)
        : Name(name), Args(std::move(Args)) {}
    virtual Function* codegen();
    std::string& getName() { return Name; }
};

class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
            std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
    virtual Function* codegen();
};

// In this language, all values are doubles, so no need for types,
// however, you usually want ExprAST to have a type.

static int CurTok; // current token that needs to be parsed

static int getNextToken() {
    return CurTok = gettok();
}

// Easy error handling, always return null

std::unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "LogError: %s\n", Str);
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
    LogErrorP(Str);
    return nullptr;
}

// productions
//   the routine eats all of the tokens that correspond to the
//   production and returns the lexer buffer with the next token

static std::unique_ptr<ExprAST> ParseExpression();

static std::unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken();
    return std::move(Result);
}

static std::unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken(); // (
    auto V = ParseExpression();
    if (!V) return nullptr; // we return null on error
    if (CurTok != ')') return LogError("expected ')'"); // this is how we use error routines
    getNextToken(); // )
    return V;
}

static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;

    getNextToken(); // eat the identifier

    if (CurTok != '(') // variable ref
        return std::make_unique<VariableExprAST>(IdName);

    // Call
    getNextToken(); // (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')') {
        while (true) {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (CurTok == ')') break;
            if (CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }
    getNextToken(); // )

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
//      ::= identifierexpr
//      ::= numberexpr
//      ::= parnexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
    default: return LogError("Unknown token when expecting an expression"); // wow, you can write defualt here
    case tok_identifier: return ParseIdentifierExpr();
    case tok_number: return ParseNumberExpr();
    case '(': return ParseParenExpr();
    }
}

// operator-precedence parsing
static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
    if (!isascii(CurTok)) return -1; // XXX ?

    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) return -1;
    return TokPrec;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(
        int ExprPrec, // XXX
        std::unique_ptr<ExprAST> LHS) {

    while (true) {
        int TokPrec = GetTokPrecedence();

        if (TokPrec < ExprPrec) // XXX Invalid tokens have precedence of -1
            return LHS;

        int BinOp = CurTok;
        getNextToken(); // eat binop

        auto RHS = ParsePrimary(); // rhs after the binop
        if (!RHS) return nullptr;

        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec+1, std::move(RHS)); // XXX!!!
            if (!RHS) return nullptr;
        }

        LHS = std::make_unique<BinaryExprAST>(BinOp,
                std::move(LHS),
                std::move(RHS));
    }
}

static std::unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (!LHS) return nullptr;
    return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype");

    std::string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");
    // XXX why not eat ( here?

    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    getNextToken(); // eat )
    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

static std::unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken();
    auto Proto = ParsePrototype();
    if (!Proto) return nullptr;

    if (auto E = ParseExpression())
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

static std::unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken(); // 'extern'
    return ParsePrototype();
}

static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
    if (auto E = ParseExpression()) {
        auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

static void HandleDefinition() {
    if (auto FnAST = ParseDefinition()) {
        if (auto *FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read function definition:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (auto ProtoAST = ParseExtern()) {
        if (auto *FnIR = ProtoAST->codegen()) {
            fprintf(stderr, "Read extern: ");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr()) {
        if (auto *FnIR = FnAST->codegen()) {
            fprintf(stderr, "Read top-level expression:");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void MainLoop() {
    while (true) {
        fprintf(stderr, "ready> ");
        switch (CurTok) {
        case tok_eof:
            return;
        case ';':
            // ignore top-level semicolons, so that the parser knows
            //  the expression has ended
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

static LLVMContext TheContext; // a lot of LLVM core data structures are owned in here
// helper object, knows where to insert expressions, you just tell it which one
static IRBuilder<> Builder(TheContext);
// modules contain functions and global variables, they own memory, so we return Value* everywhere
static std::unique_ptr<Module> TheModule;
// keeps track of which values were defined in the current scope (symbol table)
// function parameters will be here during codegen of function body.
static std::map<std::string, Value*> NamedValues;

Value* LogErrorV(const char *Str) {
    LogError(Str);
    return nullptr;
}

Value* NumberExprAST::codegen() {
    // Arbitrary Precision FP
    // ::get(), not new, since it's unique'd and shared all over LLVM IR
    return ConstantFP::get(TheContext, APFloat(Val));
}

Value* VariableExprAST::codegen() {
    Value *V = NamedValues[Name];
    if (!V) return LogErrorV("Unknown variable name");
    return V;
}

Value* BinaryExprAST::codegen() {
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();

    if (!L || !R) return nullptr;

    switch (Op) {
    case '+': return Builder.CreateFAdd(L, R, "addtmp"); // these names make reading IR dumps easier
    case '-': return Builder.CreateFSub(L, R, "subtmp");
    case '*': return Builder.CreateFMul(L, R, "multmp");
    case '<':
          L = Builder.CreateFCmpULT(L, R, "cmptmp");
          // now 0.0 or 1.0, or sitofp --> 0.0 or -1.0
          return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext), "booltmp");
    default: return LogErrorV("Invalid binary operator");
    }
}

Value* CallExprAST::codegen() {
    // function name lookup in the module's symbol table
    Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF) return LogErrorV("Unknown function referenced");

    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # of arguments passed");

    // recursively codegen every argument passed in
    std::vector<Value*> ArgsV;
    for (unsigned i = 0; i < Args.size(); i++) {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back()) // XXX?
            return nullptr;
    }

    // create a call instruction, C calling convention by default
    return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Function* PrototypeAST::codegen() {
    std::vector<Type*> Doubles(
            Args.size(), Type::getDoubleTy(TheContext));
    // types are uniques like constants are, you never do 'new', always 'get'
    FunctionType *FT = // false means "not vararg"
        FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);

    // user-specified name for later lookup, functions exist in modules's symbol table
    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    // 1. IR more readable
    // 2. subsequent code can refer to arguments direcrtly (?) insead of searching through
    // Prototype AST (?).
    unsigned Idx = 0;
    for (auto &Arg: F->args())
        Arg.setName(Args[Idx++]);

    // function with no body. This is how you represent declarations.
    // for external code (like sin, atan, this is enough).

    return F;
}

Function* FunctionAST::codegen() {
    Function *TheFunction = TheModule->getFunction(Proto->getName());
    if (!TheFunction)
        TheFunction = Proto->codegen();
    if (!TheFunction)
        return nullptr;
    if (!TheFunction->empty())
        return (Function*)LogErrorV("Function cannot be redefined");

    BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    NamedValues.clear();
    for (auto &Arg: TheFunction->args())
        NamedValues[Arg.getName()] = &Arg; // XXX nice

    if (Value *RetVal = Body->codegen()) {
        Builder.CreateRet(RetVal);
        verifyFunction(*TheFunction);
        return TheFunction;
    }

    // error reading body
    TheFunction->eraseFromParent();
    return nullptr;

    /* TODO:
     * extern foo(a);     # ok, defines foo.
     * def foo(b) b;      # Error: Unknown variable name. (decl using 'a' takes precedence).
     */
}

int main() {
    // install standard binary operators
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest

    fprintf(stderr, "ready> ");
    getNextToken();

    TheModule = std::make_unique<Module>("my first JIT", TheContext);

    MainLoop();

    // all the generated code
    TheModule->print(errs(), nullptr);

    return 0;
}

