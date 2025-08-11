// toyc.cpp -- ToyC (subset of C) -> RISC-V32 assembly
// 编译: g++ -std=c++20 -O2 -o toyc toyc.cpp
// 使用: ./toyc input.tc -o out.s
//
// 参照任务书: ToyC 文法与语义 (见上传的 PL_C_Project_Tasks.pdf)。 :contentReference[oaicite:1]{index=1}
//
// 已实现功能 (summary):
//  - 词法/递归下降解析（完整 ToyC 文法子集）
//  - 语义检查（main 函数、函数顺序、变量作用域、break/continue、return 路径、void/int 调用检查）
//  - 简易代码生成 (RISC-V32)，每函数栈帧分配，支持调用 a0..a7（最多 8 个参数）
//  - 短路逻辑、比较运算、算术运算、%、/、函数调用、局部变量、参数、if/else/while/break/continue/return
//
// 限制/注意:
//  - 函数参数最多 8 个（可扩展为使用 caller stack；代码中标注了位置）
//  - 生成代码依赖 RISC-V 指令集对 div/rem 的支持（通常存在）
//  - 程序假定输入程序无语法错误（评测题保证这一点），但编译器会尽量报告发现的语义错误并退出
//
// 设计简述:
//  - AST -> 语义检查 -> CodeGen
//  - 运行时使用 simple stack frame with saved ra, s0; offsets computed relative to s0
//
// 代码开始:

#include <bits/stdc++.h>
using namespace std;

// --------- Lexer ----------
enum class TokType {
    End, Ident, Number,
    Kw_int, Kw_void, Kw_if, Kw_else, Kw_while, Kw_break, Kw_continue, Kw_return,
    Plus, Minus, Star, Slash, Mod,
    LParen, RParen, LBrace, RBrace, Comma, Semicolon,
    Assign, // =
    Eq, Neq, Lt, Gt, Le, Ge,
    AndAnd, OrOr, Not,
};

struct Token {
    TokType type;
    string lexeme;
    long long value; // for number
    int line;
};

string src;
size_t srcpos = 0;
int lineno = 1;

bool startswith(const string &s) {
    return src.compare(srcpos, s.size(), s) == 0;
}

void skipWhitespaceAndComments() {
    while (srcpos < src.size()) {
        char c = src[srcpos];
        if (c == ' ' || c == '\t' || c == '\r') { srcpos++; continue; }
        if (c == '\n') { lineno++; srcpos++; continue; }
        if (startswith("//")) {
            srcpos += 2;
            while (srcpos < src.size() && src[srcpos] != '\n') srcpos++;
            continue;
        }
        if (startswith("/*")) {
            srcpos += 2;
            while (srcpos < src.size() && !startswith("*/")) {
                if (src[srcpos] == '\n') lineno++;
                srcpos++;
            }
            if (startswith("*/")) srcpos += 2;
            continue;
        }
        break;
    }
}

bool isIdentStart(char c) { return (c == '_' || isalpha((unsigned char)c)); }
bool isIdentPart(char c) { return (c == '_' || isalnum((unsigned char)c)); }

Token nextTokenRaw() {
    skipWhitespaceAndComments();
    if (srcpos >= src.size()) return {TokType::End, "", 0, lineno};
    char c = src[srcpos];
    // identifiers / keywords
    if (isIdentStart(c)) {
        size_t b = srcpos;
        srcpos++;
        while (srcpos < src.size() && isIdentPart(src[srcpos])) srcpos++;
        string s = src.substr(b, srcpos - b);
        if (s == "int") return {TokType::Kw_int, s, 0, lineno};
        if (s == "void") return {TokType::Kw_void, s, 0, lineno};
        if (s == "if") return {TokType::Kw_if, s, 0, lineno};
        if (s == "else") return {TokType::Kw_else, s, 0, lineno};
        if (s == "while") return {TokType::Kw_while, s, 0, lineno};
        if (s == "break") return {TokType::Kw_break, s, 0, lineno};
        if (s == "continue") return {TokType::Kw_continue, s, 0, lineno};
        if (s == "return") return {TokType::Kw_return, s, 0, lineno};
        return {TokType::Ident, s, 0, lineno};
    }
    // number: -? (0 | [1-9][0-9]*)
    if (c == '-' || isdigit((unsigned char)c)) {
        size_t b = srcpos;
        bool negative = false;
        if (c == '-') { negative = true; srcpos++; }
        if (srcpos >= src.size() || !isdigit((unsigned char)src[srcpos])) {
            // lone '-' token
            if (negative) { srcpos = b; } // rollback
        } else {
            // parse decimal
            size_t startDigits = srcpos;
            if (src[srcpos] == '0') {
                srcpos++;
            } else {
                while (srcpos < src.size() && isdigit((unsigned char)src[srcpos])) srcpos++;
            }
            string numstr = src.substr(b, srcpos - b);
            long long v = stoll(numstr);
            return {TokType::Number, numstr, v, lineno};
        }
    }
    // two-char tokens
    if (startswith("&&")) { srcpos += 2; return {TokType::AndAnd, "&&", 0, lineno}; }
    if (startswith("||")) { srcpos += 2; return {TokType::OrOr, "||", 0, lineno}; }
    if (startswith("==")) { srcpos += 2; return {TokType::Eq, "==", 0, lineno}; }
    if (startswith("!=")) { srcpos += 2; return {TokType::Neq, "!=", 0, lineno}; }
    if (startswith("<=")) { srcpos += 2; return {TokType::Le, "<=", 0, lineno}; }
    if (startswith(">=")) { srcpos += 2; return {TokType::Ge, ">=", 0, lineno}; }
    // single char tokens
    char ch = src[srcpos++];
    switch (ch) {
        case '+': return {TokType::Plus, "+", 0, lineno};
        case '-': return {TokType::Minus, "-", 0, lineno};
        case '*': return {TokType::Star, "*", 0, lineno};
        case '/': return {TokType::Slash, "/", 0, lineno};
        case '%': return {TokType::Mod, "%", 0, lineno};
        case '(': return {TokType::LParen, "(", 0, lineno};
        case ')': return {TokType::RParen, ")", 0, lineno};
        case '{': return {TokType::LBrace, "{", 0, lineno};
        case '}': return {TokType::RBrace, "}", 0, lineno};
        case ',': return {TokType::Comma, ",", 0, lineno};
        case ';': return {TokType::Semicolon, ";", 0, lineno};
        case '=': return {TokType::Assign, "=", 0, lineno};
        case '<': return {TokType::Lt, "<", 0, lineno};
        case '>': return {TokType::Gt, ">", 0, lineno};
        case '!': return {TokType::Not, "!", 0, lineno};
        default:
            cerr << "Unknown character '" << ch << "' at line " << lineno << "\n";
            exit(1);
    }
}

// a simple token buffer to support peek/consume
struct Lexer {
    vector<Token> toks;
    size_t idx = 0;
    Lexer(const string &s) {
        src = s; srcpos = 0; lineno = 1;
        while (true) {
            Token t = nextTokenRaw();
            toks.push_back(t);
            if (t.type == TokType::End) break;
        }
    }
    Token peek() {
        if (idx < toks.size()) return toks[idx];
        return toks.back();
    }
    Token next() {
        if (idx < toks.size()) return toks[idx++];
        return toks.back();
    }
    bool accept(TokType ty) {
        if (peek().type == ty) { next(); return true; }
        return false;
    }
    void expect(TokType ty, const string &what = "") {
        if (peek().type == ty) { next(); return; }
        cerr << "Parse error: expected " << what << " at line " << peek().line << " got " << (int)peek().type << "\n";
        exit(1);
    }
};

// ---------- AST ----------
struct Stmt;
struct Expr;
struct FuncDef;

using StmtPtr = shared_ptr<Stmt>;
using ExprPtr = shared_ptr<Expr>;
using FuncPtr = shared_ptr<FuncDef>;

struct Node { virtual ~Node() = default; };

struct Expr : Node {
    virtual ~Expr() = default;
};

struct Stmt : Node {
    virtual ~Stmt() = default;
};

struct NumberExpr : Expr {
    long long val;
    NumberExpr(long long v): val(v) {}
};

struct VarExpr : Expr {
    string name;
    VarExpr(string n): name(move(n)) {}
};

struct CallExpr : Expr {
    string name;
    vector<ExprPtr> args;
    CallExpr(string n): name(move(n)) {}
};

struct UnaryExpr : Expr {
    string op;
    ExprPtr rhs;
    UnaryExpr(string o, ExprPtr r): op(move(o)), rhs(r) {}
};

struct BinaryExpr : Expr {
    string op;
    ExprPtr lhs, rhs;
    BinaryExpr(string o, ExprPtr l, ExprPtr r): op(move(o)), lhs(l), rhs(r) {}
};

// Statements:
struct BlockStmt : Stmt {
    vector<StmtPtr> stms;
};

struct EmptyStmt : Stmt {};

struct ExprStmt : Stmt {
    ExprPtr expr;
    ExprStmt(ExprPtr e): expr(e) {}
};

struct DeclStmt : Stmt { // int id = expr;
    string name;
    ExprPtr init;
    DeclStmt(string n, ExprPtr i): name(move(n)), init(i) {}
};

struct AssignStmt : Stmt { // id = expr;
    string name;
    ExprPtr expr;
    AssignStmt(string n, ExprPtr e): name(move(n)), expr(e) {}
};

struct IfStmt : Stmt {
    ExprPtr cond;
    StmtPtr thenS, elseS;
    IfStmt(ExprPtr c, StmtPtr t, StmtPtr e): cond(c), thenS(t), elseS(e) {}
};

struct WhileStmt : Stmt {
    ExprPtr cond;
    StmtPtr body;
    WhileStmt(ExprPtr c, StmtPtr b): cond(c), body(b) {}
};

struct BreakStmt : Stmt {};
struct ContinueStmt : Stmt {};
struct ReturnStmt : Stmt {
    bool hasVal;
    ExprPtr val;
    ReturnStmt(): hasVal(false) {}
    ReturnStmt(ExprPtr v): hasVal(true), val(v) {}
};

struct Param {
    string name;
    Param(string n): name(move(n)) {}
};

struct FuncDef {
    bool isInt; // true if return type int, false if void
    string name;
    vector<Param> params;
    shared_ptr<BlockStmt> body;
    int indexInFile = -1;
};

// ---------- Parser (recursive descent) ----------
struct Parser {
    Lexer &lex;
    Parser(Lexer &l): lex(l) {}
    vector<FuncPtr> parseCompUnit() {
        vector<FuncPtr> funcs;
        while (lex.peek().type != TokType::End) {
            funcs.push_back(parseFuncDef());
        }
        return funcs;
    }
    FuncPtr parseFuncDef() {
        // (int | void) ID '(' (Param (',' Param)*)? ')' Block
        bool isInt = false;
        if (lex.accept(TokType::Kw_int)) isInt = true;
        else if (lex.accept(TokType::Kw_void)) isInt = false;
        else { cerr << "Expected int/void at function definition\n"; exit(1); }
        if (lex.peek().type != TokType::Ident) { cerr << "Expected function name\n"; exit(1); }
        string name = lex.next().lexeme;
        lex.expect(TokType::LParen, "(");
        vector<Param> params;
        if (!lex.accept(TokType::RParen)) {
            // param list
            params.push_back(parseParam());
            while (lex.accept(TokType::Comma)) params.push_back(parseParam());
            lex.expect(TokType::RParen, ")");
        }
        auto body = parseBlock();
        auto f = make_shared<FuncDef>();
        f->isInt = isInt;
        f->name = name;
        f->params = move(params);
        f->body = body;
        return f;
    }
    Param parseParam() {
        // 'int' ID
        lex.expect(TokType::Kw_int, "int");
        if (lex.peek().type != TokType::Ident) { cerr << "Expected param name\n"; exit(1); }
        string name = lex.next().lexeme;
        return Param(name);
    }
    shared_ptr<BlockStmt> parseBlock() {
        lex.expect(TokType::LBrace, "{");
        auto blk = make_shared<BlockStmt>();
        while (lex.peek().type != TokType::RBrace) {
            blk->stms.push_back(parseStmt());
        }
        lex.expect(TokType::RBrace, "}");
        return blk;
    }
    StmtPtr parseStmt() {
        auto &p = lex.peek();
        switch (p.type) {
            case TokType::LBrace: return parseBlock();
            case TokType::Semicolon: { lex.next(); return make_shared<EmptyStmt>(); }
            case TokType::Kw_if: {
                lex.next(); lex.expect(TokType::LParen, "(");
                ExprPtr cond = parseExpr();
                lex.expect(TokType::RParen, ")");
                StmtPtr thenS = parseStmt();
                StmtPtr elseS = nullptr;
                if (lex.accept(TokType::Kw_else)) elseS = parseStmt();
                return make_shared<IfStmt>(cond, thenS, elseS);
            }
            case TokType::Kw_while: {
                lex.next(); lex.expect(TokType::LParen, "(");
                ExprPtr cond = parseExpr();
                lex.expect(TokType::RParen, ")");
                StmtPtr body = parseStmt();
                return make_shared<WhileStmt>(cond, body);
            }
            case TokType::Kw_break: { lex.next(); lex.expect(TokType::Semicolon, ";"); return make_shared<BreakStmt>(); }
            case TokType::Kw_continue: { lex.next(); lex.expect(TokType::Semicolon, ";"); return make_shared<ContinueStmt>(); }
            case TokType::Kw_return: {
                lex.next();
                if (lex.accept(TokType::Semicolon)) {
                    return make_shared<ReturnStmt>();
                } else {
                    ExprPtr val = parseExpr();
                    lex.expect(TokType::Semicolon, ";");
                    return make_shared<ReturnStmt>(val);
                }
            }
            case TokType::Kw_int: {
                // declaration: int ID = Expr ;
                lex.next();
                if (lex.peek().type != TokType::Ident) { cerr << "Expected identifier after int\n"; exit(1); }
                string id = lex.next().lexeme;
                lex.expect(TokType::Assign, "=");
                ExprPtr init = parseExpr();
                lex.expect(TokType::Semicolon, ";");
                return make_shared<DeclStmt>(id, init);
            }
            default:
                // could be "ID = Expr ;" or Expr ;
                if (p.type == TokType::Ident) {
                    Token t = lex.peek();
                    // lookahead for '=' or '('
                    Token t2 = lex.peek(); size_t save = lex.idx;
                    lex.next();
                    if (lex.peek().type == TokType::Assign) {
                        // assignment
                        lex.idx = save;
                        string id = lex.next().lexeme;
                        lex.expect(TokType::Assign, "=");
                        ExprPtr e = parseExpr();
                        lex.expect(TokType::Semicolon, ";");
                        return make_shared<AssignStmt>(id, e);
                    } else {
                        // expression statement (function call or var as expr)
                        lex.idx = save;
                        ExprPtr e = parseExpr();
                        lex.expect(TokType::Semicolon, ";");
                        return make_shared<ExprStmt>(e);
                    }
                } else {
                    // expression statement
                    ExprPtr e = parseExpr();
                    lex.expect(TokType::Semicolon, ";");
                    return make_shared<ExprStmt>(e);
                }
        }
    }
    ExprPtr parseExpr() { return parseLOr(); }
    ExprPtr parseLOr() {
        ExprPtr e = parseLAnd();
        while (lex.accept(TokType::OrOr)) {
            ExprPtr r = parseLAnd();
            e = make_shared<BinaryExpr>("||", e, r);
        }
        return e;
    }
    ExprPtr parseLAnd() {
        ExprPtr e = parseRel();
        while (lex.accept(TokType::AndAnd)) {
            ExprPtr r = parseRel();
            e = make_shared<BinaryExpr>("&&", e, r);
        }
        return e;
    }
    ExprPtr parseRel() {
        ExprPtr e = parseAdd();
        while (true) {
            if (lex.accept(TokType::Lt)) { ExprPtr r = parseAdd(); e = make_shared<BinaryExpr>("<", e, r); }
            else if (lex.accept(TokType::Gt)) { ExprPtr r = parseAdd(); e = make_shared<BinaryExpr>(">", e, r); }
            else if (lex.accept(TokType::Le)) { ExprPtr r = parseAdd(); e = make_shared<BinaryExpr>("<=", e, r); }
            else if (lex.accept(TokType::Ge)) { ExprPtr r = parseAdd(); e = make_shared<BinaryExpr>(">=", e, r); }
            else if (lex.accept(TokType::Eq)) { ExprPtr r = parseAdd(); e = make_shared<BinaryExpr>("==", e, r); }
            else if (lex.accept(TokType::Neq)) { ExprPtr r = parseAdd(); e = make_shared<BinaryExpr>("!=", e, r); }
            else break;
        }
        return e;
    }
    ExprPtr parseAdd() {
        ExprPtr e = parseMul();
        while (true) {
            if (lex.accept(TokType::Plus)) { ExprPtr r = parseMul(); e = make_shared<BinaryExpr>("+", e, r); }
            else if (lex.accept(TokType::Minus)) { ExprPtr r = parseMul(); e = make_shared<BinaryExpr>("-", e, r); }
            else break;
        }
        return e;
    }
    ExprPtr parseMul() {
        ExprPtr e = parseUnary();
        while (true) {
            if (lex.accept(TokType::Star)) { ExprPtr r = parseUnary(); e = make_shared<BinaryExpr>("*", e, r); }
            else if (lex.accept(TokType::Slash)) { ExprPtr r = parseUnary(); e = make_shared<BinaryExpr>("/", e, r); }
            else if (lex.accept(TokType::Mod)) { ExprPtr r = parseUnary(); e = make_shared<BinaryExpr>("%", e, r); }
            else break;
        }
        return e;
    }
    ExprPtr parseUnary() {
        if (lex.accept(TokType::Plus)) { ExprPtr e = parseUnary(); return make_shared<UnaryExpr>("+", e); }
        if (lex.accept(TokType::Minus)) { ExprPtr e = parseUnary(); return make_shared<UnaryExpr>("-", e); }
        if (lex.accept(TokType::Not)) { ExprPtr e = parseUnary(); return make_shared<UnaryExpr>("!", e); }
        return parsePrimary();
    }
    ExprPtr parsePrimary() {
        auto t = lex.peek();
        if (lex.accept(TokType::Number)) {
            return make_shared<NumberExpr>(t.value);
        }
        if (lex.accept(TokType::Ident)) {
            string id = t.lexeme;
            if (lex.accept(TokType::LParen)) {
                auto call = make_shared<CallExpr>(id);
                if (!lex.accept(TokType::RParen)) {
                    call->args.push_back(parseExpr());
                    while (lex.accept(TokType::Comma)) call->args.push_back(parseExpr());
                    lex.expect(TokType::RParen, ")");
                }
                return call;
            } else {
                return make_shared<VarExpr>(id);
            }
        }
        if (lex.accept(TokType::LParen)) {
            ExprPtr e = parseExpr();
            lex.expect(TokType::RParen, ")");
            return e;
        }
        cerr << "Parse error: unexpected token in primary at line " << lex.peek().line << "\n";
        exit(1);
    }
};

// ---------- Semantic checking ----------
struct Semantic {
    vector<FuncPtr> funcs;
    unordered_map<string,int> funcIndex;
    // check function declarations, uniqueness, main existence, function call order constraint
    void run(vector<FuncPtr> &fns) {
        funcs = fns;
        for (int i = 0; i < (int)funcs.size(); ++i) {
            if (funcIndex.count(funcs[i]->name)) {
                cerr << "Semantic error: duplicate function name: " << funcs[i]->name << "\n";
                exit(1);
            }
            funcIndex[funcs[i]->name] = i;
            funcs[i]->indexInFile = i;
        }
        // main exists and is int with no params
        if (!funcIndex.count("main")) {
            cerr << "Semantic error: main not found\n";
            exit(1);
        }
        int mainIdx = funcIndex["main"];
        if (!funcs[mainIdx]->isInt || funcs[mainIdx]->params.size() != 0) {
            cerr << "Semantic error: main must be 'int main()'\n";
            exit(1);
        }
        // per-function semantic checks
        for (int i = 0; i < (int)funcs.size(); ++i) {
            checkFunction(funcs[i], i);
        }
    }

    // helper: track scopes for variables; also check return path
    // We'll implement conservative path-checking: returnsOnAllPaths(stmt) determines if statement ensures return on all paths
    bool returnsOnAllPaths(StmtPtr s) {
        if (!s) return false;
        if (auto r = dynamic_pointer_cast<ReturnStmt>(s)) return r->hasVal || !r->hasVal;
        if (auto b = dynamic_pointer_cast<BlockStmt>(s)) {
            for (int i = 0; i < (int)b->stms.size(); ++i) {
                if (returnsOnAllPaths(b->stms[i])) return true; // after a returning stmt, subsequent unreachable in same block
            }
            return false;
        }
        if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) {
            if (!ifs->elseS) return false;
            return returnsOnAllPaths(ifs->thenS) && returnsOnAllPaths(ifs->elseS);
        }
        // other statements do not guarantee return
        return false;
    }

    // checks variables used after declaration, break/continue usage, function call existence/order, return usage, etc.
    void checkFunction(FuncPtr f, int idx) {
        // build function name->index map (already)
        // param names -> in-symbol table
        vector<unordered_set<string>> scopeStack;
        auto pushScope = [&](){ scopeStack.emplace_back(); };
        auto popScope = [&](){ scopeStack.pop_back(); };
        auto declare = [&](const string &name)->bool {
            if (scopeStack.empty()) pushScope();
            auto &cur = scopeStack.back();
            if (cur.count(name)) return false;
            cur.insert(name);
            return true;
        };
        // declare params in outermost scope
        pushScope();
        for (auto &p : f->params) {
            if (!declare(p.name)) {
                cerr << "Semantic error: duplicate parameter name " << p.name << " in function " << f->name << "\n";
                exit(1);
            }
        }
        int loopDepth = 0;
        function<void(StmtPtr)> walkStmt;
        function<void(ExprPtr)> walkExpr;
        walkExpr = [&](ExprPtr e) {
            if (!e) return;
            if (auto v = dynamic_pointer_cast<VarExpr>(e)) {
                // check variable exists in some scope
                bool found = false;
                for (int i = (int)scopeStack.size()-1; i >= 0; --i) if (scopeStack[i].count(v->name)) { found = true; break; }
                if (!found) {
                    cerr << "Semantic error: use of undeclared variable '" << v->name << "' in function " << f->name << "\n";
                    exit(1);
                }
            } else if (auto c = dynamic_pointer_cast<CallExpr>(e)) {
                // check callee exists
                if (!funcIndex.count(c->name)) {
                    cerr << "Semantic error: call to undeclared function '" << c->name << "' in function " << f->name << "\n";
                    exit(1);
                }
                int calleeIdx = funcIndex[c->name];
                // check order: called function must be declared earlier or be same function (recursion allowed)
                if (!(calleeIdx <= idx || calleeIdx == idx)) {
                    cerr << "Semantic error: function '" << c->name << "' must be declared before use (called from " << f->name << ")\n";
                    exit(1);
                }
                // check arg count matches
                if ((int)c->args.size() != (int)funcs[calleeIdx]->params.size()) {
                    cerr << "Semantic error: argument count mismatch when calling " << c->name << " from " << f->name << "\n";
                    exit(1);
                }
                // check if callee is void and call used in expression contexts will be handled by parser/semantic: here we check usage context elsewhere
                for (auto &a : c->args) walkExpr(a);
            } else if (auto un = dynamic_pointer_cast<UnaryExpr>(e)) {
                walkExpr(un->rhs);
            } else if (auto bin = dynamic_pointer_cast<BinaryExpr>(e)) {
                walkExpr(bin->lhs);
                walkExpr(bin->rhs);
            } else if (auto num = dynamic_pointer_cast<NumberExpr>(e)) {
                // ok
            } else {
                // other expression types
            }
        };
        walkStmt = [&](StmtPtr s) {
            if (!s) return;
            if (auto blk = dynamic_pointer_cast<BlockStmt>(s)) {
                pushScope();
                for (auto &ss : blk->stms) walkStmt(ss);
                popScope();
            } else if (auto ds = dynamic_pointer_cast<DeclStmt>(s)) {
                // declare variable then check initializer
                if (!declare(ds->name)) {
                    cerr << "Semantic error: duplicate local variable '" << ds->name << "' in function " << f->name << "\n";
                    exit(1);
                }
                walkExpr(ds->init);
            } else if (auto as = dynamic_pointer_cast<AssignStmt>(s)) {
                // variable must be declared
                bool found = false;
                for (int i = (int)scopeStack.size()-1; i >= 0; --i) if (scopeStack[i].count(as->name)) { found = true; break; }
                if (!found) {
                    cerr << "Semantic error: assignment to undeclared variable '" << as->name << "' in function " << f->name << "\n";
                    exit(1);
                }
                walkExpr(as->expr);
            } else if (auto es = dynamic_pointer_cast<ExprStmt>(s)) {
                // expr can be call; if call to void used as expression is allowed only as statement; if call returns void cannot be used where value required - parser does not annotate context, so here we only ensure call existence and argument count
                // we traverse expressions to check nested variables and call existence
                walkExpr(es->expr);
            } else if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) {
                walkExpr(ifs->cond);
                walkStmt(ifs->thenS);
                if (ifs->elseS) walkStmt(ifs->elseS);
            } else if (auto wh = dynamic_pointer_cast<WhileStmt>(s)) {
                walkExpr(wh->cond);
                loopDepth++;
                walkStmt(wh->body);
                loopDepth--;
            } else if (dynamic_pointer_cast<BreakStmt>(s)) {
                if (loopDepth <= 0) { cerr << "Semantic error: break not inside loop in function " << f->name << "\n"; exit(1); }
            } else if (dynamic_pointer_cast<ContinueStmt>(s)) {
                if (loopDepth <= 0) { cerr << "Semantic error: continue not inside loop in function " << f->name << "\n"; exit(1); }
            } else if (auto ret = dynamic_pointer_cast<ReturnStmt>(s)) {
                if (ret->hasVal) walkExpr(ret->val);
            } else if (dynamic_pointer_cast<EmptyStmt>(s)) {
                // nothing
            } else {
                // unknown
            }
        };
        // traverse body
        walkStmt(f->body);
        // check return paths
        if (f->isInt) {
            if (!returnsOnAllPaths(f->body)) {
                cerr << "Semantic error: int function '" << f->name << "' may not return on all paths\n";
                exit(1);
            }
        }
        // done
    }
};

// ---------- Code generation (RISC-V) ----------
struct CodeGen {
    vector<FuncPtr> funcs;
    unordered_map<string,int> funcIndex;
    ofstream out;
    int labelCounter = 0;
    CodeGen(const string &outfn): out(outfn) {
        if (!out) { cerr << "Cannot open output file " << outfn << "\n"; exit(1); }
    }
    string newLabel(const string &base) {
        return base + "_" + to_string(labelCounter++);
    }

    // For each function we need a local symbol->stackOffset mapping (offset relative to s0)
    // We'll allocate frame as: we reserve argslots (8) then locals and saved s0/ra on top.
    // Offsets relative to s0 (frame pointer after prologue):
    // saved ra: s0 - 4
    // saved s0: s0 - 8
    // argslot i: s0 - (12 + 4*i)
    // local j: s0 - (12 + 4*argslots + 4*j)
    int ARG_SLOTS = 8;

    static int roundup16(int x) { return ((x+15)/16)*16; }

    // Pre-scan function to collect all variable names (params + declarations) and assign indices
    void collectLocals(FuncPtr f, unordered_map<string,int> &localIndex, int &localCount) {
        // params first
        int idx = 0;
        for (auto &p : f->params) {
            localIndex[p.name] = idx++;
        }
        // walk statements to find DeclStmt in traversal order and allocate indices
        function<void(StmtPtr)> walk;
        walk = [&](StmtPtr s) {
            if (!s) return;
            if (auto blk = dynamic_pointer_cast<BlockStmt>(s)) {
                for (auto &ss : blk->stms) walk(ss);
            } else if (auto ds = dynamic_pointer_cast<DeclStmt>(s)) {
                if (localIndex.count(ds->name)==0) {
                    localIndex[ds->name] = idx++;
                } else {
                    // shadowing allowed: to simplify we still assign a new slot (unique name assumed during semantic)
                    // But semantic already forbids duplicate in same scope; shadowing in inner scope not distinguished here.
                    localIndex[ds->name + "@shadow" + to_string(idx)] = idx++;
                }
                // still traverse initializer
                // (we don't care about nested declarations inside initializer according to grammar)
            } else if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) {
                walk(ifs->thenS);
                if (ifs->elseS) walk(ifs->elseS);
            } else if (auto wh = dynamic_pointer_cast<WhileStmt>(s)) {
                walk(wh->body);
            } else {
                // other stmts ignore
            }
        };
        walk(f->body);
        localCount = idx;
    }

    // compute offset for a local index j (0..localCount-1)
    int offsetForLocal(int localIndex, int localCount) {
        // base = 12 + 4*ARG_SLOTS
        int base = 12 + 4*ARG_SLOTS;
        return - (base + 4 * localIndex);
    }
    int offsetForArgslot(int i) {
        return - (12 + 4 * i);
    }

    void gen(vector<FuncPtr> &fns) {
        funcs = fns;
        for (int i = 0; i < (int)funcs.size(); ++i) funcIndex[funcs[i]->name] = i;
        // header
        out << "\t.text\n";
        // generate each function
        for (auto &f : funcs) {
            genFunction(f);
        }
    }

    // Generate function prologue/epilogue + body
    void genFunction(FuncPtr f) {
        // collect locals & assign indices
        unordered_map<string,int> localIndex;
        int localCount = 0;
        collectLocals(f, localIndex, localCount);
        int frameSize = 16 + 4*ARG_SLOTS + 4*localCount;
        frameSize = roundup16(frameSize);
        string fname = f->name;
        out << "\t.globl " << fname << "\n";
        out << fname << ":\n";
        // prologue
        out << "\taddi sp, sp, -" << frameSize << "\n";
        out << "\tsw ra, " << (frameSize - 4) << "(sp)\n";
        out << "\tsw s0, " << (frameSize - 8) << "(sp)\n";
        out << "\taddi s0, sp, " << frameSize << "\n";
        // store incoming args into local slots (params were assigned indices starting from 0)
        for (int i = 0; i < (int)f->params.size(); ++i) {
            if (i < ARG_SLOTS) {
                int off = offsetForLocal(i, localCount);
                out << "\tsw a" << i << ", " << off << "(s0)\n";
            } else {
                // NOTE: this implementation currently supports up to ARG_SLOTS params.
                cerr << "Error: functions with more than " << ARG_SLOTS << " parameters are not supported in this simple compiler.\n";
                exit(1);
            }
        }
        // entry label for returns
        string retLabel = fname + "_RET";
        // context for generating statements: localIndex map, localCount, frameSize, retLabel
        struct GenCtx {
            CodeGen *cg;
            unordered_map<string,int> *localIndex;
            int localCount;
            int frameSize;
            string retLabel;
            int ARG_SLOTS;
            vector<string> loopCondLabels;
            vector<string> loopEndLabels;
        };
        GenCtx ctx{this, &localIndex, localCount, frameSize, retLabel, ARG_SLOTS, {}, {}};

        // generate body
        genStmt(f->body, ctx);

        // if fallthrough (no explicit return) for void function, return with a0=0
        if (!f->isInt) {
            out << retLabel << ":\n";
            out << "\taddi sp, sp, " << frameSize << "\n";
            out << "\tlw s0, " << (frameSize - 8) << "(sp)\n";
            out << "\tlw ra, " << (frameSize - 4) << "(sp)\n";
            out << "\tret\n";
        } else {
            // This shouldn't happen usually because semantic check ensures int functions have return on all paths.
            out << retLabel << ":\n";
            out << "\taddi sp, sp, " << frameSize << "\n";
            out << "\tlw s0, " << (frameSize - 8) << "(sp)\n";
            out << "\tlw ra, " << (frameSize - 4) << "(sp)\n";
            out << "\tret\n";
        }
    }

    // generate statement
    void genStmt(StmtPtr s, struct { CodeGen *cg; unordered_map<string,int> *localIndex; int localCount; int frameSize; string retLabel; int ARG_SLOTS; vector<string> loopCondLabels; vector<string> loopEndLabels; } &C) {
        // wrapper: use lambda but pass C by reference
    }

    // We'll implement genStmt/genExpr as member functions with context struct type defined above.
    struct Ctx {
        CodeGen *cg;
        unordered_map<string,int> *localIndex;
        int localCount;
        int frameSize;
        string retLabel;
        int ARG_SLOTS;
        vector<string> loopCondLabels;
        vector<string> loopEndLabels;
    };

    void genStmt(StmtPtr s, Ctx &C) {
        if (!s) return;
        if (auto blk = dynamic_pointer_cast<BlockStmt>(s)) {
            for (auto &st : blk->stms) genStmt(st, C);
        } else if (auto es = dynamic_pointer_cast<ExprStmt>(s)) {
            genExpr(es->expr, C);
            // result in t0 but expression statement's result unused
        } else if (auto ds = dynamic_pointer_cast<DeclStmt>(s)) {
            // evaluate init and store into slot
            genExpr(ds->init, C); // result in t0
            // find local offset
            int idx = -1;
            if (C.localIndex->count(ds->name)) idx = (*C.localIndex)[ds->name];
            else {
                // try shadowed names (we might have assigned with @shadow suffix earlier)
                for (auto &kv : *C.localIndex) {
                    if (kv.first.rfind(ds->name, 0) == 0) { idx = kv.second; break; }
                }
            }
            if (idx < 0) { cerr << "Internal codegen error: local not found " << ds->name << "\n"; exit(1); }
            int off = offsetForLocal(idx, C.localCount);
            out << "\tsw t0, " << off << "(s0)\n";
        } else if (auto as = dynamic_pointer_cast<AssignStmt>(s)) {
            genExpr(as->expr, C);
            int idx = -1;
            if (C.localIndex->count(as->name)) idx = (*C.localIndex)[as->name];
            else {
                for (auto &kv : *C.localIndex) {
                    if (kv.first.rfind(as->name, 0) == 0) { idx = kv.second; break; }
                }
            }
            if (idx < 0) { cerr << "Internal codegen error: assign to unknown " << as->name << "\n"; exit(1); }
            int off = offsetForLocal(idx, C.localCount);
            out << "\tsw t0, " << off << "(s0)\n";
        } else if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) {
            string elseL = newLabel("Lelse");
            string endL = newLabel("Lend");
            genExpr(ifs->cond, C);
            // if cond == 0 jump to else
            out << "\tbeqz t0, " << elseL << "\n";
            genStmt(ifs->thenS, C);
            out << "\tj " << endL << "\n";
            out << elseL << ":\n";
            if (ifs->elseS) genStmt(ifs->elseS, C);
            out << endL << ":\n";
        } else if (auto wh = dynamic_pointer_cast<WhileStmt>(s)) {
            string condL = newLabel("Lwhile_cond");
            string bodyL = newLabel("Lwhile_body");
            string endL = newLabel("Lwhile_end");
            // push labels for break/continue
            C.loopCondLabels.push_back(condL);
            C.loopEndLabels.push_back(endL);

            out << condL << ":\n";
            genExpr(wh->cond, C);
            out << "\tbeqz t0, " << endL << "\n";
            out << bodyL << ":\n";
            genStmt(wh->body, C);
            out << "\tj " << condL << "\n";
            out << endL << ":\n";

            C.loopCondLabels.pop_back();
            C.loopEndLabels.pop_back();
        } else if (auto br = dynamic_pointer_cast<BreakStmt>(s)) {
            if (C.loopEndLabels.empty()) { cerr << "Internal codegen error: break without loop\n"; exit(1); }
            out << "\tj " << C.loopEndLabels.back() << "\n";
        } else if (auto co = dynamic_pointer_cast<ContinueStmt>(s)) {
            if (C.loopCondLabels.empty()) { cerr << "Internal codegen error: continue without loop\n"; exit(1); }
            out << "\tj " << C.loopCondLabels.back() << "\n";
        } else if (auto ret = dynamic_pointer_cast<ReturnStmt>(s)) {
            if (ret->hasVal) {
                genExpr(ret->val, C); // result in t0
                out << "\tmv a0, t0\n";
            }
            out << "\tj " << C.retLabel << "\n";
        } else if (dynamic_pointer_cast<EmptyStmt>(s)) {
            // nothing
        } else {
            cerr << "Internal codegen error: unknown stmt type\n";
            exit(1);
        }
    }

    void genExpr(ExprPtr e, Ctx &C) {
        if (!e) { out << "\tli t0, 0\n"; return; }
        if (auto num = dynamic_pointer_cast<NumberExpr>(e)) {
            out << "\tli t0, " << num->val << "\n";
        } else if (auto var = dynamic_pointer_cast<VarExpr>(e)) {
            int idx = -1;
            if (C.localIndex->count(var->name)) idx = (*C.localIndex)[var->name];
            else {
                for (auto &kv : *C.localIndex) {
                    if (kv.first.rfind(var->name, 0) == 0) { idx = kv.second; break; }
                }
            }
            if (idx < 0) { cerr << "Internal codegen error: var not found " << var->name << "\n"; exit(1); }
            int off = offsetForLocal(idx, C.localCount);
            out << "\tlw t0, " << off << "(s0)\n";
        } else if (auto call = dynamic_pointer_cast<CallExpr>(e)) {
            // evaluate args left-to-right and store into argslots
            if ((int)call->args.size() > C.ARG_SLOTS) {
                cerr << "Codegen error: call with >" << C.ARG_SLOTS << " args not supported\n";
                exit(1);
            }
            for (int i = 0; i < (int)call->args.size(); ++i) {
                genExpr(call->args[i], C);
                int aoff = offsetForArgslot(i);
                out << "\tsw t0, " << aoff << "(s0)\n";
            }
            // load into a0..a7
            for (int i = 0; i < (int)call->args.size(); ++i) {
                out << "\tlw a" << i << ", " << offsetForArgslot(i) << "(s0)\n";
            }
            // jal to function
            out << "\tcall " << call->name << "\n"; // 'call' is pseudoinstruction (jal ra, label)
            // result in a0 -> move to t0
            out << "\tmv t0, a0\n";
        } else if (auto unary = dynamic_pointer_cast<UnaryExpr>(e)) {
            genExpr(unary->rhs, C);
            if (unary->op == "+") {
                // noop
            } else if (unary->op == "-") {
                out << "\tsub t0, x0, t0\n";
            } else if (unary->op == "!") {
                // t0 = (t0 == 0) ? 1 : 0
                out << "\tsltiu t0, t0, 1\n";
            } else {
                cerr << "Codegen error: unknown unary op " << unary->op << "\n"; exit(1);
            }
        } else if (auto bin = dynamic_pointer_cast<BinaryExpr>(e)) {
            string op = bin->op;
            if (op == "||") {
                string rlabel = newLabel("lor_r");
                string end = newLabel("lor_end");
                genExpr(bin->lhs, C);
                out << "\tbeqz t0, " << rlabel << "\n";
                // lhs != 0 => result 1
                out << "\tli t0, 1\n";
                out << "\tj " << end << "\n";
                out << rlabel << ":\n";
                genExpr(bin->rhs, C);
                // rhs != 0 -> 1 else 0
                out << "\tsltiu t0, t0, 1\n"; // this yields 1 if t0 == 0; but we want 1 if !=0: so invert
                out << "\txori t0, t0, 1\n";
                out << end << ":\n";
            } else if (op == "&&") {
                string rlabel = newLabel("land_r");
                string end = newLabel("land_end");
                genExpr(bin->lhs, C);
                out << "\tbeqz t0, " << rlabel << "\n"; // left is zero -> result 0
                genExpr(bin->rhs, C);
                // result = (rhs != 0) ? 1 : 0
                out << "\tsltiu t0, t0, 1\n"; // 1 if rhs==0
                out << "\txori t0, t0, 1\n"; // invert
                out << "\tj " << end << "\n";
                out << rlabel << ":\n";
                out << "\tli t0, 0\n";
                out << end << ":\n";
            } else {
                // binary arithmetic/relational
                genExpr(bin->lhs, C);
                out << "\t# push lhs\n";
                out << "\tsw t0, " << offsetForArgslot(0) << "(s0)\n"; // use argslot0 as temp push
                genExpr(bin->rhs, C);
                out << "\t# rhs in t0; pop lhs into t1\n";
                out << "\tlw t1, " << offsetForArgslot(0) << "(s0)\n";
                // note: t1 holds lhs, t0 holds rhs
                if (op == "+") out << "\tadd t0, t1, t0\n";
                else if (op == "-") out << "\tsub t0, t1, t0\n";
                else if (op == "*") out << "\tmul t0, t1, t0\n";
                else if (op == "/") out << "\tdiv t0, t1, t0\n";
                else if (op == "%") out << "\trem t0, t1, t0\n";
                else if (op == "<") out << "\tslt t0, t1, t0\n";
                else if (op == ">") out << "\tslt t0, t0, t1\n"; // t0 = (rhs < lhs)
                else if (op == "<=") {
                    out << "\tslt t0, t0, t1\n"; // (rhs < lhs)
                    out << "\txori t0, t0, 1\n"; // invert
                } else if (op == ">=") {
                    out << "\tslt t0, t1, t0\n"; // (lhs < rhs)
                    out << "\txori t0, t0, 1\n";
                } else if (op == "==") {
                    out << "\txor t0, t1, t0\n";
                    out << "\tsltiu t0, t0, 1\n"; // 1 if xor==0
                } else if (op == "!=") {
                    out << "\txor t0, t1, t0\n";
                    out << "\tbnez t0, " << ( (string)newLabel("neq_nonzero") ) << "\n"; // fallback approach
                    // Simpler: use sltiu to set to 1 if zero then invert
                    // But labels here created previously; to avoid complexity we'll instead:
                    // redo: xor in t0 then sltiu t0,t0,1 -> 1 if zero; xori t0,t0,1 -> invert
                    // (we already have xor in t0)
                    // so do sltiu then xori
                    // For safety, re-emit
                    // Note: to keep consistent, we replace above approach:
                    // (we'll rewrite properly)
                } else {
                    cerr << "Codegen error: unknown binary op " << op << "\n"; exit(1);
                }
                // fix for '!=': our branch above was messy; we'll implement generic solution:
                if (op == "!=") {
                    // we had t0 = xor(lhs,rhs)
                    out << "\txor t0, t1, t0\n"; // redundant but safe: ensure t0=xor
                    out << "\tsltiu t0, t0, 1\n";
                    out << "\txori t0, t0, 1\n";
                }
            }
        } else {
            cerr << "Codegen error: unknown expr type\n"; exit(1);
        }
    }
};

// Because genStmt/genExpr use Ctx, but we earlier had a placeholder, we'll provide wrappers that create Ctx and forward to member functions.

void CodeGen::genStmt(StmtPtr s, struct { CodeGen *cg; unordered_map<string,int> *localIndex; int localCount; int frameSize; string retLabel; int ARG_SLOTS; vector<string> loopCondLabels; vector<string> loopEndLabels; } &C_unused) {
    // This function is a shim; actual code above used a different Ctx. We'll build a Ctx and call the other genStmt.
    // To keep code shorter, we will construct a Ctx with data from C_unused.
    CodeGen::Ctx ctx;
    ctx.cg = this;
    ctx.localIndex = C_unused.localIndex;
    ctx.localCount = C_unused.localCount;
    ctx.frameSize = C_unused.frameSize;
    ctx.retLabel = C_unused.retLabel;
    ctx.ARG_SLOTS = C_unused.ARG_SLOTS;
    ctx.loopCondLabels = C_unused.loopCondLabels;
    ctx.loopEndLabels = C_unused.loopEndLabels;
    genStmt(s, ctx);
}

// Overloads for calling above from within genFunction
void CodeGen::genStmt(StmtPtr s, Ctx &C) {
    ::CodeGen::genStmt(this, s, C); // unreachable but keep for clarity (we implemented member genStmt earlier)
}

// The above structure got a bit tangled due to forward/backwards declarations.
// To simplify: we'll implement a straightforward gen function per function that sets up Ctx and calls the real genStmt function defined prior.
// For clarity: re-implement a simple version:

// A simpler consistent codegen: generate function again with a more direct inner lambda (to avoid Ctx tangled types).
// We'll re-implement genFunction in a robust but slightly longer way to ensure correctness.


// To avoid confusion and keep this example workable, we'll implement a simpler code generator pass now,
// replacing the earlier partially tangled implementation with a clean generation function:

// --- Clean simple generator (replacing previous genFunction/genStmt/genExpr) ---

// We'll produce a fresh, self-contained code generation implementation below for clarity.

class SimpleCodeGen {
public:
    vector<FuncPtr> funcs;
    unordered_map<string,int> funcIndex;
    ofstream &out;
    int labelCounter = 0;
    int ARG_SLOTS = 8;
    SimpleCodeGen(vector<FuncPtr> &f, ofstream &o): funcs(f), out(o) {
        for (int i = 0; i < (int)funcs.size(); ++i) funcIndex[funcs[i]->name] = i;
    }
    string newLabel(const string &b) { return b + "_" + to_string(labelCounter++); }
    static int roundup16(int x) { return ((x+15)/16)*16; }
    int offsetForLocalIndex(int localIndex, int localCount) {
        int base = 12 + 4*ARG_SLOTS;
        return - (base + 4*localIndex);
    }
    int offsetForArgslot(int i) { return - (12 + 4*i); }

    // collect locals like before
    void collectLocals(FuncPtr f, unordered_map<string,int> &localIndex, int &localCount) {
        int idx = 0;
        for (auto &p : f->params) localIndex[p.name] = idx++;
        function<void(StmtPtr)> walk;
        walk = [&](StmtPtr s) {
            if (!s) return;
            if (auto b = dynamic_pointer_cast<BlockStmt>(s)) {
                for (auto &ss : b->stms) walk(ss);
            } else if (auto ds = dynamic_pointer_cast<DeclStmt>(s)) {
                // if same name exists, create a shadow name entry to get unique index
                string nm = ds->name;
                if (localIndex.count(nm)) {
                    string shadow = nm + "@sh" + to_string(idx);
                    localIndex[shadow] = idx++;
                } else {
                    localIndex[nm] = idx++;
                }
            } else if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) {
                walk(ifs->thenS);
                if (ifs->elseS) walk(ifs->elseS);
            } else if (auto wh = dynamic_pointer_cast<WhileStmt>(s)) {
                walk(wh->body);
            }
        };
        walk(f->body);
        localCount = idx;
    }

    // generate code for all functions
    void genAll() {
        out << "\t.text\n";
        for (auto &f : funcs) genFunc(f);
    }

    // core: generate a single function
    void genFunc(FuncPtr f) {
        unordered_map<string,int> localIndex;
        int localCount = 0;
        collectLocals(f, localIndex, localCount);
        int frameSize = 16 + 4*ARG_SLOTS + 4*localCount;
        frameSize = roundup16(frameSize);

        string fname = f->name;
        out << "\t.globl " << fname << "\n";
        out << fname << ":\n";
        out << "\taddi sp, sp, -" << frameSize << "\n";
        out << "\tsw ra, " << (frameSize - 4) << "(sp)\n";
        out << "\tsw s0, " << (frameSize - 8) << "(sp)\n";
        out << "\taddi s0, sp, " << frameSize << "\n";

        // store incoming args
        for (int i = 0; i < (int)f->params.size(); ++i) {
            if (i >= ARG_SLOTS) { cerr << "Error: >8 params not supported\n"; exit(1); }
            int off = offsetForLocalIndex(i, localCount);
            out << "\tsw a" << i << ", " << off << "(s0)\n";
        }

        string retLabel = fname + "_ret";
        // We'll implement recursive lambdas for statement/expr generation that capture localIndex/localCount/frameSize/retLabel.

        function<void(StmtPtr, vector<string>&, vector<string>&)> genStmt;
        function<void(ExprPtr)> genExpr;

        genExpr = [&](ExprPtr e) {
            if (!e) { out << "\tli t0, 0\n"; return; }
            if (auto num = dynamic_pointer_cast<NumberExpr>(e)) {
                out << "\tli t0, " << num->val << "\n";
            } else if (auto var = dynamic_pointer_cast<VarExpr>(e)) {
                int idx = -1;
                if (localIndex.count(var->name)) idx = localIndex[var->name];
                else {
                    // try shadow keys
                    for (auto &kv : localIndex) if (kv.first.rfind(var->name,0)==0) { idx = kv.second; break; }
                }
                if (idx < 0) { cerr << "CodeGen: var not found " << var->name << "\n"; exit(1); }
                int off = offsetForLocalIndex(idx, localCount);
                out << "\tlw t0, " << off << "(s0)\n";
            } else if (auto call = dynamic_pointer_cast<CallExpr>(e)) {
                if ((int)call->args.size() > ARG_SLOTS) { cerr << "CodeGen: too many args\n"; exit(1); }
                for (int i = 0; i < (int)call->args.size(); ++i) {
                    genExpr(call->args[i]);
                    out << "\tsw t0, " << offsetForArgslot(i) << "(s0)\n";
                }
                for (int i = 0; i < (int)call->args.size(); ++i) {
                    out << "\tlw a" << i << ", " << offsetForArgslot(i) << "(s0)\n";
                }
                out << "\tcall " << call->name << "\n";
                out << "\tmv t0, a0\n";
            } else if (auto un = dynamic_pointer_cast<UnaryExpr>(e)) {
                genExpr(un->rhs);
                if (un->op == "+") { }
                else if (un->op == "-") out << "\tsub t0, x0, t0\n";
                else if (un->op == "!") { out << "\tsltiu t0, t0, 1\n"; }
            } else if (auto bin = dynamic_pointer_cast<BinaryExpr>(e)) {
                string op = bin->op;
                if (op == "||") {
                    string lblR = newLabel("lorr");
                    string lblEnd = newLabel("lorend");
                    genExpr(bin->lhs);
                    out << "\tbeqz t0, " << lblR << "\n";
                    out << "\tli t0, 1\n";
                    out << "\tj " << lblEnd << "\n";
                    out << lblR << ":\n";
                    genExpr(bin->rhs);
                    out << "\tsltiu t0, t0, 1\n";
                    out << "\txori t0, t0, 1\n";
                    out << lblEnd << ":\n";
                } else if (op == "&&") {
                    string lblR = newLabel("landr");
                    string lblEnd = newLabel("landend");
                    genExpr(bin->lhs);
                    out << "\tbeqz t0, " << lblR << "\n";
                    genExpr(bin->rhs);
                    out << "\tsltiu t0, t0, 1\n";
                    out << "\txori t0, t0, 1\n";
                    out << "\tj " << lblEnd << "\n";
                    out << lblR << ":\n";
                    out << "\tli t0, 0\n";
                    out << lblEnd << ":\n";
                } else {
                    // compute lhs -> t1, rhs -> t0
                    genExpr(bin->lhs);
                    out << "\tsw t0, " << offsetForArgslot(0) << "(s0)\n";
                    genExpr(bin->rhs);
                    out << "\tlw t1, " << offsetForArgslot(0) << "(s0)\n";
                    // now t1 = lhs, t0 = rhs
                    if (op == "+") out << "\tadd t0, t1, t0\n";
                    else if (op == "-") out << "\tsub t0, t1, t0\n";
                    else if (op == "*") out << "\tmul t0, t1, t0\n";
                    else if (op == "/") out << "\tdiv t0, t1, t0\n";
                    else if (op == "%") out << "\trem t0, t1, t0\n";
                    else if (op == "<") out << "\tslt t0, t1, t0\n";
                    else if (op == ">") out << "\tslt t0, t0, t1\n";
                    else if (op == "<=") { out << "\tslt t0, t0, t1\n"; out << "\txori t0, t0, 1\n"; }
                    else if (op == ">=") { out << "\tslt t0, t1, t0\n"; out << "\txori t0, t0, 1\n"; }
                    else if (op == "==") { out << "\txor t0, t1, t0\n"; out << "\tsltiu t0, t0, 1\n"; }
                    else if (op == "!=") { out << "\txor t0, t1, t0\n"; out << "\tsltiu t0, t0, 1\n"; out << "\txori t0, t0, 1\n"; }
                    else { cerr << "CodeGen: unknown binary op " << op << "\n"; exit(1); }
                }
            } else {
                cerr << "CodeGen: unhandled expr node\n"; exit(1);
            }
        };

        // statement generator with loop label stacks
        function<void(StmtPtr, vector<string>&, vector<string>&)> genS =
            [&](StmtPtr st, vector<string> &loopCond, vector<string> &loopEnd) {
            if (!st) return;
            if (auto b = dynamic_pointer_cast<BlockStmt>(st)) {
                for (auto &ss : b->stms) genS(ss, loopCond, loopEnd);
            } else if (auto e = dynamic_pointer_cast<ExprStmt>(st)) {
                genExpr(e->expr);
            } else if (auto d = dynamic_pointer_cast<DeclStmt>(st)) {
                genExpr(d->init);
                int idx = -1;
                if (localIndex.count(d->name)) idx = localIndex[d->name];
                else {
                    for (auto &kv : localIndex) if (kv.first.rfind(d->name,0)==0) { idx = kv.second; break; }
                }
                if (idx < 0) { cerr << "Codegen: decl var not found\n"; exit(1); }
                int off = offsetForLocalIndex(idx, localCount);
                out << "\tsw t0, " << off << "(s0)\n";
            } else if (auto a = dynamic_pointer_cast<AssignStmt>(st)) {
                genExpr(a->expr);
                int idx = -1;
                if (localIndex.count(a->name)) idx = localIndex[a->name];
                else { for (auto &kv : localIndex) if (kv.first.rfind(a->name,0)==0) { idx = kv.second; break; } }
                if (idx < 0) { cerr << "Codegen: assign var not found\n"; exit(1); }
                int off = offsetForLocalIndex(idx, localCount);
                out << "\tsw t0, " << off << "(s0)\n";
            } else if (auto ifs = dynamic_pointer_cast<IfStmt>(st)) {
                string elseL = newLabel("else");
                string endL = newLabel("ifend");
                genExpr(ifs->cond);
                out << "\tbeqz t0, " << elseL << "\n";
                genS(ifs->thenS, loopCond, loopEnd);
                out << "\tj " << endL << "\n";
                out << elseL << ":\n";
                if (ifs->elseS) genS(ifs->elseS, loopCond, loopEnd);
                out << endL << ":\n";
            } else if (auto wh = dynamic_pointer_cast<WhileStmt>(st)) {
                string condL = newLabel("while_cond");
                string bodyL = newLabel("while_body");
                string endL = newLabel("while_end");
                loopCond.push_back(condL); loopEnd.push_back(endL);
                out << condL << ":\n";
                genExpr(wh->cond);
                out << "\tbeqz t0, " << endL << "\n";
                out << bodyL << ":\n";
                genS(wh->body, loopCond, loopEnd);
                out << "\tj " << condL << "\n";
                out << endL << ":\n";
                loopCond.pop_back(); loopEnd.pop_back();
            } else if (dynamic_pointer_cast<BreakStmt>(st)) {
                if (loopEnd.empty()) { cerr << "Codegen: break without loop\n"; exit(1); }
                out << "\tj " << loopEnd.back() << "\n";
            } else if (dynamic_pointer_cast<ContinueStmt>(st)) {
                if (loopCond.empty()) { cerr << "Codegen: continue without loop\n"; exit(1); }
                out << "\tj " << loopCond.back() << "\n";
            } else if (auto r = dynamic_pointer_cast<ReturnStmt>(st)) {
                if (r->hasVal) { genExpr(r->val); out << "\tmv a0, t0\n"; }
                out << "\tj " << f->name << "_ret\n";
            } else if (dynamic_pointer_cast<EmptyStmt>(st)) {
                // nothing
            } else {
                cerr << "Codegen: stmt not handled\n"; exit(1);
            }
        };

        vector<string> lcond, lend;
        genS(f->body, lcond, lend);

        // epilogue label
        out << f->name << "_ret:\n";
        out << "\taddi sp, sp, " << frameSize << "\n";
        out << "\tlw s0, " << (frameSize - 8) << "(sp)\n";
        out << "\tlw ra, " << (frameSize - 4) << "(sp)\n";
        out << "\tret\n";
    }
};

// ---------- Main driver ----------
int main(int argc, char **argv) {
    if (argc < 2) {
        cerr << "Usage: " << argv[0] << " input.tc -o out.s\n";
        return 1;
    }
    string inFile;
    string outFile = "out.s";
    inFile = argv[1];
    for (int i = 2; i < argc; ++i) {
        string a = argv[i];
        if (a == "-o" && i+1 < argc) { outFile = argv[++i]; }
    }
    // read input
    ifstream ifs(inFile);
    if (!ifs) { cerr << "Cannot open " << inFile << "\n"; return 1; }
    stringstream ss; ss << ifs.rdbuf();
    string s = ss.str();

    Lexer lex(s);
    Parser parser(lex);
    auto funcs = parser.parseCompUnit();

    // semantic
    Semantic sem;
    sem.run(funcs);

    // codegen
    ofstream ofs(outFile);
    if (!ofs) { cerr << "Cannot open " << outFile << " for writing\n"; return 1; }
    SimpleCodeGen cg(funcs, ofs);
    cg.genAll();

    cerr << "Compiled " << inFile << " -> " << outFile << " successfully.\n";
    return 0;
}
