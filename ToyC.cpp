// ToyC.cpp
// Simple ToyC -> RISC-V32 compiler
// Compile: g++ -std=c++20 -O2 -o toyc ToyC.cpp

#include <bits/stdc++.h>
using namespace std;

// ---------------- Tokenizer ----------------
enum class TokType {
    End, Ident, Number,
    Kw_int, Kw_void, Kw_if, Kw_else, Kw_while, Kw_break, Kw_continue, Kw_return,
    Plus, Minus, Star, Slash, Mod,
    LParen, RParen, LBrace, RBrace, Comma, Semicolon,
    Assign,
    Eq, Neq, Lt, Gt, Le, Ge,
    AndAnd, OrOr, Not,
};

struct Token {
    TokType type;
    string lexeme;
    long long value;
    int line;
};

static string src;
static size_t srcpos = 0;
static int lineno = 1;

static bool startswith(const string &s) {
    return src.compare(srcpos, s.size(), s) == 0;
}

static void skipWhitespaceAndComments() {
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

static bool isIdentStart(char c) { return (c == '_' || isalpha((unsigned char)c)); }
static bool isIdentPart(char c) { return (c == '_' || isalnum((unsigned char)c)); }

static Token nextTokenRaw() {
    skipWhitespaceAndComments();
    if (srcpos >= src.size()) return {TokType::End, "", 0, lineno};
    char c = src[srcpos];
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
    if (isdigit((unsigned char)c) || (c == '-' && srcpos+1 < src.size() && isdigit(src[srcpos+1]))) {
        size_t b = srcpos;
        srcpos++;
        while (srcpos < src.size() && isdigit((unsigned char)src[srcpos])) srcpos++;
        string numstr = src.substr(b, srcpos - b);
        long long v = stoll(numstr);
        return {TokType::Number, numstr, v, lineno};
    }
    if (startswith("&&")) { srcpos += 2; return {TokType::AndAnd, "&&", 0, lineno}; }
    if (startswith("||")) { srcpos += 2; return {TokType::OrOr, "||", 0, lineno}; }
    if (startswith("==")) { srcpos += 2; return {TokType::Eq, "==", 0, lineno}; }
    if (startswith("!=")) { srcpos += 2; return {TokType::Neq, "!=", 0, lineno}; }
    if (startswith("<=")) { srcpos += 2; return {TokType::Le, "<=", 0, lineno}; }
    if (startswith(">=")) { srcpos += 2; return {TokType::Ge, ">=", 0, lineno}; }
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
        cerr << "Parse error: expected " << what << " at line " << peek().line << "\n";
        exit(1);
    }
};

// ---------------- AST ----------------
struct Expr { virtual ~Expr() = default; };
using ExprPtr = shared_ptr<Expr>;

struct NumberExpr : Expr { long long val; NumberExpr(long long v): val(v) {} };
struct VarExpr : Expr { string name; VarExpr(string n): name(move(n)) {} };
struct CallExpr : Expr { string name; vector<ExprPtr> args; CallExpr(string n): name(move(n)) {} };
struct UnaryExpr : Expr { string op; ExprPtr rhs; UnaryExpr(string o, ExprPtr r): op(move(o)), rhs(r) {} };
struct BinaryExpr : Expr { string op; ExprPtr lhs, rhs; BinaryExpr(string o, ExprPtr l, ExprPtr r): op(move(o)), lhs(l), rhs(r) {} };

struct Stmt { virtual ~Stmt() = default; };
using StmtPtr = shared_ptr<Stmt>;

struct BlockStmt : Stmt { vector<StmtPtr> stms; };
struct EmptyStmt : Stmt {};
struct ExprStmt : Stmt { ExprPtr expr; ExprStmt(ExprPtr e): expr(e) {} };
struct DeclStmt : Stmt { string name; ExprPtr init; DeclStmt(string n, ExprPtr i): name(move(n)), init(i) {} };
struct AssignStmt : Stmt { string name; ExprPtr expr; AssignStmt(string n, ExprPtr e): name(move(n)), expr(e) {} };
struct IfStmt : Stmt { ExprPtr cond; StmtPtr thenS; StmtPtr elseS; IfStmt(ExprPtr c, StmtPtr t, StmtPtr e): cond(c), thenS(t), elseS(e) {} };
struct WhileStmt : Stmt { ExprPtr cond; StmtPtr body; WhileStmt(ExprPtr c, StmtPtr b): cond(c), body(b) {} };
struct BreakStmt : Stmt {};
struct ContinueStmt : Stmt {};
struct ReturnStmt : Stmt { bool hasVal; ExprPtr val; ReturnStmt(): hasVal(false) {} ReturnStmt(ExprPtr v): hasVal(true), val(v) {} };

struct Param { string name; Param(string n): name(move(n)) {} };
struct FuncDef { bool isInt; string name; vector<Param> params; shared_ptr<BlockStmt> body; int indexInFile = -1; };

// ---------------- Parser ----------------
struct Parser {
    Lexer &lex;
    Parser(Lexer &l): lex(l) {}
    vector<shared_ptr<FuncDef>> parseCompUnit() {
        vector<shared_ptr<FuncDef>> funcs;
        while (lex.peek().type != TokType::End) {
            funcs.push_back(parseFuncDef());
        }
        return funcs;
    }

    shared_ptr<FuncDef> parseFuncDef() {
        bool isInt = false;
        if (lex.accept(TokType::Kw_int)) isInt = true;
        else if (lex.accept(TokType::Kw_void)) isInt = false;
        else { cerr << "Expected int/void at function definition\n"; exit(1); }
        Token tname = lex.peek(); if (tname.type != TokType::Ident) { cerr << "Expected function name\n"; exit(1); }
        string name = lex.next().lexeme;
        lex.expect(TokType::LParen, "(");
        vector<Param> params;
        if (!lex.accept(TokType::RParen)) {
            params.push_back(parseParam());
            while (lex.accept(TokType::Comma)) params.push_back(parseParam());
            lex.expect(TokType::RParen, ")");
        }
        auto body = parseBlock();
        auto f = make_shared<FuncDef>();
        f->isInt = isInt; f->name = name; f->params = move(params); f->body = body;
        return f;
    }

    Param parseParam() {
        lex.expect(TokType::Kw_int, "int");
        Token t = lex.peek();
        if (t.type != TokType::Ident) { cerr << "Expected param name\n"; exit(1); }
        string name = lex.next().lexeme;
        return Param(name);
    }

    shared_ptr<BlockStmt> parseBlock() {
        lex.expect(TokType::LBrace, "{");
        auto blk = make_shared<BlockStmt>();
        while (lex.peek().type != TokType::RBrace) blk->stms.push_back(parseStmt());
        lex.expect(TokType::RBrace, "}");
        return blk;
    }

    StmtPtr parseStmt() {
        Token p = lex.peek();
        if (p.type == TokType::LBrace) return parseBlock();
        if (p.type == TokType::Semicolon) { lex.next(); return make_shared<EmptyStmt>(); }
        if (lex.accept(TokType::Kw_if)) {
            lex.expect(TokType::LParen, "(");
            ExprPtr cond = parseExpr();
            lex.expect(TokType::RParen, ")");
            StmtPtr thenS = parseStmt();
            StmtPtr elseS = nullptr;
            if (lex.accept(TokType::Kw_else)) elseS = parseStmt();
            return make_shared<IfStmt>(cond, thenS, elseS);
        }
        if (lex.accept(TokType::Kw_while)) {
            lex.expect(TokType::LParen, "(");
            ExprPtr cond = parseExpr();
            lex.expect(TokType::RParen, ")");
            StmtPtr body = parseStmt();
            return make_shared<WhileStmt>(cond, body);
        }
        if (lex.accept(TokType::Kw_break)) { lex.expect(TokType::Semicolon, ";"); return make_shared<BreakStmt>(); }
        if (lex.accept(TokType::Kw_continue)) { lex.expect(TokType::Semicolon, ";"); return make_shared<ContinueStmt>(); }
        if (lex.accept(TokType::Kw_return)) {
            if (lex.accept(TokType::Semicolon)) return make_shared<ReturnStmt>();
            ExprPtr val = parseExpr();
            lex.expect(TokType::Semicolon, ";");
            return make_shared<ReturnStmt>(val);
        }
        if (lex.accept(TokType::Kw_int)) {
            Token t = lex.peek();
            if (t.type != TokType::Ident) { cerr << "Expected identifier after int\n"; exit(1); }
            string id = lex.next().lexeme;
            lex.expect(TokType::Assign, "=");
            ExprPtr init = parseExpr();
            lex.expect(TokType::Semicolon, ";");
            return make_shared<DeclStmt>(id, init);
        }
        // Ambiguous: assignment or expr stmt
        if (p.type == TokType::Ident) {
            // lookahead
            size_t save = lex.idx;
            Token idtok = lex.next();
            if (lex.peek().type == TokType::Assign) {
                lex.idx = save;
                string id = lex.next().lexeme;
                lex.expect(TokType::Assign, "=");
                ExprPtr e = parseExpr();
                lex.expect(TokType::Semicolon, ";");
                return make_shared<AssignStmt>(id, e);
            } else {
                lex.idx = save;
                ExprPtr e = parseExpr();
                lex.expect(TokType::Semicolon, ";");
                return make_shared<ExprStmt>(e);
            }
        } else {
            ExprPtr e = parseExpr();
            lex.expect(TokType::Semicolon, ";");
            return make_shared<ExprStmt>(e);
        }
    }

    ExprPtr parseExpr() { return parseLOr(); }
    ExprPtr parseLOr() {
        ExprPtr e = parseLAnd();
        while (lex.accept(TokType::OrOr)) { ExprPtr r = parseLAnd(); e = make_shared<BinaryExpr>("||", e, r); }
        return e;
    }
    ExprPtr parseLAnd() {
        ExprPtr e = parseRel();
        while (lex.accept(TokType::AndAnd)) { ExprPtr r = parseRel(); e = make_shared<BinaryExpr>("&&", e, r); }
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
        Token t = lex.peek();
        if (lex.accept(TokType::Number)) return make_shared<NumberExpr>(t.value);
        if (lex.accept(TokType::Ident)) {
            string id = t.lexeme;
            if (lex.accept(TokType::LParen)) {
                vector<ExprPtr> args;
                if (!lex.accept(TokType::RParen)) {
                    args.push_back(parseExpr());
                    while (lex.accept(TokType::Comma)) args.push_back(parseExpr());
                    lex.expect(TokType::RParen, ")");
                }
                auto c = make_shared<CallExpr>(id);
                c->args = move(args);
                return c;
            } else {
                return make_shared<VarExpr>(id);
            }
        }
        if (lex.accept(TokType::LParen)) {
            ExprPtr e = parseExpr();
            lex.expect(TokType::RParen, ")");
            return e;
        }
        cerr << "Parse error: unexpected token at line " << lex.peek().line << "\n";
        exit(1);
    }
};

// ---------------- Semantic ----------------
struct Semantic {
    vector<shared_ptr<FuncDef>> funcs;
    unordered_map<string,int> funcIndex;
    void run(vector<shared_ptr<FuncDef>> &fns) {
        funcs = fns;
        for (int i = 0; i < (int)funcs.size(); ++i) {
            if (funcIndex.count(funcs[i]->name)) { cerr << "Semantic error: duplicate function " << funcs[i]->name << "\n"; exit(1); }
            funcIndex[funcs[i]->name] = i;
            funcs[i]->indexInFile = i;
        }
        if (!funcIndex.count("main")) { cerr << "Semantic error: main not found\n"; exit(1); }
        int mainIdx = funcIndex["main"];
        if (!funcs[mainIdx]->isInt || funcs[mainIdx]->params.size() != 0) { cerr << "Semantic error: main must be 'int main()'\n"; exit(1); }
        for (int i = 0; i < (int)funcs.size(); ++i) checkFunction(funcs[i], i);
    }

    bool returnsOnAllPaths(StmtPtr s) {
        if (!s) return false;
        if (auto r = dynamic_pointer_cast<ReturnStmt>(s)) return true;
        if (auto b = dynamic_pointer_cast<BlockStmt>(s)) {
            for (auto &st : b->stms) if (returnsOnAllPaths(st)) return true;
            return false;
        }
        if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) {
            if (!ifs->elseS) return false;
            return returnsOnAllPaths(ifs->thenS) && returnsOnAllPaths(ifs->elseS);
        }
        return false;
    }

    void checkFunction(shared_ptr<FuncDef> f, int idx) {
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
        pushScope();
        for (auto &p : f->params) {
            if (!declare(p.name)) { cerr << "Semantic error: duplicate parameter " << p.name << " in " << f->name << "\n"; exit(1); }
        }
        int loopDepth = 0;
        function<void(ExprPtr)> walkExpr;
        function<void(StmtPtr)> walkStmt;

        walkExpr = [&](ExprPtr e) {
            if (!e) return;
            if (auto v = dynamic_pointer_cast<VarExpr>(e)) {
                bool found = false;
                for (int i = (int)scopeStack.size()-1; i >= 0; --i) if (scopeStack[i].count(v->name)) { found = true; break; }
                if (!found) { cerr << "Semantic error: use of undeclared variable '" << v->name << "' in " << f->name << "\n"; exit(1); }
            } else if (auto c = dynamic_pointer_cast<CallExpr>(e)) {
                if (!funcIndex.count(c->name)) { cerr << "Semantic error: call to undeclared function '" << c->name << "' in " << f->name << "\n"; exit(1); }
                int calleeIdx = funcIndex[c->name];
                if (!(calleeIdx <= idx || calleeIdx == idx)) { cerr << "Semantic error: function '" << c->name << "' must be declared before use\n"; exit(1); }
                if ((int)c->args.size() != (int)funcs[calleeIdx]->params.size()) { cerr << "Semantic error: argument count mismatch when calling " << c->name << "\n"; exit(1); }
                for (auto &a : c->args) walkExpr(a);
            } else if (auto un = dynamic_pointer_cast<UnaryExpr>(e)) walkExpr(un->rhs);
            else if (auto bin = dynamic_pointer_cast<BinaryExpr>(e)) { walkExpr(bin->lhs); walkExpr(bin->rhs); }
            else if (auto num = dynamic_pointer_cast<NumberExpr>(e)) {}
        };

        walkStmt = [&](StmtPtr s) {
            if (!s) return;
            if (auto blk = dynamic_pointer_cast<BlockStmt>(s)) {
                pushScope();
                for (auto &ss : blk->stms) walkStmt(ss);
                popScope();
            } else if (auto ds = dynamic_pointer_cast<DeclStmt>(s)) {
                if (!declare(ds->name)) { cerr << "Semantic error: duplicate local var '" << ds->name << "' in " << f->name << "\n"; exit(1); }
                walkExpr(ds->init);
            } else if (auto as = dynamic_pointer_cast<AssignStmt>(s)) {
                bool found = false;
                for (int i = (int)scopeStack.size()-1; i >= 0; --i) if (scopeStack[i].count(as->name)) { found = true; break; }
                if (!found) { cerr << "Semantic error: assignment to undeclared var '" << as->name << "' in " << f->name << "\n"; exit(1); }
                walkExpr(as->expr);
            } else if (auto es = dynamic_pointer_cast<ExprStmt>(s)) walkExpr(es->expr);
            else if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) { walkExpr(ifs->cond); walkStmt(ifs->thenS); if (ifs->elseS) walkStmt(ifs->elseS); }
            else if (auto wh = dynamic_pointer_cast<WhileStmt>(s)) { walkExpr(wh->cond); loopDepth++; walkStmt(wh->body); loopDepth--; }
            else if (dynamic_pointer_cast<BreakStmt>(s)) { if (loopDepth <= 0) { cerr << "Semantic error: break not inside loop in " << f->name << "\n"; exit(1); } }
            else if (dynamic_pointer_cast<ContinueStmt>(s)) { if (loopDepth <= 0) { cerr << "Semantic error: continue not inside loop in " << f->name << "\n"; exit(1); } }
            else if (auto ret = dynamic_pointer_cast<ReturnStmt>(s)) { if (ret->hasVal) walkExpr(ret->val); }
            else if (dynamic_pointer_cast<EmptyStmt>(s)) {}
        };

        walkStmt(f->body);
        if (f->isInt) {
            if (!returnsOnAllPaths(f->body)) { cerr << "Semantic error: int function '" << f->name << "' may not return on all paths\n"; exit(1); }
        }
    }
};

// ---------------- SimpleCodeGen (RISC-V32) ----------------
struct SimpleCodeGen {
    vector<shared_ptr<FuncDef>> funcs;
    unordered_map<string,int> funcIndex;
    ofstream &out;
    int labelCounter = 0;
    int ARG_SLOTS = 8;
    SimpleCodeGen(vector<shared_ptr<FuncDef>> &f, ofstream &o): funcs(f), out(o) {
        for (int i = 0; i < (int)funcs.size(); ++i) funcIndex[funcs[i]->name] = i;
    }
    string newLabel(const string &b) { return b + "_" + to_string(labelCounter++); }
    static int roundup16(int x) { return ((x+15)/16)*16; }
    int offsetForLocalIndex(int localIndex, int localCount) {
        int base = 12 + 4*ARG_SLOTS;
        return - (base + 4*localIndex);
    }
    int offsetForArgslot(int i) { return - (12 + 4*i); }

    void collectLocals(shared_ptr<FuncDef> f, unordered_map<string,int> &localIndex, int &localCount) {
        int idx = 0;
        for (auto &p : f->params) localIndex[p.name] = idx++;
        function<void(StmtPtr)> walk;
        walk = [&](StmtPtr s) {
            if (!s) return;
            if (auto b = dynamic_pointer_cast<BlockStmt>(s)) for (auto &ss : b->stms) walk(ss);
            else if (auto d = dynamic_pointer_cast<DeclStmt>(s)) {
                string nm = d->name;
                if (localIndex.count(nm)) {
                    string shadow = nm + "@sh" + to_string(idx);
                    localIndex[shadow] = idx++;
                } else localIndex[nm] = idx++;
            } else if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) { walk(ifs->thenS); if (ifs->elseS) walk(ifs->elseS); }
            else if (auto wh = dynamic_pointer_cast<WhileStmt>(s)) walk(wh->body);
        };
        walk(f->body);
        localCount = idx;
    }

    void genAll() {
        out << "\t.text\n";
        for (auto &f : funcs) genFunc(f);
    }

    void genFunc(shared_ptr<FuncDef> f) {
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
        // store incoming args into local slots
        for (int i = 0; i < (int)f->params.size(); ++i) {
            if (i >= ARG_SLOTS) { cerr << "Error: >8 params not supported\n"; exit(1); }
            int off = offsetForLocalIndex(i, localCount);
            out << "\tsw a" << i << ", " << off << "(s0)\n";
        }
        // generate body
        vector<string> loopCond, loopEnd;
        function<void(StmtPtr)> genS;
        function<void(ExprPtr)> genE;

        genE = [&](ExprPtr e) {
            if (!e) { out << "\tli t0, 0\n"; return; }
            if (auto num = dynamic_pointer_cast<NumberExpr>(e)) {
                out << "\tli t0, " << num->val << "\n";
            } else if (auto var = dynamic_pointer_cast<VarExpr>(e)) {
                int idx = -1;
                if (localIndex.count(var->name)) idx = localIndex[var->name];
                else { for (auto &kv : localIndex) if (kv.first.rfind(var->name,0)==0) { idx = kv.second; break; } }
                if (idx < 0) { cerr << "CodeGen: var not found " << var->name << "\n"; exit(1); }
                int off = offsetForLocalIndex(idx, localCount);
                out << "\tlw t0, " << off << "(s0)\n";
            } else if (auto call = dynamic_pointer_cast<CallExpr>(e)) {
                if ((int)call->args.size() > ARG_SLOTS) { cerr << "CodeGen: too many args\n"; exit(1); }
                for (int i = 0; i < (int)call->args.size(); ++i) {
                    genE(call->args[i]);
                    out << "\tsw t0, " << offsetForArgslot(i) << "(s0)\n";
                }
                for (int i = 0; i < (int)call->args.size(); ++i) out << "\tlw a" << i << ", " << offsetForArgslot(i) << "(s0)\n";
                out << "\tcall " << call->name << "\n";
                out << "\tmv t0, a0\n";
            } else if (auto un = dynamic_pointer_cast<UnaryExpr>(e)) {
                genE(un->rhs);
                if (un->op == "+") {}
                else if (un->op == "-") out << "\tsub t0, x0, t0\n";
                else if (un->op == "!") out << "\tsltiu t0, t0, 1\n";
            } else if (auto bin = dynamic_pointer_cast<BinaryExpr>(e)) {
                string op = bin->op;
                if (op == "||") {
                    string rlabel = newLabel("lor_r");
                    string end = newLabel("lor_end");
                    genE(bin->lhs);
                    out << "\tbeqz t0, " << rlabel << "\n";
                    out << "\tli t0, 1\n";
                    out << "\tj " << end << "\n";
                    out << rlabel << ":\n";
                    genE(bin->rhs);
                    out << "\tsltiu t0, t0, 1\n";
                    out << "\txori t0, t0, 1\n";
                    out << end << ":\n";
                } else if (op == "&&") {
                    string rlabel = newLabel("land_r");
                    string end = newLabel("land_end");
                    genE(bin->lhs);
                    out << "\tbeqz t0, " << rlabel << "\n";
                    genE(bin->rhs);
                    out << "\tsltiu t0, t0, 1\n";
                    out << "\txori t0, t0, 1\n";
                    out << "\tj " << end << "\n";
                    out << rlabel << ":\n";
                    out << "\tli t0, 0\n";
                    out << end << ":\n";
                } else {
                    genE(bin->lhs);
                    out << "\tsw t0, " << offsetForArgslot(0) << "(s0)\n";
                    genE(bin->rhs);
                    out << "\tlw t1, " << offsetForArgslot(0) << "(s0)\n";
                    // t1 = lhs, t0 = rhs
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
                    else { cerr << "CodeGen: unknown binop " << op << "\n"; exit(1); }
                }
            } else { cerr << "CodeGen: unknown expr node\n"; exit(1); }
        };

        genS = [&](StmtPtr s) {
            if (!s) return;
            if (auto b = dynamic_pointer_cast<BlockStmt>(s)) {
                for (auto &st : b->stms) genS(st);
            } else if (auto es = dynamic_pointer_cast<ExprStmt>(s)) { genE(es->expr); }
            else if (auto ds = dynamic_pointer_cast<DeclStmt>(s)) {
                genE(ds->init);
                int idx = -1;
                if (localIndex.count(ds->name)) idx = localIndex[ds->name];
                else { for (auto &kv : localIndex) if (kv.first.rfind(ds->name,0)==0) { idx = kv.second; break; } }
                if (idx < 0) { cerr << "CodeGen: decl var not found\n"; exit(1); }
                int off = offsetForLocalIndex(idx, localCount);
                out << "\tsw t0, " << off << "(s0)\n";
            } else if (auto as = dynamic_pointer_cast<AssignStmt>(s)) {
                genE(as->expr);
                int idx = -1;
                if (localIndex.count(as->name)) idx = localIndex[as->name];
                else { for (auto &kv : localIndex) if (kv.first.rfind(as->name,0)==0) { idx = kv.second; break; } }
                if (idx < 0) { cerr << "CodeGen: assign var not found\n"; exit(1); }
                int off = offsetForLocalIndex(idx, localCount);
                out << "\tsw t0, " << off << "(s0)\n";
            } else if (auto ifs = dynamic_pointer_cast<IfStmt>(s)) {
                string elseL = newLabel("else");
                string endL = newLabel("ifend");
                genE(ifs->cond);
                out << "\tbeqz t0, " << elseL << "\n";
                genS(ifs->thenS);
                out << "\tj " << endL << "\n";
                out << elseL << ":\n";
                if (ifs->elseS) genS(ifs->elseS);
                out << endL << ":\n";
            } else if (auto wh = dynamic_pointer_cast<WhileStmt>(s)) {
                string condL = newLabel("while_cond");
                string bodyL = newLabel("while_body");
                string endL = newLabel("while_end");
                loopCond.push_back(condL); loopEnd.push_back(endL);
                out << condL << ":\n";
                genE(wh->cond);
                out << "\tbeqz t0, " << endL << "\n";
                out << bodyL << ":\n";
                genS(wh->body);
                out << "\tj " << condL << "\n";
                out << endL << ":\n";
                loopCond.pop_back(); loopEnd.pop_back();
            } else if (dynamic_pointer_cast<BreakStmt>(s)) {
                if (loopEnd.empty()) { cerr << "CodeGen: break without loop\n"; exit(1); }
                out << "\tj " << loopEnd.back() << "\n";
            } else if (dynamic_pointer_cast<ContinueStmt>(s)) {
                if (loopCond.empty()) { cerr << "CodeGen: continue without loop\n"; exit(1); }
                out << "\tj " << loopCond.back() << "\n";
            } else if (auto r = dynamic_pointer_cast<ReturnStmt>(s)) {
                if (r->hasVal) { genE(r->val); out << "\tmv a0, t0\n"; }
                out << "\tj " << fname << "_ret\n";
            } else if (dynamic_pointer_cast<EmptyStmt>(s)) {}
            else { cerr << "CodeGen: unknown stmt type\n"; exit(1); }
        };

        genS(f->body);

        out << fname << "_ret:\n";
        out << "\taddi sp, sp, " << frameSize << "\n";
        out << "\tlw s0, " << (frameSize - 8) << "(sp)\n";
        out << "\tlw ra, " << (frameSize - 4) << "(sp)\n";
        out << "\tret\n";
    }
};

// ---------------- Main ----------------
int main(int argc, char **argv) {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    if (argc < 4) {
        cerr << "Usage: ./ToyC input.tc -o out.s\n";
        return 1;
    }

    string inFile = argv[1];
    string outFile;
    for (int i = 2; i < argc - 1; i++) {
        if (string(argv[i]) == "-o" && i + 1 < argc) {
            outFile = argv[i + 1];
            break;
        }
    }

    if (outFile.empty()) {
        cerr << "Output file not specified with -o\n";
        return 1;
    }

    // 读取源码文件
    ifstream fin(inFile);
    if (!fin.is_open()) {
        cerr << "Cannot open input file: " << inFile << "\n";
        return 1;
    }
    string code((istreambuf_iterator<char>(fin)), istreambuf_iterator<char>());

    // 词法分析
    Lexer lex(code);

    // 语法分析
    Parser parser(lex);
    vector<FuncPtr> funcs = parser.parseProgram();

    // 语义检查
    SemanticAnalyzer sema;
    sema.analyze(funcs);

    // 代码生成
    ofstream fout(outFile);
    if (!fout.is_open()) {
        cerr << "Cannot open output file: " << outFile << "\n";
        return 1;
    }
    SimpleCodeGen cg(fout);
    cg.gen(funcs);

    return 0;
}
