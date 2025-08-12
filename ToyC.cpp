#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <stdexcept>
#include <sstream>
#include <regex>
#include <stack>

// Enum for token types
enum class TokenType {
    ID, NUMBER, INT, VOID, IF, ELSE, WHILE, BREAK, CONTINUE, RETURN,
    PLUS, MINUS, STAR, SLASH, PERCENT, BANG, AMPAMP, PIPEPIPE,
    LT, GT, LE, GE, EQEQ, BANGEQ, EQ, LPAREN, RPAREN, LBRACE, RBRACE, SEMI, COMMA,
    EOF_TOKEN
};

// Token structure
struct Token {
    TokenType type;
    std::string value;
    int line;
    int column;
};

// Lexer class
class Lexer {
private:
    std::string source;
    size_t pos = 0;
    int line = 1;
    int column = 1;

    std::unordered_map<std::string, TokenType> keywords = {
        {"int", TokenType::INT},
        {"void", TokenType::VOID},
        {"if", TokenType::IF},
        {"else", TokenType::ELSE},
        {"while", TokenType::WHILE},
        {"break", TokenType::BREAK},
        {"continue", TokenType::CONTINUE},
        {"return", TokenType::RETURN}
    };

    char peek() {
        if (pos >= source.size()) return '\0';
        return source[pos];
    }

    char advance() {
        char c = peek();
        if (c == '\n') { line++; column = 1; } else { column++; }
        pos++;
        return c;
    }

    void skipWhitespace() {
        while (peek() == ' ' || peek() == '\t' || peek() == '\n' || peek() == '\r') advance();
    }

    void skipComment() {
        if (peek() == '/' && source[pos+1] == '/') {
            advance(); advance();
            while (peek() != '\n' && peek() != '\0') advance();
        } else if (peek() == '/' && source[pos+1] == '*') {
            advance(); advance();
            while (peek() != '\0') {
                if (peek() == '*' && source[pos+1] == '/') {
                    advance(); advance();
                    return;
                }
                advance();
            }
        }
    }

public:
    Lexer(const std::string& src) : source(src) {}

    Token nextToken() {
        skipWhitespace();
        while (peek() == '/' && (source[pos+1] == '/' || source[pos+1] == '*')) {
            skipComment();
            skipWhitespace();
        }

        if (peek() == '\0') return {TokenType::EOF_TOKEN, "", line, column};

        // Identifiers and keywords
        if (isalpha(peek()) || peek() == '_') {
            std::string id;
            while (isalnum(peek()) || peek() == '_') id += advance();
            auto it = keywords.find(id);
            if (it != keywords.end()) return {it->second, id, line, column};
            return {TokenType::ID, id, line, column};
        }

        // Numbers
        if (isdigit(peek()) || (peek() == '-' && isdigit(source[pos+1]))) {
            std::string num;
            num += advance();
            while (isdigit(peek())) num += advance();
            return {TokenType::NUMBER, num, line, column};
        }

        // Operators and punctuation
        char c = advance();
        switch (c) {
            case '+': return {TokenType::PLUS, "+", line, column};
            case '-': return {TokenType::MINUS, "-", line, column};
            case '*': return {TokenType::STAR, "*", line, column};
            case '/': return {TokenType::SLASH, "/", line, column};
            case '%': return {TokenType::PERCENT, "%", line, column};
            case '!': 
                if (peek() == '=') { advance(); return {TokenType::BANGEQ, "!=", line, column}; }
                return {TokenType::BANG, "!", line, column};
            case '&': 
                if (peek() == '&') { advance(); return {TokenType::AMPAMP, "&&", line, column}; }
                break;
            case '|': 
                if (peek() == '|') { advance(); return {TokenType::PIPEPIPE, "||", line, column}; }
                break;
            case '<': 
                if (peek() == '=') { advance(); return {TokenType::LE, "<=", line, column}; }
                return {TokenType::LT, "<", line, column};
            case '>': 
                if (peek() == '=') { advance(); return {TokenType::GE, ">=", line, column}; }
                return {TokenType::GT, ">", line, column};
            case '=': 
                if (peek() == '=') { advance(); return {TokenType::EQEQ, "==", line, column}; }
                return {TokenType::EQ, "=", line, column};
            case '(': return {TokenType::LPAREN, "(", line, column};
            case ')': return {TokenType::RPAREN, ")", line, column};
            case '{': return {TokenType::LBRACE, "{", line, column};
            case '}': return {TokenType::RBRACE, "}", line, column};
            case ';': return {TokenType::SEMI, ";", line, column};
            case ',': return {TokenType::COMMA, ",", line, column};
        }

        throw std::runtime_error("Unexpected character: " + std::string(1, c));
    }
};

// AST nodes
class ASTNode {
public:
    virtual ~ASTNode() = default;
};

class Expr : public ASTNode {};

class Stmt : public ASTNode {};

class FuncDef : public ASTNode {
public:
    std::string returnType; // "int" or "void"
    std::string name;
    std::vector<std::string> params;
    std::shared_ptr<Stmt> body;
};

class CompUnit : public ASTNode {
public:
    std::vector<std::shared_ptr<FuncDef>> functions;
};

class BlockStmt : public Stmt {
public:
    std::vector<std::shared_ptr<Stmt>> statements;
};

class ExprStmt : public Stmt {
public:
    std::shared_ptr<Expr> expr;
};

class EmptyStmt : public Stmt {};

class AssignStmt : public Stmt {
public:
    std::string id;
    std::shared_ptr<Expr> expr;
};

class DeclStmt : public Stmt {
public:
    std::string id;
    std::shared_ptr<Expr> expr;
};

class IfStmt : public Stmt {
public:
    std::shared_ptr<Expr> cond;
    std::shared_ptr<Stmt> thenStmt;
    std::shared_ptr<Stmt> elseStmt;
};

class WhileStmt : public Stmt {
public:
    std::shared_ptr<Expr> cond;
    std::shared_ptr<Stmt> body;
};

class BreakStmt : public Stmt {};

class ContinueStmt : public Stmt {};

class ReturnStmt : public Stmt {
public:
    std::shared_ptr<Expr> expr; // optional, nullptr for void
};

class BinaryExpr : public Expr {
public:
    std::string op;
    std::shared_ptr<Expr> left;
    std::shared_ptr<Expr> right;
};

class UnaryExpr : public Expr {
public:
    std::string op;
    std::shared_ptr<Expr> expr;
};

class IdExpr : public Expr {
public:
    std::string id;
};

class NumberExpr : public Expr {
public:
    int value;
};

class CallExpr : public Expr {
public:
    std::string funcName;
    std::vector<std::shared_ptr<Expr>> args;
};

class ParenExpr : public Expr {
public:
    std::shared_ptr<Expr> expr;
};

// Parser class
class Parser {
private:
    std::vector<Token> tokens;
    size_t pos = 0;

    Token current() {
        if (pos >= tokens.size()) return {TokenType::EOF_TOKEN, "", 0, 0};
        return tokens[pos];
    }

    void advance() { pos++; }

    bool match(TokenType type) {
        if (current().type == type) {
            advance();
            return true;
        }
        return false;
    }

    void expect(TokenType type) {
        if (!match(type)) {
            throw std::runtime_error("Expected token type");
        }
    }

    std::shared_ptr<Expr> parsePrimaryExpr() {
        if (match(TokenType::ID)) {
            std::string id = tokens[pos-1].value;
            if (match(TokenType::LPAREN)) {
                auto call = std::make_shared<CallExpr>();
                call->funcName = id;
                if (current().type != TokenType::RPAREN) {
                    do {
                        call->args.push_back(parseExpr());
                    } while (match(TokenType::COMMA));
                }
                expect(TokenType::RPAREN);
                return call;
            }
            auto idExpr = std::make_shared<IdExpr>();
            idExpr->id = id;
            return idExpr;
        } else if (match(TokenType::NUMBER)) {
            auto num = std::make_shared<NumberExpr>();
            num->value = std::stoi(tokens[pos-1].value);
            return num;
        } else if (match(TokenType::LPAREN)) {
            auto paren = std::make_shared<ParenExpr>();
            paren->expr = parseExpr();
            expect(TokenType::RPAREN);
            return paren;
        }
        throw std::runtime_error("Unexpected primary expr");
    }

    std::shared_ptr<Expr> parseUnaryExpr() {
        if (match(TokenType::PLUS) || match(TokenType::MINUS) || match(TokenType::BANG)) {
            auto unary = std::make_shared<UnaryExpr>();
            unary->op = tokens[pos-1].value;
            unary->expr = parseUnaryExpr();
            return unary;
        }
        return parsePrimaryExpr();
    }

    std::shared_ptr<Expr> parseMulExpr() {
        auto left = parseUnaryExpr();
        while (match(TokenType::STAR) || match(TokenType::SLASH) || match(TokenType::PERCENT)) {
            auto bin = std::make_shared<BinaryExpr>();
            bin->left = left;
            bin->op = tokens[pos-1].value;
            bin->right = parseUnaryExpr();
            left = bin;
        }
        return left;
    }

    std::shared_ptr<Expr> parseAddExpr() {
        auto left = parseMulExpr();
        while (match(TokenType::PLUS) || match(TokenType::MINUS)) {
            auto bin = std::make_shared<BinaryExpr>();
            bin->left = left;
            bin->op = tokens[pos-1].value;
            bin->right = parseMulExpr();
            left = bin;
        }
        return left;
    }

    std::shared_ptr<Expr> parseRelExpr() {
        auto left = parseAddExpr();
        while (match(TokenType::LT) || match(TokenType::GT) || match(TokenType::LE) || match(TokenType::GE) || match(TokenType::EQEQ) || match(TokenType::BANGEQ)) {
            auto bin = std::make_shared<BinaryExpr>();
            bin->left = left;
            bin->op = tokens[pos-1].value;
            bin->right = parseAddExpr();
            left = bin;
        }
        return left;
    }

    std::shared_ptr<Expr> parseLAndExpr() {
        auto left = parseRelExpr();
        while (match(TokenType::AMPAMP)) {
            auto bin = std::make_shared<BinaryExpr>();
            bin->left = left;
            bin->op = "&&";
            bin->right = parseRelExpr();
            left = bin;
        }
        return left;
    }

    std::shared_ptr<Expr> parseLOrExpr() {
        auto left = parseLAndExpr();
        while (match(TokenType::PIPEPIPE)) {
            auto bin = std::make_shared<BinaryExpr>();
            bin->left = left;
            bin->op = "||";
            bin->right = parseLAndExpr();
            left = bin;
        }
        return left;
    }

    std::shared_ptr<Expr> parseExpr() {
        return parseLOrExpr();
    }

    std::shared_ptr<Stmt> parseStmt() {
        if (match(TokenType::LBRACE)) {
            auto block = std::make_shared<BlockStmt>();
            while (current().type != TokenType::RBRACE && current().type != TokenType::EOF_TOKEN) {
                block->statements.push_back(parseStmt());
            }
            expect(TokenType::RBRACE);
            return block;
        } else if (match(TokenType::SEMI)) {
            return std::make_shared<EmptyStmt>();
        } else if (match(TokenType::INT)) {
            auto decl = std::make_shared<DeclStmt>();
            expect(TokenType::ID);
            decl->id = tokens[pos-1].value;
            expect(TokenType::EQ);
            decl->expr = parseExpr();
            expect(TokenType::SEMI);
            return decl;
        } else if (match(TokenType::IF)) {
            auto ifStmt = std::make_shared<IfStmt>();
            expect(TokenType::LPAREN);
            ifStmt->cond = parseExpr();
            expect(TokenType::RPAREN);
            ifStmt->thenStmt = parseStmt();
            if (match(TokenType::ELSE)) {
                ifStmt->elseStmt = parseStmt();
            }
            return ifStmt;
        } else if (match(TokenType::WHILE)) {
            auto whileStmt = std::make_shared<WhileStmt>();
            expect(TokenType::LPAREN);
            whileStmt->cond = parseExpr();
            expect(TokenType::RPAREN);
            whileStmt->body = parseStmt();
            return whileStmt;
        } else if (match(TokenType::BREAK)) {
            expect(TokenType::SEMI);
            return std::make_shared<BreakStmt>();
        } else if (match(TokenType::CONTINUE)) {
            expect(TokenType::SEMI);
            return std::make_shared<ContinueStmt>();
        } else if (match(TokenType::RETURN)) {
            auto ret = std::make_shared<ReturnStmt>();
            if (current().type != TokenType::SEMI) {
                ret->expr = parseExpr();
            }
            expect(TokenType::SEMI);
            return ret;
        } else {
            // ExprStmt or Assign
            auto expr = parseExpr();
            if (match(TokenType::SEMI)) {
                auto exprStmt = std::make_shared<ExprStmt>();
                exprStmt->expr = expr;
                return exprStmt;
            } else if (match(TokenType::EQ)) {
                auto assign = std::make_shared<AssignStmt>();
                if (auto idExpr = std::dynamic_pointer_cast<IdExpr>(expr)) {
                    assign->id = idExpr->id;
                } else {
                    throw std::runtime_error("Invalid assign lhs");
                }
                assign->expr = parseExpr();
                expect(TokenType::SEMI);
                return assign;
            }
            throw std::runtime_error("Invalid statement");
        }
    }

    std::shared_ptr<FuncDef> parseFuncDef() {
        auto func = std::make_shared<FuncDef>();
        if (match(TokenType::INT)) {
            func->returnType = "int";
        } else if (match(TokenType::VOID)) {
            func->returnType = "void";
        } else {
            throw std::runtime_error("Expected int or void");
        }
        expect(TokenType::ID);
        func->name = tokens[pos-1].value;
        expect(TokenType::LPAREN);
        if (current().type != TokenType::RPAREN) {
            do {
                expect(TokenType::INT);
                expect(TokenType::ID);
                func->params.push_back(tokens[pos-1].value);
            } while (match(TokenType::COMMA));
        }
        expect(TokenType::RPAREN);
        func->body = parseStmt();
        return func;
    }

public:
    Parser(const std::vector<Token>& tks) : tokens(tks) {}

    std::shared_ptr<CompUnit> parse() {
        auto unit = std::make_shared<CompUnit>();
        while (current().type != TokenType::EOF_TOKEN) {
            unit->functions.push_back(parseFuncDef());
        }
        return unit;
    }
};

// Semantic Analyzer
class SemanticAnalyzer {
private:
    std::unordered_map<std::string, std::string> funcTypes;
    std::stack<std::unordered_map<std::string, bool>> scopes; // bool for declared
    bool inLoop = false;
    std::string currentFunc;

    void enterScope() {
        scopes.push({});
    }

    void exitScope() {
        scopes.pop();
    }

    bool isDeclared(const std::string& id) {
        for (auto it = scopes.top(); it == scopes.top(); ) wait, use loop.
        for (int i = scopes.size() - 1; i >= 0; i--) {
            if (scopes._Get_container()[i].count(id)) return true;
        }
        return false;
    }

    void declare(const std::string& id) {
        if (scopes.top().count(id)) throw std::runtime_error("Redeclaration");
        scopes.top()[id] = true;
    }

    std::string getFuncType(const std::string& name) {
        auto it = funcTypes.find(name);
        if (it == funcTypes.end()) throw std::runtime_error("Undefined function");
        return it->second;
    }

    void analyzeExpr(std::shared_ptr<Expr> expr, bool expectInt = true) {
        // Simplified, assume all expr are int
        if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr)) {
            analyzeExpr(bin->left);
            analyzeExpr(bin->right);
        } else if (auto un = std::dynamic_pointer_cast<UnaryExpr>(expr)) {
            analyzeExpr(un->expr);
        } else if (auto id = std::dynamic_pointer_cast<IdExpr>(expr)) {
            if (!isDeclared(id->id)) throw std::runtime_error("Undefined variable");
        } else if (auto num = std::dynamic_pointer_cast<NumberExpr>(expr)) {
            // ok
        } else if (auto call = std::dynamic_pointer_cast<CallExpr>(expr)) {
            std::string ret = getFuncType(call->funcName);
            if (expectInt && ret == "void") throw std::runtime_error("Void function in expression");
            // Check args count, assume params match args
            for (auto& arg : call->args) analyzeExpr(arg);
        } else if (auto par = std::dynamic_pointer_cast<ParenExpr>(expr)) {
            analyzeExpr(par->expr);
        }
    }

    void analyzeStmt(std::shared_ptr<Stmt> stmt) {
        if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt)) {
            enterScope();
            for (auto& s : block->statements) analyzeStmt(s);
            exitScope();
        } else if (auto exprS = std::dynamic_pointer_cast<ExprStmt>(stmt)) {
            analyzeExpr(exprS->expr, false); // allow void calls
        } else if (auto assign = std::dynamic_pointer_cast<AssignStmt>(stmt)) {
            if (!isDeclared(assign->id)) throw std::runtime_error("Undefined variable");
            analyzeExpr(assign->expr);
        } else if (auto decl = std::dynamic_pointer_cast<DeclStmt>(stmt)) {
            declare(decl->id);
            analyzeExpr(decl->expr);
        } else if (auto ifS = std::dynamic_pointer_cast<IfStmt>(stmt)) {
            analyzeExpr(ifS->cond);
            analyzeStmt(ifS->thenStmt);
            if (ifS->elseStmt) analyzeStmt(ifS->elseStmt);
        } else if (auto whileS = std::dynamic_pointer_cast<WhileStmt>(stmt)) {
            analyzeExpr(whileS->cond);
            inLoop = true;
            analyzeStmt(whileS->body);
            inLoop = false;
        } else if (auto br = std::dynamic_pointer_cast<BreakStmt>(stmt)) {
            if (!inLoop) throw std::runtime_error("Break outside loop");
        } else if (auto cont = std::dynamic_pointer_cast<ContinueStmt>(stmt)) {
            if (!inLoop) throw std::runtime_error("Continue outside loop");
        } else if (auto ret = std::dynamic_pointer_cast<ReturnStmt>(stmt)) {
            if (funcTypes[currentFunc] == "void") {
                if (ret->expr) throw std::runtime_error("Return value in void function");
            } else {
                if (!ret->expr) throw std::runtime_error("Missing return value in int function");
                analyzeExpr(ret->expr);
            }
        }
    }

public:
    void analyze(std::shared_ptr<CompUnit> unit) {
        enterScope();
        for (auto& func : unit->functions) {
            if (funcTypes.count(func->name)) throw std::runtime_error("Duplicate function");
            funcTypes[func->name] = func->returnType;
        }
        if (funcTypes.find("main") == funcTypes.end() || funcTypes["main"] != "int" || !unit->functions.back()->params.empty()) {
            throw std::runtime_error("Missing main function");
        }
        for (auto& func : unit->functions) {
            currentFunc = func->name;
            enterScope();
            for (auto& param : func->params) declare(param);
            analyzeStmt(func->body);
            exitScope();
        }
        exitScope();
    }
};

// Code Generator for RISC-V
class CodeGenerator {
private:
    std::ostream& out;
    int labelCount = 0;
    std::unordered_map<std::string, int> varOffsets;
    int stackOffset = 0;
    std::stack<int> loopLabels;

    std::string newLabel() {
        return "L" + std::to_string(labelCount++);
    }

    void genExpr(std::shared_ptr<Expr> expr) {
        if (auto bin = std::dynamic_pointer_cast<BinaryExpr>(expr)) {
            genExpr(bin->left);
            out << "  sw a0, 0(sp)\n";
            stackOffset += 4;
            genExpr(bin->right);
            out << "  lw t0, -" << stackOffset << "(sp)\n";
            stackOffset -= 4;
            if (bin->op == "+") out << "  add a0, t0, a0\n";
            else if (bin->op == "-") out << "  sub a0, t0, a0\n";
            else if (bin->op == "*") out << "  mul a0, t0, a0\n";
            else if (bin->op == "/") out << "  div a0, t0, a0\n";
            else if (bin->op == "%") out << "  rem a0, t0, a0\n";
            else if (bin->op == "==") {
                std::string ltrue = newLabel();
                std::string lend = newLabel();
                out << "  beq t0, a0, " << ltrue << "\n";
                out << "  li a0, 0\n";
                out << "  j " << lend << "\n";
                out << ltrue << ":\n";
                out << "  li a0, 1\n";
                out << lend << ":\n";
            }
            // Add other ops similarly: !=, <, >, <=, >=, &&, ||
            // For && and ||, need short circuit
            // Simplified, assume no short circuit for now
        } else if (auto un = std::dynamic_pointer_cast<UnaryExpr>(expr)) {
            genExpr(un->expr);
            if (un->op == "-") out << "  neg a0, a0\n";
            else if (un->op == "!") out << "  seqz a0, a0\n";
        } else if (auto id = std::dynamic_pointer_cast<IdExpr>(expr)) {
            int offset = varOffsets[id->id];
            out << "  lw a0, " << offset << "(sp)\n";
        } else if (auto num = std::dynamic_pointer_cast<NumberExpr>(expr)) {
            out << "  li a0, " << num->value << "\n";
        } else if (auto call = std::dynamic_pointer_cast<CallExpr>(expr)) {
            // Push args
            for (auto& arg : call->args) {
                genExpr(arg);
                out << "  sw a0, -" << call->args.size() * 4 << "(sp)\n"; // Simplified
            }
            out << "  call " << call->funcName << "\n";
        } else if (auto par = std::dynamic_pointer_cast<ParenExpr>(expr)) {
            genExpr(par->expr);
        }
    }

    void genStmt(std::shared_ptr<Stmt> stmt) {
        if (auto block = std::dynamic_pointer_cast<BlockStmt>(stmt)) {
            for (auto& s : block->statements) genStmt(s);
        } else if (auto exprS = std::dynamic_pointer_cast<ExprStmt>(stmt)) {
            genExpr(exprS->expr);
        } else if (auto assign = std::dynamic_pointer_cast<AssignStmt>(stmt)) {
            genExpr(assign->expr);
            int offset = varOffsets[assign->id];
            out << "  sw a0, " << offset << "(sp)\n";
        } else if (auto decl = std::dynamic_pointer_cast<DeclStmt>(stmt)) {
            stackOffset += 4;
            varOffsets[decl->id] = -stackOffset;
            genExpr(decl->expr);
            out << "  sw a0, " << varOffsets[decl->id] << "(sp)\n";
        } else if (auto ifS = std::dynamic_pointer_cast<IfStmt>(stmt)) {
            std::string lelse = newLabel();
            std::string lend = newLabel();
            genExpr(ifS->cond);
            out << "  beqz a0, " << lelse << "\n";
            genStmt(ifS->thenStmt);
            out << "  j " << lend << "\n";
            out << lelse << ":\n";
            if (ifS->elseStmt) genStmt(ifS->elseStmt);
            out << lend << ":\n";
        } else if (auto whileS = std::dynamic_pointer_cast<WhileStmt>(stmt)) {
            std::string lstart = newLabel();
            std::string lend = newLabel();
            loopLabels.push(lend); // for break
            out << lstart << ":\n";
            genExpr(whileS->cond);
            out << "  beqz a0, " << lend << "\n";
            genStmt(whileS->body);
            out << "  j " << lstart << "\n";
            out << lend << ":\n";
            loopLabels.pop();
        } else if (auto br = std::dynamic_pointer_cast<BreakStmt>(stmt)) {
            out << "  j " << loopLabels.top() << "\n";
        } else if (auto cont = std::dynamic_pointer_cast<ContinueStmt>(stmt)) {
            // Need start label for continue
            // Assume added
        } else if (auto ret = std::dynamic_pointer_cast<ReturnStmt>(stmt)) {
            if (ret->expr) genExpr(ret->expr);
            out << "  ret\n";
        }
    }

public:
    CodeGenerator(std::ostream& o) : out(o) {}

    void generate(std::shared_ptr<CompUnit> unit) {
        out << ".text\n";
        for (auto& func : unit->functions) {
            out << ".global " << func->name << "\n";
            out << func->name << ":\n";
            stackOffset = 0;
            varOffsets.clear();
            // Alloc params
            for (size_t i = 0; i < func->params.size(); i++) {
                stackOffset += 4;
                varOffsets[func->params[i]] = -stackOffset;
                out << "  sw a" << i << ", " << -stackOffset << "(sp)\n";
            }
            genStmt(func->body);
            // For void, add ret if no return
            if (func->returnType == "void") out << "  ret\n";
        }
        // Exit for main
        // In main, after ret, add exit
        out << "  li a7, 93\n"; // exit syscall
        out << "  ecall\n";
    }
};

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
