%skeleton "lalr1.cc"
%define parser_class_name {conj_parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%define parse.error verbose
%locations   // <--

%code requires
{
#include <map>
#include <list>
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>

#define ENUM_IDENTIFIERS(o) \
        o(undefined)                              /* undefined */ \
        o(function)                               /* a pointer to given function */ \
        o(parameter)                              /* one of the function params */ \
        o(variable)                               /* a local variable */
#define o(n) n,
enum class id_type {  ENUM_IDENTIFIERS(o) };
#undef o

struct identifier
{
    id_type type  = id_type::undefined;
    std::size_t     index = 0; // function#, parameter# within surrounding function, variable#
    std::string     name;
};

#define ENUM_EXPRESSIONS(o) \
        o(nop) o(string) o(number) o(ident)       /* atoms */ \
        o(add) o(neg) o(eq)                       /* transformation */ \
        o(cor) o(cand) o(loop)                    /* logic. Loop is: while(param0) { param1..n } */ \
        o(addrof) o(deref)                        /* pointer handling */ \
        o(fcall)                                  /* function param0 call with param1..n */ \
        o(copy)                                   /* assign: param1 <<- param0 */ \
        o(comma)                                  /* a sequence of expressions */ \
        o(ret)                                    /* return(param0) */

#define o(n) n,
enum class ex_type {  ENUM_EXPRESSIONS(o) };
#undef o

typedef std::list<struct expression> expr_vec;
struct expression
{
    ex_type type;
    identifier      ident{};    // For ident
    std::string     strvalue{}; // For string
    long            numvalue=0; // For number
    expr_vec        params;
    // For while() and if(), the first item is the condition and the rest are the contingent code
    // For fcall, the first parameter is the variable to use as function

    template<typename... T>
    expression(ex_type t, T&&... args) : type(t), params{ std::forward<T>(args)... } {}

    expression()                    : type(ex_type::nop) {}
    expression(const identifier& i) : type(ex_type::ident),  ident(i)            { }
    expression(identifier&& i)      : type(ex_type::ident),  ident(std::move(i)) { }
    expression(std::string&& s)     : type(ex_type::string), strvalue(std::move(s)) { }
    expression(long v)              : type(ex_type::number), numvalue(v) {}

    bool is_pure() const;

    expression operator%=(expression&& b) && { return expression(ex_type::copy, std::move(b), std::move(*this)); }
};

#define o(n) \
template<typename... T> \
inline expression e_##n(T&&... args) { return expression(ex_type::n, std::forward<T>(args)...); }
ENUM_EXPRESSIONS(o)
#undef o

struct function
{
    std::string name;
    expression  code;
    unsigned num_vars = 0, num_params = 0;
};

struct lexcontext;

}//%code requires

%param { lexcontext& ctx }//%param

%code
{

struct lexcontext
{
    const char* cursor;
    yy::location loc;
    std::vector<std::map<std::string, identifier>> scopes;
    std::vector<function> func_list;
    unsigned tempcounter = 0;
    function fun;
public:
    const identifier& define(const std::string& name, identifier&& f)
    {
        auto r = scopes.back().emplace(name, std::move(f));
        if(!r.second) throw yy::conj_parser::syntax_error(loc, "Duplicate definition <"+name+">");
        return r.first->second;
    }
    expression def(const std::string& name)     { return define(name, identifier{id_type::variable,  fun.num_vars++,   name}); }
    expression defun(const std::string& name)   { return define(name, identifier{id_type::function,  func_list.size(), name}); }
    expression defparm(const std::string& name) { return define(name, identifier{id_type::parameter, fun.num_params++, name}); }
    expression temp()                           { return def("$I" + std::to_string(tempcounter++)); }
    expression use(const std::string& name)
    {
        for(auto j = scopes.crbegin(); j != scopes.crend(); ++j)
            if(auto i = j->find(name); i != j->end())
                return i->second;
        throw yy::conj_parser::syntax_error(loc, "Undefined identifier <"+name+">");
    }
    void add_function(std::string&& name, expression&& code)
    {
        fun.code = e_comma(std::move(code), e_ret(0l)); // Add implicit "return 0;" at the end
        fun.name = std::move(name);
        func_list.push_back(std::move(fun));
        fun = {};
    }
    void operator ++() { scopes.emplace_back(); } // Enter scope
    void operator --() { scopes.pop_back();     } // Exit scope
};

namespace yy { conj_parser::symbol_type yylex(lexcontext& ctx); }

#define M(x) std::move(x)
#define C(x) expression(x)

}//%code

%token             END 0
%token             RETURN "return" WHILE "while" IF "if" VAR "var" IDENTIFIER NUMCONST STRINGCONST
%token             OR "||"  AND "&&"  EQ "=="  NE "!="  PP "++"  MM "--"  PL_EQ "+="  MI_EQ "-="
%left  ','
%right '?' ':' '=' "+=" "-="
%left  "||"
%left  "&&"
%left  "==" "!="
%left  '+' '-'
%left  '*'
%right '&' "++" "--"
%left  '(' '['
%type<long>        NUMCONST
%type<std::string> IDENTIFIER STRINGCONST identifier1
%type<expression>  expr expr1 exprs exprs1 c_expr1 p_expr1 stmt stmt1 var_defs var_def1 com_stmt
%%

library:                        { ++ctx; } functions { --ctx; };
functions:                      functions identifier1 { ctx.defun($2); ++ctx; } paramdecls colon1 stmt1 { ctx.add_function(M($2), M($6)); --ctx; }
|                               %empty;
paramdecls:                     paramdecl | %empty;
paramdecl:                      paramdecl ',' identifier1   { ctx.defparm($3); }
|                               IDENTIFIER                  { ctx.defparm($1); };
identifier1:        error{}   | IDENTIFIER                  { $$ = M($1); };
colon1:             error{}   | ':';
semicolon1:         error{}   | ';';
cl_brace1:          error{}   | '}';
cl_bracket1:        error{}   | ']';
cl_parens1:         error{}   | ')';
stmt1:              error{}   | stmt                        { $$ = M($1); };
exprs1:             error{}   | exprs                       { $$ = M($1); };
expr1:              error{}   | expr                        { $$ = M($1); };
p_expr1:            error{}   | '(' exprs1 cl_parens1       { $$ = M($2); };
stmt:                           com_stmt         cl_brace1  { $$ = M($1); --ctx; }
|                               "if"     p_expr1 stmt1      { $$ = e_cand(M($2), M($3)); }
|                               "while"  p_expr1 stmt1      { $$ = e_loop(M($2), M($3)); }
|                               "return" exprs1  semicolon1 { $$ = e_ret(M($2));         }
|                               exprs            semicolon1 { $$ = M($1);        }
|                               ';'                         { };
com_stmt:                       '{'                         { $$ = e_comma(); ++ctx; }
|                               com_stmt stmt               { $$ = M($1); $$.params.push_back(M($2)); };
var_defs:                       "var"           var_def1    { $$ = e_comma(M($2)); }
|                               var_defs    ',' var_def1    { $$ = M($1); $$.params.push_back(M($3)); };
var_def1:                       identifier1 '=' expr1       { $$ = ctx.def($1) %= M($3); }
|                               identifier1                 { $$ = ctx.def($1) %= 0l; };
exprs:                          var_defs                    { $$ = M($1); }
|                               expr                        { $$ = M($1); }
|                               expr    ',' c_expr1         { $$ = e_comma(M($1)); $$.params.splice($$.params.end(), M($3.params)); };
c_expr1:                        expr1                       { $$ = e_comma(M($1)); }
|                               c_expr1 ',' expr1           { $$ = M($1); $$.params.push_back(M($3)); };
expr:                           NUMCONST                    { $$ = $1;    }
|                               STRINGCONST                 { $$ = M($1); }
|                               IDENTIFIER                  { $$ = ctx.use($1);   }
|                               '(' exprs1 cl_parens1       { $$ = M($2); }
|                               expr '[' exprs1 cl_bracket1 { $$ = e_deref(e_add(M($1), M($3))); }
|                               expr '(' ')'                { $$ = e_fcall(M($1)); }
|                               expr '(' c_expr1 cl_parens1 { $$ = e_fcall(M($1)); $$.params.splice($$.params.end(), M($3.params)); }
| expr '='  error {$$=M($1);} | expr '='  expr              { $$ = M($1) %= M($3); }
| expr '+'  error {$$=M($1);} | expr '+'  expr              { $$ = e_add( M($1), M($3)); }
| expr '-'  error {$$=M($1);} | expr '-'  expr   %prec '+'  { $$ = e_add( M($1), e_neg(M($3))); }
| expr "+=" error {$$=M($1);} | expr "+=" expr              { if(!$3.is_pure()) { $$ = ctx.temp() %= e_addrof(M($1)); $1 = e_deref($$.params.back()); }
                                                              $$ = e_comma(M($$), M($1) %= e_add(C($1), M($3))); }

| expr "-=" error {$$=M($1);} | expr "-=" expr              { if(!$3.is_pure()) { $$ = ctx.temp() %= e_addrof(M($1)); $1 = e_deref($$.params.back()); }
                                                              $$ = e_comma(M($$), M($1) %= e_add(C($1), e_neg(M($3)))); }

| "++" error {}               | "++" expr                   { if(!$2.is_pure()) { $$ = ctx.temp() %= e_addrof(M($2)); $2 = e_deref($$.params.back()); }
                                                              $$ = e_comma(M($$), M($2) %= e_add(C($2),  1l)); }

| "--" error {}               | "--" expr        %prec "++" { if(!$2.is_pure()) { $$ = ctx.temp() %= e_addrof(M($2)); $2 = e_deref($$.params.back()); }
                                                              $$ = e_comma(M($$), M($2) %= e_add(C($2), -1l)); }

|                               expr "++"                   { if(!$1.is_pure()) { $$ = ctx.temp() %= e_addrof(M($1)); $1 = e_deref($$.params.back()); }
                                                              auto i = ctx.temp(); $$ = e_comma(M($$), C(i) %= C($1), C($1) %= e_add(C($1),  1l), C(i)); }

|                               expr "--"        %prec "++" { if(!$1.is_pure()) { $$ = ctx.temp() %= e_addrof(M($1)); $1 = e_deref($$.params.back()); }
                                                              auto i = ctx.temp(); $$ = e_comma(M($$), C(i) %= C($1), C($1) %= e_add(C($1), -1l), C(i)); }

| expr "||" error {$$=M($1);} | expr "||" expr              { $$ = e_cor( M($1), M($3)); }
| expr "&&" error {$$=M($1);} | expr "&&" expr              { $$ = e_cand(M($1), M($3)); }
| expr "==" error {$$=M($1);} | expr "==" expr              { $$ = e_eq(  M($1), M($3)); }
| expr "!=" error {$$=M($1);} | expr "!=" expr   %prec "==" { $$ = e_eq(e_eq(M($1), M($3)), 0l); }
| '&' error{}                 | '&' expr                    { $$ = e_addrof(M($2)); }
| '*' error{}                 | '*' expr         %prec '&'  { $$ = e_deref(M($2));  }
| '-' error{}                 | '-' expr         %prec '&'  { $$ = e_neg(M($2));    }
| '!' error{}                 | '!' expr         %prec '&'  { $$ = e_eq(M($2), 0l); }
| expr '?'  error {$$=M($1);} | expr '?' expr ':' expr      { auto i = ctx.temp();
                                                              $$ = e_comma(e_cor(e_cand(M($1), e_comma(C(i) %= M($3), 1l)), C(i) %= M($5)), C(i)); }

%%

yy::conj_parser::symbol_type yy::yylex(lexcontext& ctx)
{
    const char* anchor = ctx.cursor;
    ctx.loc.step();
    auto s = [&](auto func, auto&&... params) { ctx.loc.columns(ctx.cursor - anchor); return func(params..., ctx.loc); };
%{ /* Begin re2c lexer */
re2c:yyfill:enable   = 0;
re2c:define:YYCTYPE  = "char";
re2c:define:YYCURSOR = "ctx.cursor";

// Keywords:
"return"                { return s(conj_parser::make_RETURN); }
"while" | "for"         { return s(conj_parser::make_WHILE); }
"var"                   { return s(conj_parser::make_VAR); }
"if"                    { return s(conj_parser::make_IF); }

// Identifiers:
[a-zA-Z_] [a-zA-Z_0-9]* { return s(conj_parser::make_IDENTIFIER, std::string(anchor,ctx.cursor)); }

// String and integer literals:
"\"" [^"]* "\""         { return s(conj_parser::make_STRINGCONST, std::string(anchor+1, ctx.cursor-1)); }
[0-9]+                  { return s(conj_parser::make_NUMCONST, std::stol(std::string(anchor,ctx.cursor))); }

// Whitespace and comments:
"\000"                  { return s(conj_parser::make_END); }
"\r\n" | [\r\n]         { ctx.loc.lines();   return yylex(ctx); }
"//" [^\r\n]*           {                    return yylex(ctx); }
[\t\v\b\f ]             { ctx.loc.columns(); return yylex(ctx); }

// Multi-char operators and any other character (either an operator or an invalid symbol):
"&&"                    { return s(conj_parser::make_AND); }
"||"                    { return s(conj_parser::make_OR); }
"++"                    { return s(conj_parser::make_PP); }
"--"                    { return s(conj_parser::make_MM); }
"!="                    { return s(conj_parser::make_NE); }
"=="                    { return s(conj_parser::make_EQ); }
"+="                    { return s(conj_parser::make_PL_EQ); }
"-="                    { return s(conj_parser::make_MI_EQ); }
.                       { return s([](auto...s){return conj_parser::symbol_type(s...);}, conj_parser::token_type(ctx.cursor[-1]&0xFF)); } // Return that character
%} /* End lexer */
}

void yy::conj_parser::error(const location_type& l, const std::string& m)
{
    std::cerr << (l.begin.filename ? l.begin.filename->c_str() : "(undefined)");
    std::cerr << ':' << l.begin.line << ':' << l.begin.column << '-' << l.end.column << ": " << m << '\n';
}

#include <fstream>

// is_pure tells whether the expression is safe to duplicate,
// or even delete if the return value is not used,
// without changing the program semantics.
bool expression::is_pure() const
{
    // if any parameter is not pure, the expression is not pure
    for(const auto& e: params) if(!e.is_pure()) return false;
    switch(type)
    {
        // Function calls are assumed to be not pure for now.
        case ex_type::fcall: return false;
        // Assigns are not pure.
        case ex_type::copy:  return false;
        // Returns and not pure either, because they do not evaluate into a value.
        case ex_type::ret:   return false;
        // Loops are not pure, because they may be infinite,
        // in which case deleting the loop would alter the program behavior.
        case ex_type::loop:  return false;
        // Anything else is pure
        default:             return true;
    }
}

std::string stringify(const expression& e, bool stmt);
std::string stringify_op(const expression& e, const char* sep, const char* delim, bool stmt = false, unsigned first=0, unsigned limit=~0u)
{
    std::string result(1, delim[0]);
    const char* fsep = "";
    for(const auto& p: e.params) { if(first) { --first; continue; }
                                   if(!limit--) break;
                                   result += fsep; fsep = sep; result += stringify(p, stmt); }
    if(stmt) result += sep;
    result += delim[1];
    return result;
}
std::string stringify(const expression& e, bool stmt = false)
{
    auto expect1 = [&]{ return e.params.empty() ? "?" : e.params.size()==1 ? stringify(e.params.front()) : stringify_op(e, "??", "()"); };
    switch(e.type)
    {
        // atoms
        case ex_type::nop    : return "";
        case ex_type::string : return "\"" + e.strvalue + "\"";
        case ex_type::number : return std::to_string(e.numvalue);
        case ex_type::ident  : return "?FPVS"[(int)e.ident.type] + std::to_string(e.ident.index) + "\"" + e.ident.name + "\"";
        // binary & misc
        case ex_type::add    : return stringify_op(e, " + ",  "()");
        case ex_type::eq     : return stringify_op(e, " == ", "()");
        case ex_type::cand   : return stringify_op(e, " && ", "()");
        case ex_type::cor    : return stringify_op(e, " || ", "()");
        case ex_type::comma  : return stmt ? stringify_op(e, "; ", "{}", true) : stringify_op(e, ", ",  "()");
        // unary
        case ex_type::neg    : return "-(" + expect1() + ")";
        case ex_type::deref  : return "*(" + expect1() + ")";
        case ex_type::addrof : return "&(" + expect1() + ")";
        // special
        case ex_type::copy   : return "(" + stringify(e.params.back()) + " = " + stringify(e.params.front()) + ")";
        case ex_type::fcall  : return "(" + (e.params.empty() ? "?" : stringify(e.params.front()))+")"+stringify_op(e,", ","()",false,1);
        case ex_type::loop   : return "while " + stringify(e.params.front()) + " " + stringify_op(e, "; ", "{}", true, 1);
        case ex_type::ret    : return "return " + expect1();
    }
    return "?";
}
static std::string stringify(const function& f)
{
    return stringify(f.code, true);
}

#include "textbox.hh"

static std::string stringify_tree(const function& f)
{
    textbox result;
    result.putbox(2,0, create_tree_graph(f.code, 132-2,
        [](const expression& e)
        {
            std::string p = stringify(e), k = p;
            switch(e.type)
            {
                #define o(n) case ex_type::n: k.assign(#n,sizeof(#n)-1); break;
                ENUM_EXPRESSIONS(o)
                #undef o
            }
            return e.params.empty() ? (k + ' ' + p) : std::move(k);
        },
        [](const expression& e) { return std::make_pair(e.params.cbegin(), e.params.cend()); },
        [](const expression& e) { return e.params.size() >= 1; }, // whether simplified horizontal layout can be used
        [](const expression&  ) { return true; },                 // whether extremely simplified horiz layout can be used
        [](const expression& e) { return e.type == ex_type::loop; }));
    return "function " + f.name + ":\n" + stringify(f) + '\n' + result.to_string();
}

int main(int argc, char** argv)
{
    std::string filename = argv[1];
    std::ifstream f(filename);
    std::string buffer(std::istreambuf_iterator<char>(f), {});

    lexcontext ctx;
    ctx.cursor = buffer.c_str();
    ctx.loc.begin.filename = &filename;
    ctx.loc.end.filename   = &filename;

    yy::conj_parser parser(ctx);
    parser.parse();
    std::vector<function> func_list = std::move(ctx.func_list);

    for(const auto& f: func_list) std::cerr << stringify_tree(f);
}


