%skeleton "lalr1.cc"
%define parser_class_name {conj_parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%define parse.error verbose
%locations

%code requires
{
#include <map>
#include <list>
#include <regex>
#include <vector>
#include <string>
#include <sstream>
#include <cstring>
#include <iomanip>
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
    bool is_compiletime_expr() const;

    expression operator%=(expression&& b) && { return expression(ex_type::copy, std::move(b), std::move(*this)); }
};

#define o(n) \
inline bool is_##n(const identifier& i) { return i.type == id_type::n; }
ENUM_IDENTIFIERS(o)
#undef o

#define o(n) \
inline bool is_##n(const expression& e) { return e.type == ex_type::n; } \
template<typename... T> \
inline expression e_##n(T&&... args) { return expression(ex_type::n, std::forward<T>(args)...); }
ENUM_EXPRESSIONS(o)
#undef o

struct function
{
    std::string name;
    expression  code;
    unsigned num_vars = 0,     num_params = 0;
    bool     pure     = false, pure_known = false;

    expression maketemp() { expression r(identifier{id_type::variable, num_vars, "$C" + std::to_string(num_vars)}); ++num_vars; return r; }
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
|                               IDENTIFIER                  { $$ = ctx.use($1);   };
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
#line 281 "conj.y"
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
#include <memory>
#include <unordered_map>
#include <functional>
#include <numeric>
#include <set>

/* GLOBAL DATA */
std::vector<function> func_list;

// pure_fcall tells whether the given fcall expression refers
//            to a function that is known to have no side effects,
//            i.e. its only outcome is the return value.
static bool pure_fcall(const expression& exp)
{
    // Identify the called function. Note that that the function
    // may be any arbitrary expression, not only a function identifier.
    if(const auto& p = exp.params.front(); is_ident(p) && is_function(p.ident))
        if(auto called_function = p.ident.index; called_function < func_list.size())
            if(const auto& f = func_list[called_function]; f.pure_known && f.pure)
                return true;
    return false;
}

// is_pure tells whether the expression is safe to duplicate,
// or even delete if the return value is not used,
// without changing the program semantics.
bool expression::is_pure() const
{
    // if any parameter is not pure, the expression is not pure
    for(const auto& e: params) if(!e.is_pure()) return false;
    switch(type)
    {
        // Function calls are judged using a lookup.
        case ex_type::fcall: return pure_fcall(*this);
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

bool expression::is_compiletime_expr() const
{
    for(const auto& e: params) if(!e.is_compiletime_expr()) return false;
    switch(type)
    {
        case ex_type::number: case ex_type::string:
        case ex_type::add:    case ex_type::neg:    case ex_type::cand:  case ex_type::cor:
        case ex_type::comma:  case ex_type::nop:
            return true;
        case ex_type::ident:
            return is_function(ident);
        default:
            return false;
    }
}

// callv: Invokes the functor with args.
// Returns its return value, except, if func returns void, callv() returns def instead.
template<typename F, typename B, typename... A>
static decltype(auto) callv(F&& func, B&& def, A&&... args)
{
    if constexpr(std::is_invocable_r_v<B,F,A...>) { return std::forward<F>(func)(std::forward<A>(args)...); }
    else                                          { static_assert(std::is_void_v<std::invoke_result_t<F,A...>>);
                                                    std::forward<F>(func)(std::forward<A>(args)...); return std::forward<B>(def); }
}

// for_all_expr() executes the given callbacks on all sub-expressions of the given expression
// (but not on itself). The first param can be: function&, expression&, const expression&.
// The provided callbacks will be executed sequentially. Recursion is depth-first.
// If a callback returns false, the rest of following callbacks will not be executed for that expression.
// If all callbacks return true, testing ends immediately and for_all_expr returns true.
// If a callback returns void, it is treated as if it returned false.
template<typename E, typename... F>
static bool for_all_expr(E& p, bool inclusive, F&&... funcs)
{
    static_assert(std::conjunction_v<std::is_invocable<F,expression&>...>);
    return std::any_of(p.params.begin(), p.params.end(), [&](E& e) { return for_all_expr(e, true, funcs...); })
         || (inclusive && ... && callv(funcs,false,p));
}

static void FindPureFunctions()
{
    // Reset the information
    for(auto& f: func_list) f.pure_known = f.pure = false;
    // Loop until the algorithm can't find new functions to identify as pure/impure
    do {} while(std::count_if(func_list.begin(), func_list.end(), [&](function& f)
    {
        if(f.pure_known) return false;
        //std::cerr << "Identifying " << f.name << '\n';

        // The function has side-effects, if there is _any_ pointer dereference
        // in the LHS side of an assign operator, or if the function calls
        // some other function that is known to have side-effects.
        bool unknown_functions = false;
        bool side_effects      = for_all_expr(f.code, true, [&](const expression& exp)
        {
            if(is_copy(exp)) { return for_all_expr(exp.params.back(), true, is_deref); }
            if(is_fcall(exp))
            {
                const auto& e = exp.params.front();
                // Indirect function calls are always considered impure
                if(!e.is_compiletime_expr()) return true;
                // Identify the function that was called
                const auto& u = func_list[e.ident.index];
                if(u.pure_known && !u.pure) return true; // An impure function called
                if(!u.pure_known && e.ident.index != std::size_t(&f - func_list.data())) // Recursion is ignored
                {
                    //std::cerr << "Function " << f.name << " calls unknown function " << u.name << '\n';
                    unknown_functions = true; // An unknown function called
                }
            }
            return false;
        });
        // If found side-effects, mark impure. If nothing of that sort was found
        // and all called functions were known pure, mark this function also pure.
        if(side_effects || !unknown_functions)
        {
            f.pure_known = true;
            f.pure       = !side_effects;
            //std::cerr << "Function " << f.name << (f.pure ? " is pure\n" : " may have side-effects\n");
            return true; // improvements found; restart the do-while loop
        }
        return false;
    }));
    for(auto& f: func_list)
        if(!f.pure_known)
            std::cerr << "Could not figure out whether " << f.name << " is a pure function\n";
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
        [](const expression& e) { return is_loop(e); }));
    return "function " + f.name + ":\n" + stringify(f) + '\n' + result.to_string();
}

static bool equal(const expression& a, const expression& b)
{
    return (a.type == b.type)
        && (!is_ident(a) || (a.ident.type == b.ident.type && a.ident.index == b.ident.index))
        && (!is_string(a) || a.strvalue == b.strvalue)
        && (!is_number(a) || a.numvalue == b.numvalue)
        && std::equal(a.params.begin(), a.params.end(), b.params.begin(), b.params.end(), equal);
}

static void ConstantFolding(expression& e, function& f)
{
    if(is_add(e) || is_comma(e) || is_cor(e) || is_cand(e))
    {
        // Adopt all params of that same type
        for(auto j = e.params.end(); j != e.params.begin(); )
            if((--j)->type == e.type)
            {
                // Adopt all params of the parameter. Delete *j.
                auto tmp(M(j->params));
                e.params.splice(j = e.params.erase(j), std::move(tmp));
            }
        // No need to do this recursively, it has already been done recursively
    }

    // If an assign operator (copy) is used as a parameter
    // to any other kind of expression than a comma or an addrof,
    // create a comma sequence, such that e.g. x + 3 + (y=4) is replaced with x + 3 + (y=4, 4).
    // If the RHS of the assign has side-effects, use a temporary:
    // x + (y = f()) becomes x + (temp=f(), y = temp, temp)
    // This helps bring out the RHS into the outer levels of optimizations,
    // because the expression will be converted into (y=4, x+3+4) etc.
    // For while-loops, only the first operand will be inspected; the rest is treated as comma.
    if(!is_comma(e) && !is_addrof(e) && !e.params.empty())
        for(auto i = e.params.begin(), j = (is_loop(e) ? std::next(i) : e.params.end()); i != j; ++i)
            if(is_copy(*i))
            {
                auto assign = M(*i); *i = e_comma();
                if(assign.params.front().is_compiletime_expr())
                {
                    i->params.push_back(C(assign.params.front()));
                    i->params.push_front(M(assign));
                }
                else
                {
                    expression temp = f.maketemp();
                    i->params.push_back(C(temp)                 %= M(assign.params.front()));
                    i->params.push_back(M(assign.params.back()) %= C(temp));
                    i->params.push_back(M(temp));
                }
            }

    // If expr has multiple params, such as in a function calls,
    // and any of those parameters are comma expressions,
    // keep only the last value in each comma expression.
    //
    // Convert e.g.      func((a,b,c), (d,e,f), (g,h,i))
    //               --> (a,b, temp=c, d,e, temp2=f, g,h, func(temp,temp2,i))
    //
    // This way, expr itself becomes a comma expression,
    // providing the same optimization opportunity to the parent expression.
    //
    // Take care to preserve execution order.
    // For "loop", nothing is hoisted because everything must be re-executed every loop iteration.
    if(std::find_if(e.params.begin(), e.params.end(), is_comma) != e.params.end())
    {
        // For conditional execution, only the first parameter is operated on, because
        // the rest of them are only executed depending on the value of the first.
        auto end = (is_cand(e) || is_cor(e) || is_loop(e)) ? std::next(e.params.begin()) : e.params.end();
        // Move the "end" backwards until we hit a comma parameter.
        for(; end != e.params.begin(); --end)
        {
            auto prev = std::prev(end);
            if(is_comma(*prev) && prev->params.size() > 1) break;
        }
        expr_vec comma_params;
        for(expr_vec::iterator i = e.params.begin(); i != end; ++i)
        {
            if(std::next(i) == end) // last?
            {
                if(is_comma(*i) && i->params.size() > 1)
                    comma_params.splice(comma_params.end(), i->params, i->params.begin(), std::prev(i->params.end()));
            }
            else if(!i->is_compiletime_expr())
            {
                expression temp = f.maketemp();
                if(is_comma(*i) && i->params.size() > 1)
                    comma_params.splice(comma_params.end(), i->params, i->params.begin(), std::prev(i->params.end()));
                comma_params.insert(comma_params.end(), C(temp) %= M(*i));
                *i = M(temp);
            }
        }
        if(!comma_params.empty())
        {
            // If the condition to a "loop" statement is a comma expression, replicate
            // the expression to make it better optimizable:
            //           while(a,b,c) { code }
            // --> a; b; while(c)     { code; a; b; }
            if(is_loop(e)) { for(auto& f: comma_params) e.params.push_back(C(f)); }
            comma_params.push_back(M(e));
            e = e_comma(M(comma_params));
        }
    }

    switch(e.type)
    {
        case ex_type::add:
        {
            // Count the sum of literals
            long tmp = std::accumulate(e.params.begin(), e.params.end(), 0l,
                                       [](long n,auto&p){ return is_number(p) ? n + p.numvalue : n; });
            // And remove them
            e.params.remove_if(is_number);
            // Adopt all negated adds: x + -(y + z) --> x + -(y) + -(z)
            for(auto j = e.params.begin(); j != e.params.end(); ++j)
                if(is_neg(*j) && is_add(j->params.front()))
                {
                    auto tmp(std::move(j->params.front().params));
                    for(auto& p: tmp) p = e_neg(M(p));
                    e.params.splice(j = e.params.erase(j), std::move(tmp));
                }
            // Readd the literal parameter if nonzero.
            if(tmp != 0) e.params.push_back(tmp);
            // Count inverted parameters. If the number is greater
            // than the number of non-inverted parameters, flip the inversion
            // and invert the sum. E.g. -(a) + -(b) + -(c) + d --> -(a + b + c + -(d))
            if(std::count_if(e.params.begin(), e.params.end(), is_neg) > long(e.params.size()/2))
            {
                for(auto& p: e.params) p = e_neg(M(p));
                e = e_neg(M(e));
            }
            break;
        }
        case ex_type::neg:
            // If the parameter is a literal, replace with negated literal
            if(is_number(e.params.front()))   e = -e.params.front().numvalue;
            else if(is_neg(e.params.front())) e = C(M(e.params.front().params.front()));
            break;
        case ex_type::eq:
            if(is_number(e.params.front()) && is_number(e.params.back()))
                e = long(e.params.front().numvalue == e.params.back().numvalue);
            else if(equal(e.params.front(), e.params.back()) && e.params.front().is_pure())
                e = 1l;
            break;
        case ex_type::deref:
            // Convert deref(addrof(x)) into x
            if(is_addrof(e.params.front())) e = C(M(e.params.front().params.front()));
            break;
        case ex_type::addrof:
            // Convert addrof(deref(x)) into x ; this is meaningful when x is a pointer
            if(is_deref(e.params.front())) e = C(M(e.params.front().params.front()));
            break;
        case ex_type::cand:
        case ex_type::cor:
        {
            auto value_kind = is_cand(e) ? [](long v){ return v!=0; } : [](long v){ return v==0; };
            // With &&, delete nonzero literals. With ||, delete zero literals.
            e.params.erase(std::remove_if(e.params.begin(), e.params.end(),
                                          [&](expression& p) { return is_number(p) && value_kind(p.numvalue); }),
                           e.params.end());
            // Found zero (&&) or nonzero (||) --> delete all remaining params and all *preceding* pure params
            if(auto i = std::find_if(e.params.begin(), e.params.end(), [&](const expression& p)
                                     { return is_number(p) && !value_kind(p.numvalue); });
               i != e.params.end())
            {
                // Find the last non-pure param before that constant
                while(i != e.params.begin() && std::prev(i)->is_pure()) { --i; }
                // Delete everything that follows that
                e.params.erase(i, e.params.end());
                // Replace with a comma stmt that produces 0 (&&) or 1 (||)
                e = e_comma(M(e), is_cand(e) ? 0l : 1l);
            }
            break;
        }
        case ex_type::copy:
        {
            auto& tgt = e.params.back(), &src = e.params.front();
            // If an assign-statement assigns into itself, and the expression has no side effects,
            // replace with the lhs.
            if(equal(tgt, src) && tgt.is_pure())
                e = C(M(tgt));
            // If the target expression of the assign-statement is also used in the source expression,
            // replace the target-param reference with a temporary variable. A new temporary is created
            // for every repeated reference in case the expression is question is impure.
            else
            {
                expr_vec comma_params;
                for_all_expr(src, true, [&](auto& e)
                                        { if(equal(e, tgt)) comma_params.push_back(C(e = f.maketemp()) %= C(tgt)); });
                if(!comma_params.empty())
                {
                    comma_params.push_back(M(e));
                    e = e_comma(M(comma_params));
                }
            }
            break;
        }
        case ex_type::loop:
            // If the loop condition is a literal zero, delete the code that is never executed.
            if(is_number(e.params.front()) && !e.params.front().numvalue) { e = e_nop(); break; }
            // Process the contingent code like a comma statement
            [[fallthrough]];
        case ex_type::comma:
            for(auto i = e.params.begin(); i != e.params.end(); )
            {
                // For while(), leave the condition expression untouched
                // For comma,   leave the *final* expression untouched
                if(is_loop(e))
                    { if(i == e.params.begin()) { ++i; continue; } }
                else
                    { if(std::next(i) == e.params.end()) break; }

                // Delete all pure params except the last
                if(i->is_pure())
                    { i = e.params.erase(i); }
                else switch(i->type)
                {
                    // Adopt members from any sub-expression where the result is not needed
                    default:
                        ++i;
                        break;
                    case ex_type::fcall:
                        // Even if the function call is not pure, it might be
                        // because of the parameters, not the function itself.
                        // Check if only need to keep the parameters.
                        if(!pure_fcall(e)) { ++i; break; }
                        [[fallthrough]];

                    case ex_type::add:
                    case ex_type::neg:
                    case ex_type::eq:
                    case ex_type::addrof:
                    case ex_type::deref:
                    case ex_type::comma:
                        // Adopt all params of the parameter. Delete *i.
                        auto tmp(std::move(i->params));
                        e.params.splice(i = e.params.erase(i), std::move(tmp));
                }
            }
            // Delete all params following a "return" statement or following an infinite loop
            if(auto r = std::find_if(e.params.begin(), e.params.end(),
                                  [](const expression& e) { return is_ret(e)
                                                                || (is_loop(e)
                                                                    && is_number(e.params.front())
                                                                    && e.params.front().numvalue != 0); });
               r != e.params.end() && ++r != e.params.end())
            {
                //std::cerr << std::distance(r, e.params.end()) << " dead expressions deleted\n";
                e.params.erase(r, e.params.end());
            }

            // If the last element in the list is the same as the preceding assign-target,
            // delete the last element. e.g. x = (a=3, a)  -->  x = (a=3)
            if(e.params.size() >= 2)
            {
                auto& last = e.params.back(), &prev = *std::next(e.params.rbegin());
                if(is_copy(prev) && equal(prev.params.back(), last))
                    e.params.pop_back();
            }
            if(e.params.size() == 1 && !is_loop(e))
            {
                // Replace with param
                e = C(M(e.params.front()));
            }
            break;

        default:
            break;
    }

    // If the type is add, cand or cor, and there remains only one operand, replace with the operand
    switch(e.params.size())
    {
        case 1: if(is_add(e))                    e = C(M(e.params.front()));
                else if(is_cor(e) || is_cand(e)) e = e_eq(e_eq(M(e.params.front()), 0l), 0l); // bool-cast
                break;
        case 0: if(is_add(e) || is_cor(e))       e = 0l;
                else if(is_cand(e))              e = 1l;
    }
}

static void DoConstantFolding()
{
    do {} while(std::any_of(func_list.begin(), func_list.end(), [&](function& f)
    {
        // Recalculate function purity; the status may have changed
        // as unreachable statements have been deleted etc.
        FindPureFunctions();

        std::string text_before = stringify(f);
        //std::cerr << "Before: " << text_before << '\n';
        //std::cerr << stringify_tree(f);

        for_all_expr(f.code, true, [&](expression& e){ ConstantFolding(e,f); });
        return stringify(f) != text_before;
    }));
}

#include "transform_iterator.hh"
#include "shuffle.hh"
#include <string_view>
#include <optional>
#include <variant>

#define ENUM_STATEMENTS(o)  /* flags: #write_params, has_side_effects, special constructor; name */ \
    o(0b000,nop)        /* placeholder that does nothing */ \
    o(0b101,init)       /* p0 <- &IDENT + value (assign a pointer to name resource with offset) */ \
    o(0b100,add)        /* p0 <- p1 + p2           */ \
    o(0b100,neg)        /* p0 <- -p1               */ \
    o(0b100,copy)       /* p0 <- p1      (assign a copy of another var) */ \
    o(0b100,read)       /* p0 <- *p1     (reading dereference) */ \
    o(0b010,write)      /* *p0 <- p1     (writing dereference) */ \
    o(0b100,eq)         /* p0 <- p1 == p2          */ \
    o(0b011,ifnz)       /* if(p0 != 0) JMP branch  */ \
    o(0b110,fcall)      /* p0 <- CALL(p1, <LIST>)  */ \
    o(0b010,ret)        /* RETURN p0;              */

#define o(f,n) n,
#define p(f,n) f,
#define q(f,n) #n,
enum class st_type { ENUM_STATEMENTS(o) };
static constexpr unsigned char st_flags[]     { ENUM_STATEMENTS(p) };
static constexpr const char* const st_names[] { ENUM_STATEMENTS(q) };
#undef q
#undef p
#undef o

template<typename T, typename... Bad>
using forbid1_t = std::enable_if_t<(... && !std::is_same_v<Bad, std::decay_t<T>>)>;
template<typename...U>
struct forbid_t { template<typename...T> using in = std::void_t<forbid1_t<T,U...>...>; };

template<typename Iterator, typename PointedType, typename Category>
using require_iterator_t = std::enable_if_t
<   std::is_convertible_v<typename std::iterator_traits<Iterator>::value_type,       PointedType>
 && std::is_convertible_v<typename std::iterator_traits<Iterator>::iterator_category,Category>>;

struct statement
{
    typedef unsigned reg_type;
    static constexpr reg_type nowhere = ~reg_type();
    static constexpr reg_type no_mask = ~(nowhere >> 3);

    st_type               type{st_type::nop};
    std::string           ident{};         // For init: reference to globals, empty=none
    long                  value{};         // For init: literal/offset
    std::vector<reg_type> params{};        // Variable indexes
    statement*            next{nullptr};   // Pointer to next stmt in the chain. nullptr = last.
    statement*            cond{nullptr};   // For ifnz; If var[p0] <> 0, cond overrides next.

    // Construct with type and zero or more register params
    statement() {}
    template<class...T, class=forbid_t<st_type,long,statement*>::in<T...>>
    statement(st_type t, T&&...r)                  : statement(std::forward<T>(r)...) { type=t; }

    template<class...T>
    statement(reg_type tgt, T&&...r)               : statement(&tgt, &tgt+1,  std::forward<T>(r)...) {}

    // Special types that also force the statement type:
    template<class...T, class=forbid_t<st_type,long>::in<T...>>
    statement(std::string_view i, long v, T&&...r) : statement(st_type::init, std::forward<T>(r)...) { ident=i; value=v; }

    template<class...T, class=forbid_t<st_type,statement*>::in<T...>>
    statement(statement* b, T&&...r)               : statement(st_type::ifnz, std::forward<T>(r)...) { cond=b; }

    // An iterator range can be used to assign register params
    template<class...T, class It, class=require_iterator_t<It, reg_type, std::input_iterator_tag>>
    statement(It begin, It end, T&&...r)           : statement(std::forward<T>(r)...) { params.insert(params.begin(), begin, end); }

    template<class...T>
    void Reinit(T&&... r) // Reinitialize statement as a different one, without changing ->next
    {
        auto n = next;
        *this = statement(std::forward<T>(r)...);
        next = n;
    }

    template<typename F>
    bool ForAllRegs(F&& func, std::size_t begin=0, std::size_t end = ~size_t())
    {
        return std::any_of(params.begin()+begin, params.begin()+std::min(end,params.size()),
                           [&](reg_type& p) { return p < no_mask && callv(func,false,p, &p-params.data()); });
    }

    template<typename F>
    auto ForAllWriteRegs(F&& func) { return ForAllRegs(std::forward<F>(func), 0, NumWriteRegs()); }
    template<typename F>
    auto ForAllReadRegs(F&& func)  { return ForAllRegs(std::forward<F>(func), NumWriteRegs(), params.size()); }

    reg_type& lhs() { return params.front(); }
    reg_type& rhs() { return params.back();  }

    std::size_t NumWriteRegs() const { return st_flags[unsigned(type)]/4; }
    bool HasSideEffects() const      { return st_flags[unsigned(type)]&2; }

    void Dump(std::ostream& out) const;
};

std::map<std::pair<std::string, long>, statement::reg_type> number_constants;
std::map<statement::reg_type, std::pair<std::string, long>> number_constants_reverse;

void statement::Dump(std::ostream& out) const
{
    out << '\t' << st_names[unsigned(type)] << '\t';
    for(auto u: params)
    {
        if(u == nowhere) out << " nil";
        else if(u >= no_mask)
        {
            auto i = number_constants_reverse.find(u);
            if(i != number_constants_reverse.end())
            {
                if(i->second.first.empty()) out << ' ' << i->second.second;
                else if(!i->second.second) out << " \"" << i->second.first << '"';
                else out << " \"" << i->second.first << "\"+" << i->second.second;
            }
            else
                out << " R" << u;
        }
        else out << " R" << u;
    }
    if(type == st_type::init)  { out << " \"" << ident << "\" " << value; }
}

#define o(_,n) \
inline bool is_##n(const statement& s) { return s.type == st_type::n; }
ENUM_STATEMENTS(o)
#undef o

template<typename F>
static void ForAllBitsIn(std::uint64_t mask, F&& func)
{
    unsigned idx = 0;
    if(mask) for(;;)
    {
        if(mask&1) { func(idx); mask &= ~1; if(!mask) break; }
        unsigned skip=1;
        #ifdef __GNUC__
        skip = __builtin_ctzll(mask);
        #endif
        mask >>= skip; idx += skip;
    }
}

template<typename...T>
static constexpr std::enable_if_t<(... && std::is_same_v<int,std::decay_t<T>>), std::uint_fast64_t>
                 BitMaskFrom(T... n)
    { return std::uint_fast64_t(((1ull << n) | ...)); }

struct compilation
{
    std::vector<std::unique_ptr<statement>> all_statements; // All statements

    template<typename... T>
    statement* CreateStatement(T&&... args) { return CreateStatement(new statement(std::forward<T>(args)...)); }
    statement* CreateStatement(statement*s) { return all_statements.emplace_back(s).get(); }

    #define o(f,n) /* f: flag that indicates if there's a special constructor that doesn't need the type */ \
    template<typename... T> \
    inline statement* s_##n(T&&... args) { if constexpr((f)&1)return CreateStatement(std::forward<T>(args)...); \
                                           else return CreateStatement(st_type::n, std::forward<T>(args)...); }
    ENUM_STATEMENTS(o)
    #undef o

    std::map<std::string, std::size_t> function_parameters; // Number of parameters in each function
    std::map<std::string, statement*>  entry_points;

    std::string                                                 string_constants;

    void BuildStrings()
    {
        std::vector<std::string> strings;
        for(auto& f: func_list)
            for_all_expr(f.code, true, is_string, [&](const expression& exp)
            {
                strings.push_back(exp.strvalue + '\0');
            });
        // Sort by length, longest first
        std::sort(strings.begin(), strings.end(),
            [](const std::string& a, const std::string& b)
            {
                return a.size()==b.size() ? (a<b) : (a.size()>b.size());
            });
        // Produce a single string that contains all the original strings
        for(const auto& s: strings)
            if(string_constants.find(s) == string_constants.npos)
                string_constants += s;
    }

    using parameter_source = std::pair<std::string, std::size_t>; // Function name & parameter index
    using undefined_source = std::nullptr_t;                      // Dummy lessthan-comparable type
    using source_type      = std::variant<undefined_source, statement*, parameter_source>;

    static statement* is_statement(const source_type& s) { auto r = std::get_if<statement*>(&s); return r ? *r : nullptr; }
    static void is_statement(const statement*) = delete;

    struct AccessInfo
    {
        /* Info about registers, for each statement: */
        using StateType = std::vector<std::set<source_type>>;
        struct info
        {
            // For each parameter;
            //   if write param, list statements that directly depend on this write
            //   if read  param, list statements that can be possible sources of this value
            StateType params;
            // For all registers, indexed by register number
            StateType presence;
        };
        std::map<statement*, info> data;
    private:
        void Trace(statement* where, StateType&& state, bool follow_copies, bool include_ifnz_as_writer)
        {
            // Add all information from state into the data
            auto& mydata = data[where];

            // For this statement, add information about where
            // the values in each register come from at this point
            std::size_t changes{};
            for(std::size_t r=0; r<state.size(); ++r)
                for(const auto& s: state[r])
                    changes += mydata.presence[r].insert(s).second;

            if(follow_copies && is_copy(*where))
            {
                if(!changes) return;

                // After this insn, sources of tgt_reg are the same as sources of src_reg
                if(where->rhs() < statement::no_mask)
                    state[where->lhs()] = state[where->rhs()];
                else
                    state[where->lhs()] = {undefined_source()};
            }
            else
            {
                where->ForAllReadRegs([&](auto regno, auto index)
                {
                    changes += std::count_if(state[regno].begin(), state[regno].end(), [&](const auto& source)
                    {
                        auto writer = is_statement(source);
                        // Add writer info for the reader
                        return mydata.params[index].insert(source).second
                        // Add reader info for the writer, if it's a statement (and e.g. not a parameter)
                        + (writer && writer->ForAllWriteRegs([&](auto wregno, auto windex)
                            { return wregno == regno && data[writer].params[windex].insert(where).second; }));
                    });
                });
                if(!changes) return;

                // After this stmt, where is the only source for any register that it wrote into.
                where->ForAllWriteRegs([&](auto wregno, auto) { if(wregno < statement::no_mask) state[wregno] = { where }; });

                // If the statement is IFNZ and include_ifnz_as_writer is set,
                // we save the test knowledge as well.
                // The lhs of the IFNZ is a read-register, but based on the branch taken,
                // we can infer whether its value was zero or nonzero, and for the purposes
                // of some optimizations, this inference counts as a source of a value.
                if(include_ifnz_as_writer && is_ifnz(*where)) { if(where->lhs() < statement::no_mask) state[where->lhs()] = { where }; };
            }

            if(is_ifnz(*where) && where->cond) // pass copies of the arrays into cond processing
                Trace(where->cond, where->next ? StateType(state) : std::move(state), follow_copies, include_ifnz_as_writer);

            if(where->next) // Move the arrays into the tail processing.
                Trace(where->next, std::move(state), follow_copies, include_ifnz_as_writer);
        }
    public:
        // For every reachable insn, build a map where the value might be set for that register
        //   If follow_copies = true, tracks sources of values in register across COPY statements.
        //   If include_ifnz_as_writer = true, IFNZ statements are treated as write instructions.
        AccessInfo(const compilation& c, bool follow_copies, bool include_ifnz_as_writer)
        {
            // Calculate the list of register numbers to track
            std::size_t max_register_number = 0;
            for(const auto& s: c.all_statements)
                s->ForAllRegs([&](auto r, auto) {
                   max_register_number = std::max(max_register_number, std::size_t(r)+1); });
            for(const auto& f: c.function_parameters)
                max_register_number = std::max(max_register_number, f.second);

            // Initialize structure for all statements, including potentially unreachable ones
            for(const auto& s: c.all_statements)
                data.emplace(s.get(), info{ StateType(s->params.size()), StateType(max_register_number)});

            // Begin from all entry points
            for(const auto& [name,st]: c.entry_points)
            {
                // Initialize state for all known register numbers
                StateType state(max_register_number);

                std::size_t num_params = 0;
                if(auto i = c.function_parameters.find(name); i != c.function_parameters.end())
                    num_params = i->second;

                // At the function entry, all registers are undefined,
                // except those containing function parameters.
                for(statement::reg_type r = 0; r < max_register_number; ++r)
                    if(r < num_params)
                        state[r].insert(parameter_source{name, r});
                    else
                        state[r].insert(undefined_source{});

                // Trace recursively from this starting point.
                Trace(st, std::move(state), follow_copies, include_ifnz_as_writer);
            }
        }
    };

    std::map<statement*, AccessInfo::info> GenerateAccessInfo(bool follow_copies, bool include_ifnz_as_writer=false) const
    {
        return std::move(AccessInfo(*this, follow_copies, include_ifnz_as_writer).data);
    }

    void Dump(std::ostream& out)
    {
        struct data
        {
            std::vector<std::string> labels{};
            std::size_t done{}, referred{}; // bool would be fine if permitted by c++17
        };
        std::map<statement*, data> statistics;
        std::list<statement*> remaining_statements;

        auto add_label = [l=0lu](data& d) mutable { d.labels.push_back('L' + std::to_string(l++)); };

        for(const auto& [name,st]: entry_points)
        {
            remaining_statements.push_back(st);
            statistics[st].labels.push_back(name);
        }
        for(const auto& s: all_statements)
        {
            if(s->next) { auto& t = statistics[s->next]; if(t.labels.empty() && t.referred++) add_label(t); }
            if(s->cond) { auto& t = statistics[s->cond]; if(t.labels.empty()) add_label(t); }
        }
        while(!remaining_statements.empty())
        {
            statement* chain = remaining_statements.front(); remaining_statements.pop_front();
            for(bool needs_jmp = false; chain != nullptr; chain = chain->next, needs_jmp = true)
            {
                auto& stats = statistics[chain];
                if(stats.done++)
                {
                    if(needs_jmp) { out << "\tJMP " << stats.labels.front() << '\n'; }
                    break;
                }

                for(const auto& l: stats.labels) out << l << ":\n";
                chain->Dump(out);
                if(chain->cond)
                {
                    auto& branch_stats = statistics[chain->cond];
                    out << ", JMP " << branch_stats.labels.front();
                    if(!branch_stats.done) { remaining_statements.push_front(chain->cond); }
                }
                out << '\n';
            }
        }
    }

    bool Optimize_DeleteNOPs()
    {
        // Delete the ->next from RET statements
        for(auto& s: all_statements)
            if(is_ret(*s) && s->next)
                s->next = nullptr;

        std::size_t n_elisions = 0;
        const auto info = GenerateAccessInfo(false); // Don't follow copies
        // Replace all references to NOP nodes with references to their targets.
        auto ReduceNOPchain = [&](statement*& p)
        {
            // If p points to a NOP node, change p to point into the NOP's followup instead.
            // The counter scheme is to guard against infinite loops.
            std::size_t counter=0, counter_max=1;
            for(statement* seen = p; p; p = p->next)
            {
                // These nodes are treated as NOPs:
                //  - NOP statements, obviously
                //  - IFNZ statements (branches) with both branches (COND, NEXT) pointing the same
                //  - COPY statements where target is same as source
                //  - Any modify-instructions where the target register is never read thereafter,
                //    and that does not have inherent side-effects
                if(!is_nop(*p)
                && !(is_ifnz(*p) && (p->cond == p->next || !p->cond))
                && !(is_copy(*p) && (p->lhs() == p->rhs()))
                && (p->HasSideEffects() || p->ForAllWriteRegs([&d=info.find(p)->second.params](auto, auto windex)
                                                              { return !d[windex].empty(); }))
                  ) break;

                if(p->next == seen) { p->Reinit(st_type::nop); break; } // Prevent infinite loop
                if(++counter == counter_max) { seen = p; counter = 0; counter_max <<= 1; }
                ++n_elisions;
            }
        };
        // Process everything that _points_ to a statement
        for(auto& t: entry_points)   { ReduceNOPchain(t.second); }
        for(auto& s: all_statements) { ReduceNOPchain(s->next); if(is_ifnz(*s)) ReduceNOPchain(s->cond); }

        // If there is a write-instruction that writes to a register that is never read,
        // but the instruction has side effects and cannot be removed,
        // replace the target register with a special tag that indicates unused value.
        for(const auto& [s,p]: info)
            if(s->HasSideEffects())
                s->ForAllWriteRegs([&](auto& r, auto windex)
                    { if(p.params[windex].empty()) r = statement::nowhere; });

        if(n_elisions) std::cerr << n_elisions << " NOPs elided\n";
        return n_elisions;
    }

    bool Optimize_GC()
    {
        std::size_t n_erased = 0;
        for(;;)
        {
            // Collect a list of all statements that can be visited by following next/cond pointers.
            std::list<statement*> pending;
            // Add all entry points
            for(const auto& s: entry_points) if(s.second) pending.push_back(s.second);
            // Track everything that can be reached from entry points
            std::set<statement*> visited;
            while(!pending.empty())
            {
                statement* chain = pending.front(); pending.pop_front();
                if(!visited.insert(chain).second) continue;
                if(is_ifnz(*chain) && chain->cond) pending.push_back(chain->cond);
                if(chain->next) pending.push_back(chain->next);
            }

            // Delete all unreachable statements. These are the ones that were never visited.
            auto endptr = std::remove_if(all_statements.begin(), all_statements.end(),
                [&](const auto& s) { return visited.find(s.get()) == visited.end(); });

            auto size_reduced_by = std::distance(endptr, all_statements.end());
            if(!size_reduced_by) break; // End GC if nothing was removed

            all_statements.erase(endptr, all_statements.end());
            n_erased += size_reduced_by;
            // Run the GC again, because GC'd nodes may refer to other nodes
            // that prevented them from being GC'd, but are now available
        }
        if(n_erased) { std::cerr << n_erased << " unreferred statements deleted\n"; }
        return n_erased;
    }

    bool Optimize_JumpThreading()
    {
        std::size_t n_changes = 0;
        for(const auto& s: all_statements)
        {
            // If a test for a register is immediately followed by another test
            // for the same register, skip the second test
            while(is_ifnz(*s) && s->next && is_ifnz(*s->next) && s->next->lhs() == s->lhs() && s->next != s->next->next)
            {
                // If it didn't match the first condition, it won't match the second one either
                std::cerr << "IFNZ->IFNZ mismatch threaded\n";
                s->next = s->next->next;
                ++n_changes;
            }
            while(is_ifnz(*s) && s->cond && is_ifnz(*s->cond) && s->cond->lhs() == s->lhs() && s->cond != s->cond->cond)
            {
                // If it DID match the first condition, it will also match the second one
                std::cerr << "IFNZ->IFNZ match threaded\n";
                s->cond = s->cond->cond;
                ++n_changes;
            }

            // If a literal load is immediately followed by an IFNZ, hardcode the jump.
            while(is_init(*s) && s->ident.empty() && s->next && is_ifnz(*s->next) && s->next->lhs() == s->lhs())
            {
                s->next = s->value ? s->next->cond : s->next->next;
                std::cerr << "LIT->IFNZ branch elided (type 1)\n";
                ++n_changes;
            }

            // If a COPY instruction is immediately followed by a RET that consumes
            // the COPY result, change the COPY into RET and rewrite operands.
            if(is_copy(*s) && s->next && is_ret(*s->next))
            {
                s->Reinit(st_type::ret, s->rhs());
                std::cerr << "COPY-RET elision\n";
                ++n_changes;
            }
        }
        return n_changes;
    }

    bool Optimize_MergeTrees()
    {
        // Merge identical instructions
        std::size_t n_merged = 0;
        // Index all statements by a hash
        std::multimap<std::size_t, statement*> hashes;
        for(const auto& s: all_statements)
        {
            // Build the hash from the sum of hashes of elements
            auto h = [](auto p) { return std::hash<std::decay_t<decltype(p)>>()(p); };
            auto hash = h(long(s->type)) + h(s->next) + h(s->value) + h(s->ident) + h(s->cond);
            // Add in the hashes of all param-register numbers
            for(auto p: s->params) hash = h(hash) ^ h(p);
            hashes.emplace(hash, s.get());
        }
        // Process everything that _points_ to a statement
        std::multimap<statement*, statement**> src;
        for(auto& p: entry_points)   { src.emplace(p.second, &p.second); }
        for(auto& b: all_statements) { if(b->next) src.emplace(b->next, &b->next);
                                       if(b->cond) src.emplace(b->cond, &b->cond); }
        // Process all distinct hashes
        for(auto ai = hashes.cbegin(); ai != hashes.cend(); ++ai)
        {
            // If another statement matches this one, replace all
            // references to that statement with references to this one.
            for(auto bi = ai; ++bi != hashes.cend() && bi->first == ai->first; )
            {
                // Verify actual match, because hashes can collide
                const auto& a = *ai->second, &b = *bi->second;
                if(std::tie(a.type,a.next, a.params, a.value,a.ident,a.cond)
                == std::tie(b.type,b.next, b.params, b.value,b.ident,b.cond))
                {
                    // Match! Replace all references to insn B with references to insn A
                    for(auto ci = src.lower_bound(bi->second); ci != src.end() && ci->first == bi->second; )
                        { *ci->second = ai->second; ++n_merged; ci = src.erase(ci); }
                }
            }
        }
        if(n_merged) std::cerr << n_merged << " trees merged\n";
        return n_merged;
    }

    static bool Reach(const statement* from, const statement* to, const statement* exclude = nullptr)
    {
        // Collect a list of all statements that can be visited by following next/cond pointers.
        std::set<const statement*> visited;
        std::list<const statement*> pending;
        // Add all entry points
        pending.push_back(from);
        // Track everything that can be reached from entry points
        while(!pending.empty())
        {
            const statement* chain = pending.front(); pending.pop_front();
            if(chain == exclude || !visited.insert(chain).second) continue;
            if(chain == to) return true;
            if(is_ifnz(*chain) && chain->cond) pending.push_back(chain->cond);
            if(chain->next) pending.push_back(chain->next);
        }
        return false;
    }

    bool Optimize_ConvertLiterals()
    {
        std::size_t n_upd = 0;
        auto MakeRegFromLiteral = [&](std::pair<std::string,long>&& value)
        {
            statement::reg_type r = number_constants.size() | statement::no_mask;
            auto i2 = number_constants.emplace(value, r);
            if(!i2.second) r = i2.first->second;
            else number_constants_reverse.emplace(r, std::move(value));
            ++n_upd;
            return r;
        };
        for(unsigned round=1; round<=2; ++round)
        {
            const auto info = GenerateAccessInfo(true, round==2); // Follow copies
            for(const auto& [st,p]: info)
            {
                auto GetLiteral = [&](const auto& sources, bool exact_value = true)
                {
                    // exact_value=false: Only care whether value is zero or nonzero
                    std::optional<std::pair<std::string,long>> op;
                    for(const auto& src: sources)
                    {
                        std::pair<std::string,long> myvalue;
                        switch(auto s = is_statement(src); (s && s != st) ? s->type : st_type::nop)
                        {
                            case st_type::init:
                                myvalue = (s->ident.empty() && !exact_value)
                                              ? std::pair<std::string,long>{std::string{}, !!s->value}
                                              : std::pair<std::string,long>{s->ident, s->value};
                                break;

                            case st_type::ifnz:
                                myvalue.second =     Reach(s->cond, st, s);
                                if(myvalue.second == Reach(s->next, st, s)) { goto reject; }
                                // If exact value is needed, verify that *all* the sources to the IFNZ
                                // are EQ statements (where the only possible nonzero outcome is 1)
                                if(myvalue.second && exact_value)
                                    if(const auto& if_src = info.find(s)->second.params[0];
                                       !std::all_of(if_src.begin(), if_src.end(),
                                            [&](const auto& src) { auto s = is_statement(src); return s && is_eq(*s); }))
                                        goto reject;
                                break;

                            default: goto reject;
                        }
                        if(op && op.value() != myvalue) { reject: op.reset(); break; }
                        op = myvalue;
                    }
                    return op;
                };

                st->ForAllReadRegs([&](auto& regno, auto index) {
                    if(auto op = GetLiteral(p.params[index], !is_ifnz(*st)))
                        regno = MakeRegFromLiteral(std::move(op.value()));
                });
                if(is_init(*st) && is_ret(*st->next) && st->next->rhs() == st->lhs())
                {
                    st->Reinit(st_type::ret, MakeRegFromLiteral({st->ident, st->value}));
                    st->next = nullptr;
                }
            }
        }
        if(n_upd) { std::cerr << n_upd << " literal expressions converted\n"; }
        return n_upd;
    }

    bool Optimize_Simplify()
    {
        for(unsigned round=1; round<=2; ++round)
        {
            const auto info = GenerateAccessInfo(true, round==2); // Follow copies

            std::size_t n_upd = 0;
            bool        elisions = false;
            for(const auto& [st,p]: info)
            {
                auto GetLiteral = [&](const auto& sources, bool exact_value = true)
                {
                    // exact_value=false: Only care whether value is zero or nonzero
                    std::optional<long> op;
                    for(const auto& src: sources)
                    {
                        long myvalue;
                        switch(auto s = is_statement(src); (s && s != st) ? s->type : st_type::nop)
                        {
                            case st_type::init:
                                if(!s->ident.empty()) { goto reject; }
                                myvalue = exact_value ? s->value : !!s->value;
                                break;

                            case st_type::ifnz:
                                myvalue =     Reach(s->cond, st, s);
                                if(myvalue == Reach(s->next, st, s)) { goto reject; }
                                // If exact value is needed, verify that *all* the sources to the IFNZ
                                // are EQ statements (where the only possible nonzero outcome is 1)
                                if(myvalue && exact_value)
                                    if(const auto& if_src = info.find(s)->second.params[0];
                                       !std::all_of(if_src.begin(), if_src.end(),
                                            [&](const auto& src) { auto s = is_statement(src); return s && is_eq(*s); }))
                                        goto reject;
                                break;

                            default: goto reject;
                        }
                        if(op && op.value() != myvalue) { reject: op.reset(); break; }
                        op = myvalue;
                    }
                    return op;
                };

                switch(st->type)
                {
                    case st_type::neg:
                        // If the source operand can only come from an integer literal, replace with negated literal
                        if(auto op1 = GetLiteral(p.params[1])) { st->Reinit(st->lhs(), "", -op1.value()); ++n_upd; }
                        break;

                    case st_type::add:
                        // If both operands can only come from an integer literal, replace with sum
                        if(auto op1 = GetLiteral(p.params[1]), op2 = GetLiteral(p.params[2]);
                           op1 && op2)                 { st->Reinit(st->lhs(), "", op1.value() + op2.value()); ++n_upd; }
                        else if(op1 && op1.value()==0) { st->Reinit(st_type::copy, st->lhs(), st->params[2]); ++n_upd; }
                        else if(op2 && op2.value()==0) { st->Reinit(st_type::copy, st->lhs(), st->params[1]); ++n_upd; }
                        break;

                    case st_type::eq:
                        // If both operands come from the same source, replace with a 1-literal
                        if(p.params[1] == p.params[2]) { st->Reinit(st->lhs(), "", 1l); ++n_upd; }
                        else if(auto op1 = GetLiteral(p.params[1]), op2 = GetLiteral(p.params[2]);
                                op1 && op2)            { st->Reinit(st->lhs(), "", op1.value() == op2.value()); ++n_upd; }
                        break;

                    case st_type::ifnz:
                    {
                        if(auto op = GetLiteral(p.params[0], false))
                        {
                            // If the value of the operand is known at compile time,
                            // change the IFNZ into a NOP and choose the appropriate next-pointer.
                            std::cerr << "LIT->IFNZ branch elided (type 2)\n";
                            if(op.value()) { st->next = st->cond; }
                            st->Reinit(st_type::nop);
                            elisions = true;
                        }

                        // If an IFNZ instruction is applied on a register
                        // that contains the result of an EQ instruction,
                        // and one of the sources to the EQ instruction
                        // is a register that contains a literal zero,
                        // replace the source of IFNZ with the other source of EQ,
                        // and swap the next & cond of the IFNZ.
                        // This optimizes "(x==0)!=0 ? a : b" into "x!=0 ? b : a", eliding the EQ.
                        std::optional<statement::reg_type> zero_reg;
                        for(const auto& src: p.params[0])
                            if(auto s = is_statement(src); s && is_eq(*s))
                            {
                                const auto& src_lore = info.find(s)->second;
                                std::size_t paramno = 0;
                                if(auto op = GetLiteral(src_lore.params[1]); op && op.value() == 0) { paramno = 2; }
                                else if(op = GetLiteral(src_lore.params[2]); op && op.value() == 0) { paramno = 1; }
                                else { zero_reg.reset(); break; } // Not an EQ-zero
                                if(!zero_reg) zero_reg = s->params[paramno];
                                else if(zero_reg.value() != s->params[paramno]) { zero_reg.reset(); break; } // Differing reg
                                if(p.presence[zero_reg.value()] != src_lore.params[paramno]) { zero_reg.reset(); break; } // Different sources
                            }
                            else
                                { zero_reg.reset(); break; } // Invalid source
                        if(zero_reg)
                        {
                            std::cerr << "EQ0->IFNZ branch shortened\n";
                            st->params[0] = zero_reg.value();
                            std::swap(st->cond, st->next);
                            elisions = true;
                        }
                        break;
                    }

                    case st_type::init:
                        // If at the point of an INIT instruction there is already another
                        // register that holds the same value, change the INIT into a COPY
                        for(std::size_t regno = 0; regno < p.presence.size(); ++regno)
                        {
                            if(elisions || p.presence[regno].empty()) continue;
                            bool ok = true;
                            // Find any reason not to use this register:
                            if(st->ident.empty())
                            {
                                if(auto op = GetLiteral(p.presence[regno], true);
                                   !op || op.value() != st->value) { ok = false; }
                            }
                            else
                                for(const auto& src: p.presence[regno])
                                    if(auto s = is_statement(src); !s || s == st || !is_init(*s) || st->ident != s->ident
                                                                                                 || st->value != s->value)
                                        { ok = false; }
                            if(ok)
                            {
                                // Change st into a COPY
                                std::cerr << "Load elision, INIT R" << st->lhs() << " changed into COPY from R" << regno << '\n';
                                st->Reinit(st_type::copy, st->lhs(), regno);
                                elisions = true;
                            }
                        }
                        break;

                    case st_type::fcall:
                        // If there is a FCALL immediately followed by a RET,
                        // where the function pointer comes from a single INIT instruction,
                        // convert the FCALL into a shuffle + jump. (Tail call optimization)
                        // The RET instruction will be deleted later by GC if necessary.
                        if(round == 1 && is_ret(*st->next) && st->lhs() == st->next->lhs())
                            // FCALL + RET detected, study the source of the function pointer.
                            // Require that the source register contains a reference to one distinct known function.
                            if(std::optional<std::string> name; std::all_of(p.params[1].begin(), p.params[1].end(), [&](const auto& src)
                            {
                                auto s = is_statement(src);
                                if(s && is_init(*s) && !s->value && (!name || name.value() == s->ident)) { name = s->ident; return true; }
                                return false;
                            }) && name)
                                if(auto entry = entry_points.find(name.value()); entry != entry_points.end())
                                {
                                    std::cerr << "- Performing tailcall optimization, call to " << name.value() << " will be removed\n";

                                    auto params = std::move(st->params);            // Move params from fcall
                                    params.erase(params.begin(), params.begin()+2); // Delete result-reg and ident-reg
                                    // Change the FCALL into a NOP, and append COPY insns to shuffle parameters.
                                    st->Reinit(st_type::nop);
                                    statement** target = &st->next;
                                    ProduceShuffle(std::move(params),
                                        [&](auto tgt, auto src) { target = &(*target = s_copy(tgt, src))->next; },
                                        [](auto reg)            { return reg; });
                                    // Link the last insn into the beginning of the target function
                                    *target = entry->second;
                                    elisions = true;
                                }
                        break;

                    default:
                        break;
                }
            }

            if(n_upd) { std::cerr << n_upd << " literal expressions simplified\n"; }
            if(n_upd || elisions) return true;
        }
        return false;
    }

    bool Optimize_CopyElision()
    {
        auto COPYreaders = [&](auto& readers, statement::reg_type writereg)
        {
            // Collect the list of COPY insns that read directly from this write-insn
            std::vector<statement*> copy_insns;
            for(const auto& reader: readers)
                if(auto read_insn = is_statement(reader); read_insn && is_copy(*read_insn)
                                                          && read_insn->rhs() == writereg && read_insn->lhs() != writereg
                                                          && (copy_insns.empty() || copy_insns.front()->lhs() == read_insn->lhs()))
                    { copy_insns.push_back(read_insn); }
                else
                    { copy_insns.clear(); break; }
            return copy_insns;
        };

        const auto info = GenerateAccessInfo(false); // Don't follow copies
        for(const auto& [write_insn,p]: info)
        {
            // Replace the target in statements that are followed by a copy,
            // with the target from the copy, if the *only* place where the
            // target is used is in the COPY insn. Skip the COPY insn.
            if(write_insn->ForAllWriteRegs([&](auto wregno, auto windex)
            {
                // Collect all COPY-type readers of this write-insn. All copies must have the same target.
                // If there are multiple different targets, or different types of consumers, the optimization cannot be done.
                if(auto copy_insns = COPYreaders(p.params[windex], wregno); !copy_insns.empty())
                {
                    std::map<statement*, std::size_t> valid_write_insns;
                    valid_write_insns.emplace(write_insn, windex);

                    auto new_regno = copy_insns.front()->lhs();

                    if(std::none_of(copy_insns.begin(), copy_insns.end(), [&](auto s)
                        {
                            // Make sure that the value consumed by the copy_insn can only
                            // originate from this write_insn, or some other write_insn
                            // where the value ends up being consumed only by this copy_insn.
                            const auto& sources = info.find(s)->second.params[1];
                            return std::any_of(sources.begin(), sources.end(), [&](const auto& src)
                            {
                                auto copy_source = is_statement(src);
                                if(!copy_source) return true;
                                if(valid_write_insns.find(copy_source) != valid_write_insns.end()) return false;
                                return copy_source->ForAllWriteRegs([&](auto wregno2, auto windex2)
                                {
                                    if(wregno2 == wregno)
                                    {
                                        if(COPYreaders(info.find(copy_source)->second.params[windex2], wregno2) != copy_insns)
                                            return true;
                                        valid_write_insns.emplace(copy_source, windex2);
                                    }
                                    return false;
                                });
                            });
                        })
                    && std::none_of(copy_insns.begin(), copy_insns.end(), [&](auto s)
                        {
                            // Make sure that there is nothing that may change the value
                            // of new_regno between the write_insn and s (the copy statement).
                            return std::any_of(valid_write_insns.begin(), valid_write_insns.end(),
                                [&,      &p = info.find(s      )->second.presence[new_regno]](auto w)
                                { return p != info.find(w.first)->second.presence[new_regno]; });
                        }))
                    {
                        // Change the write target
                        for(auto& [insn_ptr,param_index]: valid_write_insns) { insn_ptr->params[param_index] = new_regno; }

                        /* Invalidate, if, after performing the change to write_insn but WITHOUT changing the COPY into NOP,
                         * the write_insn would have any readers.
                         */
                        bool revert = false;
                        const auto info2 = GenerateAccessInfo(false); // Don't follow copies
                        for(auto& [insn_ptr,param_index]: valid_write_insns)
                            if(!info2.find(insn_ptr)->second.params[param_index].empty())
                                { revert = true; break; }

                        if(revert)
                        {
                            std::cerr << "Copy elision (type 1), R" << wregno << " to R" << new_regno
                                      << " hastily cancelled in " << valid_write_insns.size() << " insns\n";
                            for(auto& [insn_ptr,param_index]: valid_write_insns) { insn_ptr->params[param_index] = wregno; }
                        }
                        else
                        {
                            std::cerr << "Copy elision (type 1), R" << wregno << " changed to R" << new_regno
                                      << " in " << valid_write_insns.size() << " insns\n";
                            // And change the COPY insns into NOPs
                            for(auto s: copy_insns) { s->Reinit(st_type::nop); }
                            return true;
                        }
                    }
                }
                return false;
            })) { return true; }

            // Replace the source in stmts that get the value from a COPY stmt,
            // with the COPY's source, if the list of sources for that register
            // is the identical at the COPY and the target statements.
            if(is_copy(*write_insn) && write_insn->lhs() != write_insn->rhs())
            {
                auto tgt_reg = write_insn->lhs();
                auto src_reg = write_insn->rhs();

                bool changes = false;
                // One condition:
                //  All sources of tgt_reg at read_insn must be copy_insns
                //  that fulfill the following condition:
                //     src_reg must not be modified between write_insn and read_insn
                //     (list of sources must be identical)
                for(auto reader: p.params[0]) // Readers of tgt_reg
                    if(auto read_insn = is_statement(reader))
                        if(auto& w = info.find(read_insn)->second.presence[tgt_reg]; std::all_of(w.begin(), w.end(), [&](auto writer)
                            {
                                auto other_writer = is_statement(writer);
                                return other_writer
                                    && is_copy(*other_writer)
                                    && other_writer->params == write_insn->params
                                    && (src_reg >= statement::no_mask
                                     || info.find(other_writer)->second.presence[src_reg]
                                     == info.find(read_insn)->second.presence[src_reg]);
                            }))
                        {
                            read_insn->ForAllReadRegs([&](auto regno, auto index)
                            {
                                if(regno == tgt_reg)
                                {
                                    // Change the read source
                                    std::cerr << "Copy elision (type 2), R" << tgt_reg << " changed to R" << src_reg << '\n';
                                    read_insn->params[index] = src_reg;
                                    changes = true;
                                }
                            });
                        }
                if(changes) return true;
            }
            // If a copy is created only because the original value is changed,
            // reorder instructions.
            // The FIRST consume-insn will be moved BEFORE the FIRST modify-insn.
            //
            // e.g.
            //    COPY R4 R0      - copy-insn
            //    ADD R0 R0 #1    - modify-insn
            //    ADD R5 R4 R2    - consume-insn
            // change to:
            //    COPY R4 R0      - copy-insn (might get deleted)
            //    ADD R5 R4 R2    - consume-insn
            //    ADD R0 R0 #1    - modify-insn
            //
            // Instead of COPY, any write-insn will do.
            // Definitions:
            //    - CopyInsn    = this (write_insn)
            //    - ConsumeInsn = each consumer of whatever the write_insn produces
            //    - ModifyInsn  = the first insn that overwrites any of the read-reads of CopyInsn
            //                    (a write-insn that overwrites a read-reg of CopyInsn
            //                     where its Presence[that reg] == Presence[copyinsn])
            // Conditions:
            //    - ConsumeInsn.Presence == ModifyInsn.Presence
            //        For all read-regs of consume-insn
            //        For all write-regs of consume-insn, too
            //    - ModifyInsn does not have side-effects
            //
            if(write_insn->ForAllWriteRegs([&](auto wregno, auto windex)
            {
                std::set<statement::reg_type> read_regs_of_copyinsn;
                write_insn->ForAllReadRegs([&](auto regno, auto) { read_regs_of_copyinsn.insert(regno); });
                for(const auto& [write_insn2,p2]: info)
                    if(write_insn2 != write_insn && write_insn2->ForAllWriteRegs([&](auto wregno2, auto /*windex2*/)
                    {
                        std::set<statement::reg_type> read_regs_of_modinsn;
                        write_insn2->ForAllReadRegs([&](auto regno, auto) { read_regs_of_modinsn.insert(regno); });
                        if(p.presence[wregno2] == p2.presence[wregno2])
                        {
                            // write_insn is a CopyInsn.
                            // write_insn2 is a ModifyInsn.
                            for(const auto& consumer: p.params[windex])
                                if(auto reader = is_statement(consumer))
                                    if(reader != write_insn2 && !is_fcall(*reader) && !reader->HasSideEffects() && !is_ifnz(*reader) && !is_ifeq(*reader) && !is_ret(*reader)
                                    && Reach(write_insn2, reader, write_insn)
                                    && (read_regs_of_copyinsn.find(wregno2) != read_regs_of_copyinsn.end()
                                     ||  (write_insn2->ForAllReadRegs([&](auto regno,auto){return regno==wregno;})
                                           && !reader->ForAllReadRegs([&](auto regno,auto){return regno==wregno;})
                                       ) )
                                    && (!reader->ForAllWriteRegs([&](auto wregno3,auto)
                                           { return read_regs_of_modinsn.find(wregno3) != read_regs_of_modinsn.end(); })
                                       )
                                      )
                                    {
                                        // reader is ConsumeInsn.
                                        const auto& info2 = info.find(reader)->second;
                                        if(!reader->ForAllRegs([&](auto regno, auto)
                                        {
                                            return info2.presence[regno] != p2.presence[regno];
                                        }))
                                        {
                                            std::cerr << "Performing shuffle with Copy=<"; write_insn->Dump(std::cerr);
                                            std::cerr << ">, Modify=<";                    write_insn2->Dump(std::cerr);
                                            std::cerr << ">, Consume=<";                   reader->Dump(std::cerr);
                                            std::cerr << ">\n";

                                            statement* next = reader->next;
                                            // Change whatever pointed to ModifyInsn, to point to ConsumeInsn
                                            // Change whatever pointed to ConsumeInsn, to point to ConsumeInsn->next
                                            // Change ConsumeInsn to point to ModifyInsn
                                            auto RePoint = [&](statement*& p)
                                            {
                                                if(p == write_insn2) p = reader;
                                                else if(p == reader) p = next;
                                            };
                                            for(auto& t: entry_points)   { RePoint(t.second); }
                                            for(auto& s: all_statements) { RePoint(s->next); if(is_ifnz(*s) || is_ifeq(*s)) RePoint(s->cond); }
                                            reader->next = write_insn2;
                                            return true;
                                        }
                                    }
                        }
                        return false;
                    })) return true;
                return false;
            })) return true;
            // How about this situation?
            //
            // Before:
            //    add      R2 R1 R8     - copy-insn
            //    read     R3 R1        - does not read R2
            //    write    R0 R3        - does not read R2
            //    copy     R1 R2        - consume-insn
            // After:
            //    read     R3 R1
            //    write    R0 R3
            //    add      R2 R1 R8
            //    copy     R1 R2
            //
            // How about this situation:
            // Before:
            //    add      R3 R1 1
            //    read     R4 R1
            //    write    R0 R4
            //    copy     R1 R3
            // After:
            //    read     R4 R1
            //    write    R0 R4
            //    add      R3 R1 1
            //    copy     R1 R3
        }
        return false;
    }

    bool AvailableForGrabs(statement::reg_type r, statement* s,
                           const std::map<statement*,AccessInfo::info>& info,
                           std::set<statement*>& reach_list) const
    {
        const auto& lore = info.find(s)->second;
        if(lore.presence[r].empty())
        {
            //std::cerr << "R" << r << " is available for grabs; it's uninitialized\n";
            return true;
        }
        for(const auto& src: lore.presence[r])
        {
            /* check the list of statements that depend on that write.
               If any of those statements can be reached from this statement,
               skip and try the next register number. */
            if(auto s2 = is_statement(src))
            {
                const auto& their_lore = info.find(s2)->second;
                for(const auto& reader: their_lore.params[0])
                    if(is_statement(reader) != s && Reach(s, is_statement(reader), s2))
                    {
                        /*std::cerr << "- R" << r << " bad, writer ";
                        s2->Dump(std::cerr);
                        std::cerr << " reader ";
                        is_statement(reader)->Dump(std::cerr);
                        std::cerr << " can still be reached\n";*/
                        return false;
                    }
                /*std::cerr << "- R" << r << " ok, writer ";
                s2->Dump(std::cerr);
                std::cerr << " does not have still reachable readers\n";*/
            }
            else if(std::get_if<parameter_source>(&src))
            {
                // Check if any of the reachable statements from ours depends
                // on this parameter.
                if(reach_list.empty())
                {
                    // Collect a list of all statements that can be visited by following next/cond pointers.
                    std::set<statement*>& visited = reach_list;
                    std::list<statement*> pending;
                    // Add all entry points
                    pending.push_back(s);
                    // Track everything that can be reached from entry points
                    bool recursion_or_loop = false;
                    while(!pending.empty())
                    {
                        statement* chain = pending.front(); pending.pop_front();
                        if(!visited.insert(chain).second)
                        {
                            if(chain == s) recursion_or_loop = true;
                            continue;
                        }
                        if((is_ifnz(*chain) || is_ifeq(*chain)) && chain->cond) pending.push_back(chain->cond);
                        if(chain->next) pending.push_back(chain->next);
                    }
                    if(!recursion_or_loop) visited.erase(s);
                }
                for(auto s2: reach_list)
                {
                    const auto& their_lore = info.find(s2)->second;
                    if(s2->ForAllReadRegs([&](auto,auto index){
                        if(const auto& l = their_lore.params[index]; l.find(src) != l.end())
                        {
                            //std::cerr << "- R" << r << " bad, source is param that is still used\n";
                            return true;
                        }
                        return false;
                    })) return false;
                }
                //std::cerr << "- R" << r << " ok, param is not used by any of our reader\n";
            }
        }
        //std::cerr << "Result: Yes\n";
        return true;
    }

    bool Optimize_RegisterRename()
    {
        std::size_t n_renames = 0;
        /* 1. Sort writes by the number of statements where this write is in presence[] */
        const auto info = GenerateAccessInfo(false,false); // Don't follow copies/ifnz
        std::vector<std::pair<std::size_t/* num observers */, statement*>> write_statements;
        {std::map<statement*, std::set<statement*>> observers;
        for(const auto& [st, info]: info)
            for(const auto& pres: info.presence)
                for(const auto& source: pres)
                    if(auto src = is_statement(source))
                        observers[src].insert(st);

        for(const auto& s: all_statements)
            if(auto i = observers.find(s.get()); i != observers.end())
                write_statements.emplace_back(i->second.size(), s.get());}

        std::sort(write_statements.begin(), write_statements.end());
        /* 2. Pick the write with smallest number of observers */
        for(auto [num_observers,s]: write_statements)
        {
            //std::cerr << num_observers << " observers for "; s->Dump(std::cerr); std::cerr << '\n';

            std::set<statement*> reach_list;
            for(std::size_t p = s->NumWriteRegs(); p-- > 0; )
            {
                /* Create a list of candidate renamed register numbers */
                statement::reg_type original_regno = s->params[p];
                std::set<statement::reg_type> consider;
                for(statement::reg_type r=0; r<original_regno; ++r)
                    consider.insert(r);

                /* for every reader of this statement, check that
                 * we are the only source of that register
                 */
                std::optional<std::set<source_type>> sources;
                std::list<statement*> test_statements;
                std::set<statement*> writer_statements;
                test_statements.push_back(s);
                while(!test_statements.empty())
                {
                    statement* writer = test_statements.front(); test_statements.pop_front();
                    if(writer_statements.insert(writer).second)
                    {
                        const auto& lore = info.find(writer)->second;
                        for(auto reader: lore.params[p])
                        {
                            auto s2 = is_statement(reader); assert(s2);
                            const auto& their_lore = info.find(s2)->second;
                            const auto& their_sources = their_lore.presence[original_regno];
                            if(!sources)
                                sources = their_sources;
                            else if(sources.value() != their_sources)
                            {
                                //std::cerr << "- bad, readers disagree\n";
                                goto not_good;
                            }

                            /* For every reader of ours, check that _their_ sources of this reg match ours */
                            for(auto i = consider.begin(); i != consider.end(); )
                                if(their_lore.presence[*i] == lore.presence[*i])
                                    ++i;
                                else
                                    i = consider.erase(i);
                        }
                        for(auto s: sources.value())
                            if(auto ss = is_statement(s))
                                test_statements.push_back(ss);
                            else
                            {
                                std::cerr << "- bad, source for reader is not a statement\n";
                                goto not_good;
                            }
                    }
                }

                /* 3. For each write-param, go from R0.. up to the current register number; */
                for(auto r: consider)
                {
                    /* for each item in presence[that register number], */
                    if(std::all_of(writer_statements.begin(), writer_statements.end(),
                                   [&](statement*s) { return AvailableForGrabs(r, s, info, reach_list); }))
                    {
                        /* OK! Change our write-reg */
                        std::cerr << "Register renumbered R" << original_regno << " <- R" << r << " in " << writer_statements.size() << " statements\n";
                        for(auto s: writer_statements)
                        {
                            const auto& lore = info.find(s)->second;
                            s->params[p] = r;
                            /* And change the read-reg for every reader of ours */
                            for(auto reader: lore.params[p])
                            {
                                auto s2 = is_statement(reader);
                                assert(s2);
                                s2->ForAllReadRegs([&](auto& regno, auto/*index*/)
                                {
                                    if(regno == original_regno) regno = r;
                                });
                            }
                            ++n_renames;
                        }
                        return n_renames;
                    }
                }
            not_good:;
            }
        }
        return n_renames;
    }

    void Optimize()
    {
        while(Optimize_DeleteNOPs()
           || Optimize_GC()
           || Optimize_JumpThreading()
           || Optimize_CopyElision()
           || Optimize_Simplify()
           || Optimize_ConvertLiterals()
           || Optimize_RegisterRename()
           || Optimize_MergeTrees())
        {
        }
    }

    struct compilation_context
    {
        statement::reg_type counter; // Counter for next unused register number
        statement**         tgt;     // Pointer to where the next instruction will be stored
        std::map<std::size_t, statement::reg_type> map; // AST variables to register numbers mapping
    };
    statement::reg_type Compile(const expression& code, compilation_context& ctx)
    {
        statement::reg_type result = statement::nowhere;

        // make(): Create a new register (variable) for the IR
        auto make = [&]()             { return ctx.counter++; };
        // put():  Place the given chain of code at *tgt, then re-point tgt into the end of the chain
        auto put  = [&](statement* s) { for(*ctx.tgt = s; s; s = *ctx.tgt) ctx.tgt = &s->next; };

        switch(code.type)
        {
            case ex_type::string:
            {
                // Create an INIT statement (+ possible integer offset) that refers to the string table
                put(s_init(result = make(), "STRINGTABLE", (long)string_constants.find(code.strvalue + '\0')));
                break;
            }
            case ex_type::ident:
            {
                switch(auto& id = code.ident; id.type)
                {
                    case id_type::function:  put(s_init(result = make(), id.name, 0l)); break;
                    case id_type::variable:  result = ctx.map.emplace(id.index, make()).first->second; break;
                    case id_type::parameter: result = id.index; break;
                    case id_type::undefined: std::cerr << "UNDEFINED IDENTIFIER, DON'T KNOW WHAT TO DO\n"; break;
                }
                break;
            }

            case ex_type::deref:  put(s_read(result = make(), Compile(code.params.front(), ctx))); break;
            case ex_type::neg:    put(s_neg(result  = make(), Compile(code.params.front(), ctx))); break;
            case ex_type::ret:    put(s_ret(result  =         Compile(code.params.front(), ctx))); break;
            case ex_type::number: put(s_init(result = make(), "", code.numvalue)); break;
            case ex_type::nop:    put(s_init(result = make(), "", 0L)); break; // dummy expr
            case ex_type::addrof: std::cerr << "NO IDEA WHAT TO DO WITH " << stringify(code) << '\n'; break; // Unhandleable

            case ex_type::add:
            case ex_type::eq:
            case ex_type::comma:
            {
                // Trivially reduce parameters from left to right
                for(auto i = code.params.begin(); i != code.params.end(); ++i)
                    if(statement::reg_type prev = result, last = result = Compile(*i, ctx); prev != statement::nowhere)
                      { if(is_add(code))     { put(s_add(result = make(), prev, last)); }
                        else if(is_eq(code)) { put(s_eq(result  = make(), prev, last)); }
                        else /*comma, no reducer: discard everything but the last stmt*/ {result=last;} }
                break;
            }
            case ex_type::copy:
            {
                // Be careful to compile the source expression first, and then the target expression.
                // If the target expression is a pointer dereference, create a WRITE statement rather than COPY.
                if(const auto& src = code.params.front(), &dest = code.params.back(); is_deref(dest))
                    { result = Compile(src, ctx); put(s_write(Compile(dest.params.front(), ctx), result)); }
                else
                    { auto temp = Compile(src, ctx); put(s_copy(result = Compile(dest, ctx), temp)); }
                break;
            }
            case ex_type::fcall:
            {
                // Compile each parameter expression, and create a subroutine call statement with those params.
                put(s_fcall(result = make(), make_transform_iterator(code.params.begin(), code.params.end(),
                                                                     [&](const expression&p){ return Compile(p,ctx); }),
                                             transform_iterator<statement::reg_type>{}));
                break;
            }
            case ex_type::loop:
            case ex_type::cand:
            case ex_type::cor:
            {
                // Conditional code (including the while-loop!)
                const bool is_and = !is_cor(code); // while(), if() and &&
                result            = make();
                // Three mandatory statements will be created:
                statement* b_then = s_init(result, "", is_and ? 1l : 0l);        // Then-branch
                statement* b_else = s_init(result, "", is_and ? 0l : 1l);        // Else-branch
                statement* end    = s_nop(); b_then->next = b_else->next = end; // A common target for both.
                // Save a pointer to the first expression (needed for loops).
                // Take a reference to the pointer and not copy of the pointer,
                // because the pointer will change in this loop.
                statement*& begin = *ctx.tgt;
                for(auto i = code.params.begin(); i != code.params.end(); ++i)
                {
                    // Compile the expression.
                    statement::reg_type var = Compile(*i, ctx);
                    // Don't create a branch after contingent statements in a loop.
                    if(is_loop(code) && i != code.params.begin()) { continue; }
                    // Immediately after the expression, create a branch on its result.
                    statement* condition = *ctx.tgt = s_ifnz(var, nullptr);
                    // With &&, the code continues in the true-branch. With ||, in false-branch.
                    // The other branch is tied into b_else.
                    if(is_and) { ctx.tgt = &condition->cond; condition->next = b_else; }
                    else       { ctx.tgt = &condition->next; condition->cond = b_else; }
                }
                // The end of the statement chain is linked into b_then.
                // For loops, the chain is linked back into the start of the loop instead.
                *ctx.tgt = is_loop(code) ? begin : b_then;
                ctx.tgt = &end->next;  // Code continues after the end statement.
                break;
            }
        }
        return result;
    }

    void CompileFunction(function& f)
    {
        function_parameters[f.name] = f.num_params;

        compilation_context ctx{ f.num_params, &entry_points[f.name], {} };
        Compile(f.code, ctx);
    }

    void Compile()
    {
        BuildStrings();
        for(auto& f: func_list) CompileFunction(f);
    }
};

int main(int /*argc*/, char** argv)
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
    func_list = std::move(ctx.func_list);

    std::cerr << "Initial\n";
    for(const auto& f: func_list) std::cerr << stringify_tree(f);

    DoConstantFolding();

    std::cerr << "Final\n";
    for(const auto& f: func_list) std::cerr << stringify_tree(f);

    compilation code;
    code.Compile();

    std::cerr << "COMPILED CODE\n";
    code.Dump(std::cerr);

    code.Optimize();

    std::cerr << "OPTIMIZED CODE\n";
    code.Dump(std::cerr);
}



