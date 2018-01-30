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
        std::cerr << "Identifying " << f.name << '\n';

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
                if(!u.pure_known && e.ident.index != (&f - &func_list[0])) // Recursion is ignored
                {
                    std::cerr << "Function " << f.name << " calls unknown function " << u.name << '\n';
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
            std::cerr << "Function " << f.name << (f.pure ? " is pure\n" : " may have side-effects\n");
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
                if(assign.params.front().is_pure())
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
                std::cerr << std::distance(r, e.params.end()) << " dead expressions deleted\n";
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
        std::cerr << "Before: " << text_before << '\n';
        std::cerr << stringify_tree(f);

        for_all_expr(f.code, true, [&](expression& e){ ConstantFolding(e,f); });
        return stringify(f) != text_before;
    }));
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
    func_list = std::move(ctx.func_list);

    std::cerr << "Initial\n";
    for(const auto& f: func_list) std::cerr << stringify_tree(f);

    DoConstantFolding();

    std::cerr << "Final\n";
    for(const auto& f: func_list) std::cerr << stringify_tree(f);
}




