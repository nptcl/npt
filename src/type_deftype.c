#include "common_header.h"
#include "condition.h"
#include "control_execute.h"
#include "cons.h"
#include "declare.h"
#include "lambda.h"
#include "type_deftype.h"
#include "symbol.h"

void getdeftype(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getdeftype_symbol(symbol, ret);
}

int setdeftype_(addr symbol, addr pos)
{
	CheckSymbol(symbol);
	return setdeftype_symbol_(symbol, pos);
}

int symbol_deftypep(addr symbol)
{
	CheckSymbol(symbol);
	getdeftype(symbol, &symbol);
	return symbol != Nil;
}

int execute_list_deftype(Execute ptr, addr *ret, addr list, addr env)
{
	addr call, symbol;

	CheckType(list, LISPTYPE_CONS);
	GetCar(list, &symbol);
	CheckSymbol(symbol);
	getdeftype(symbol, &call);
	if (call == Nil)
		return Result(ret, NULL);

	return callclang_funcall(ptr, ret, call, list, env, NULL);
}

int execute_symbol_deftype(Execute ptr, addr *ret, addr symbol, addr env)
{
	addr call;

	CheckSymbol(symbol);
	getdeftype(symbol, &call);
	if (call == Nil) {
		*ret = NULL;
		return 0;
	}
	/* name -> (call `(name ,@args) env) */
	cons_heap(&symbol, symbol, Nil);
	return callclang_funcall(ptr, ret, call, symbol, env, NULL);
}


/*
 *  deftype
 */
int deftype_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr right, eval, name, args, decl, doc;

	/* (deftype . form) */
	Return_getcdr(form, &right);
	if (right == Nil)
		return fmte_("deftype form must have at least a name and body.", NULL);
	if (! consp(right))
		return fmte_("Invalid deftype form.", NULL);

	/* name */
	Return_getcons(right, &name, &right);
	if (! symbolp(name))
		return fmte_("deftype name ~S must be a symbol.", name, NULL);
	if (right == Nil)
		return fmte_("deftype form must have at least a name and body.", NULL);
	if (! consp(right))
		return fmte_("Invalid deftype form.", NULL);

	/* args */
	Return_getcons(right, &args, &right);
	if (! IsList(right))
		return fmte_("Invalid deftype form.", NULL);

	/* parse */
	Return(lambda_deftype_(ptr->local, &args, args, Nil));
	Return(declare_body_documentation_(ptr, env, right, &doc, &decl, &right));

	/* (eval::deftype name args decl doc body) */
	GetConst(SYSTEM_DEFTYPE, &eval);
	list_heap(&eval, eval, name, args, decl, doc, right, NULL);
	eval_when_compile(eval, ret);

	return 0;
}

