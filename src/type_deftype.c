#include "condition.h"
#include "control.h"
#include "type_deftype.h"
#include "symbol.h"

_g void getdeftype(addr symbol, addr *ret)
{
	CheckSymbol(symbol);
	getdeftype_symbol(symbol, ret);
}

_g void setdeftype(addr symbol, addr pos)
{
	CheckSymbol(symbol);
	setdeftype_symbol(symbol, pos);
}

_g int symbol_deftypep(addr symbol)
{
	CheckSymbol(symbol);
	getdeftype(symbol, &symbol);
	return symbol != Nil;
}

_g int execute_list_deftype(Execute ptr, addr *ret, addr list, addr env)
{
	addr call;

	CheckType(list, LISPTYPE_CONS);
	GetCar(list, &call);
	CheckSymbol(call);
	getdeftype(call, &call);
	if (call == Nil) {
		*ret = NULL;
		return 0;
	}

	return callclang_funcall(ptr, ret, call, list, env, NULL);
}

_g int execute_symbol_deftype(Execute ptr, addr *ret, addr symbol, addr env)
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

