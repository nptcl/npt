#include "build.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "integer.h"
#include "package.h"
#include "strtype.h"
#include "symbol.h"
#include "symbol_common.h"

/*
 *  make-symbol
 */
_g void make_symbol_common(addr var, addr *ret)
{
	addr pos;

	symbol_heap(&pos);
	SetNameSymbol(pos, var);
	*ret = pos;
}


/*
 *  copy-symbol
 */
static void copy_symbol_type_common(addr var, addr symbol)
{
	addr type;

	/* value */
	gettype_value_symbol(var, &type);
	if (type != Nil)
		settype_value_symbol(symbol, type);

	/* function */
	gettype_function_symbol(var, &type);
	if (type != Nil)
		settype_function_symbol(symbol, type);

	/* setf */
	gettype_setf_symbol(var, &type);
	if (type != Nil)
		settype_setf_symbol(symbol, type);
}

_g void copy_symbol_common(addr var, addr opt, addr *ret)
{
	addr symbol, pos;

	if (opt == Unbound)
		opt = Nil;
	GetNameSymbol(var, &pos);
	string_heap(&pos, pos);
	symbol_heap(&symbol);
	SetNameSymbol(symbol, pos);

	if (opt != Nil) {
		/* symbol-value */
		GetValueSymbol(var, &pos);
		SetValueSymbol(symbol, pos);
		/* symbol-function */
		GetFunctionSymbol(var, &pos);
		SetFunctionSymbol(symbol, pos);
		/* symbol-setf */
		getsetf_symbol(var, &pos);
		if (pos != Unbound)
			setsetf_symbol(symbol, pos);
		/* property-list */
		GetPlistSymbol(var, &pos);
		copy_list_heap_unsafe(&pos, pos);
		SetPlistSymbol(symbol, pos);
		/* copy-type */
		copy_symbol_type_common(var, symbol);
	}
	*ret = symbol;
}


/*
 *  gensym
 */
_g int gensym_common(Execute ptr, addr opt, addr *ret)
{
	if (opt == Unbound)
		make_gensym(ptr, ret);
	else if (stringp(opt))
		make_gensym_prefix(ptr, opt, ret);
	else if (integerp(opt))
		make_gensym_integer(ptr, opt, ret);
	else
		return fmte("type-error.", NULL);

	return 0;
}


/*
 *  gentemp
 */
_g void gentemp_common(Execute ptr, addr opt1, addr opt2, addr *ret)
{
	if (opt1 == Unbound)
		opt1 = NULL;
	if (opt2 == Unbound)
		opt2 = NULL;
	make_gentemp(ptr, opt1, opt2, ret);
}


/*
 *  (setf symbol-function)
 */
_g int setf_symbol_function_common(addr value, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte("The symbol ~S is readonly.", symbol, NULL);
	remtype_function_symbol(symbol);
	SetFunctionSymbol(symbol, value);

	return 0;
}


/*
 *  (setf symbol-value)
 */
_g int setf_symbol_value_common(Execute ptr, addr value, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, value);

	return 0;
}


/*
 *  (setf symbol-plist)
 */
_g int setf_symbol_plist_common(addr value, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte("The symbol ~S is readonly.", symbol, NULL);
	SetPlistSymbol(symbol, value);
	return 0;
}


/*
 *  get
 */
_g int get_common(addr var1, addr var2, addr opt, addr *ret)
{
	int check;

	if (opt == Unbound)
		opt = Nil;
	GetPlistSymbol(var1, &var1);
	check = getplist_safe(var1, var2, &var1);

	return Result(ret, check == 0? var1: opt);
}


/*
 *  (setf get)
 */
_g int setf_get_common(addr value, addr symbol, addr key)
{
	addr list;

	if (GetStatusReadOnly(symbol))
		return fmte("The symbol ~S is readonly.", symbol, NULL);
	GetPlistSymbol(symbol, &list);
	if (setplist_heap_safe(list, key, value, &list))
		SetPlistSymbol(symbol, list);

	return 0;
}


/*
 *  remprop
 */
_g int remprop_common(addr symbol, addr key, addr *ret)
{
	addr list;

	if (GetStatusReadOnly(symbol))
		return fmte("The symbol ~S is readonly.", symbol, NULL);
	GetPlistSymbol(symbol, &list);
	switch (remplist_check_safe(list, key, &list)) {
		case RemPlist_Delete:
			return Result(ret, T);

		case RemPlist_Update:
			SetPlistSymbol(symbol, list);
			return Result(ret, T);

		case RemPlist_NotFound:
		default:
			return Result(ret, Nil);

	}
}


/*
 *  makunbound
 */
_g int makunbound_common(Execute ptr, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, Unbound);
	return 0;
}


/*
 *  set
 */
_g int set_common(Execute ptr, addr symbol, addr value)
{
	if (GetStatusReadOnly(symbol))
		return fmte("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, value);
	return 0;
}

