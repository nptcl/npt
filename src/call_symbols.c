#include "build.h"
#include "call_symbols.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "integer.h"
#include "package_common.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  make-symbol
 */
void make_symbol_common(addr var, addr *ret)
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

int copy_symbol_common_(addr var, addr opt, addr *ret)
{
	addr symbol, pos;

	if (opt == Unbound)
		opt = Nil;
	GetNameSymbol(var, &pos);
	Return(string_heap_(&pos, pos));
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

	return Result(ret, symbol);
}


/*
 *  gensym
 */
int gensym_common_(Execute ptr, addr opt, addr *ret)
{
	if (opt == Unbound)
		return make_gensym_(ptr, ret);
	else if (stringp(opt))
		return make_gensym_prefix_(ptr, opt, ret);
	else if (integerp(opt))
		return make_gensym_integer_(ptr, opt, ret);
	else
		return fmte_("type-error.", NULL);
}


/*
 *  gentemp
 */
int gentemp_common_(Execute ptr, addr opt1, addr opt2, addr *ret)
{
	if (opt1 == Unbound)
		opt1 = NULL;
	if (opt2 == Unbound)
		opt2 = NULL;
	return make_gentemp_(ptr, opt1, opt2, ret);
}


/*
 *  (setf symbol-function)
 */
int setf_symbol_function_common_(addr value, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	Return(remtype_function_symbol_(symbol));
	SetFunctionSymbol(symbol, value);

	return 0;
}


/*
 *  (setf symbol-value)
 */
int setf_symbol_value_common_(Execute ptr, addr value, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, value);

	return 0;
}


/*
 *  (setf symbol-plist)
 */
int setf_symbol_plist_common_(addr value, addr symbol)
{
	SetPlistSymbol(symbol, value);
	return 0;
}


/*
 *  get
 */
int get_common_(addr var1, addr var2, addr opt, addr *ret)
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
int setf_get_common_(addr value, addr symbol, addr key)
{
	addr list;

	GetPlistSymbol(symbol, &list);
	if (setplist_heap_safe(list, key, value, &list)) {
		SetPlistSymbol(symbol, list);
	}

	return 0;
}


/*
 *  remprop
 */
int remprop_common_(addr symbol, addr key, addr *ret)
{
	enum RemPlist value;
	addr list;

	GetPlistSymbol(symbol, &list);
	Return(remplist_safe_(list, key, &list, &value));
	switch (value) {
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
int makunbound_common_(Execute ptr, addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, Unbound);
	return 0;
}


/*
 *  set
 */
int set_common_(Execute ptr, addr symbol, addr value)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("The symbol ~S is readonly.", symbol, NULL);
	setspecial_local(ptr, symbol, value);
	return 0;
}

