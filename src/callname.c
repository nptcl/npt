#include <string.h>
#include "callname.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "symbol.h"

/*
 *  access
 */
_g addr refcallname(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	return RefCallName_Low(pos);
}
_g void getcallname(addr pos, addr *value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	GetCallName_Low(pos, value);
}
_g void setcallname(addr pos, addr value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCallName_Low(pos, value);
}

_g CallNameType refcallnametype(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	return RefCallNameType_Low(pos);
}
_g void getcallnametype(addr pos, CallNameType *value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	GetCallNameType_Low(pos, value);
}
_g void setcallnametype(addr pos, CallNameType value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCallNameType_Low(pos, value);
}


/*
 *  allocate
 */
_g void make_callname_alloc(LocalRoot local, addr *ret)
{
	alloc_array2(local, ret, LISPTYPE_CALLNAME, 1);
}

_g void callname_alloc(LocalRoot local, addr *ret, addr name, CallNameType type)
{
	addr pos;

	Check(! symbolp(name), "name error.");
	make_callname_alloc(local, &pos);
	SetCallName_Low(pos, name);
	SetCallNameType_Low(pos, type);
	*ret = pos;
}
_g void callname_local(LocalRoot local, addr *ret, addr name, CallNameType type)
{
	Check(local == NULL, "local error");
	callname_alloc(local, ret, name, type);
}
_g void callname_heap(addr *ret, addr name, CallNameType type)
{
	callname_alloc(NULL, ret, name, type);
}

_g void setf_callname_alloc(LocalRoot local, addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	callname_alloc(local, ret, symbol, CALLNAME_SETF);
}
_g void setf_callname_local(LocalRoot local, addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	callname_local(local, ret, symbol, CALLNAME_SETF);
}
_g void setf_callname_heap(addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error");
	callname_heap(ret, symbol, CALLNAME_SETF);
}


/*
 *  copy
 */
_g void copy_callname_alloc(LocalRoot local, addr *ret, addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	callname_alloc(local, ret, RefCallName_Low(pos), RefCallNameType_Low(pos));
}
_g void copy_callname_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	copy_callname_alloc(local, ret, pos);
}
_g void copy_callname_heap(addr *ret, addr pos)
{
	copy_callname_alloc(NULL, ret, pos);
}


/*
 *  parse
 */
_g CallNameType parse_callname(addr name, addr *ret)
{
	CallNameType type;
	addr setf, cons;

	Check(name == Unbound, "unbound error.");
	switch (GetType(name)) {
		case LISPTYPE_NIL:
		case LISPTYPE_T:
		case LISPTYPE_SYMBOL:
			type = CALLNAME_SYMBOL;
			break;

		case LISPTYPE_CONS: /* (setf name) */
			GetConst(COMMON_SETF, &setf);
			GetCons(name, &name, &cons);
			if (name != setf)
				goto error;
			Check(cons == Unbound, "unbound cons error.");
			if (GetType(cons) != LISPTYPE_CONS)
				goto error;
			GetCons(cons, &name, &cons);
			if (! symbolp(name))
				goto error;
			if (cons != Nil)
				goto error;
			type = CALLNAME_SETF;
			break;

		default:
			goto error;
	}
	*ret = name;
	return type;

error:
	return CALLNAME_ERROR;
}

_g int parse_callname_alloc(LocalRoot local, addr *ret, addr name)
{
	CallNameType type;

	type = parse_callname(name, &name);
	if (type == CALLNAME_ERROR)
		return 1;
	callname_alloc(local, ret, name, type);

	return 0;
}
_g int parse_callname_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	return parse_callname_alloc(local, ret, name);
}
_g int parse_callname_heap(addr *ret, addr name)
{
	return parse_callname_alloc(NULL, ret, name);
}
_g void parse_callname_abort(LocalRoot local, addr *ret, addr name)
{
	if (parse_callname_alloc(local, ret, name))
		Abort("Invalid function name.");
}
_g void parse_callname_error(addr *ret, addr name)
{
	if (parse_callname_heap(ret, name))
		fmte("Invalid function name ~S.", name, NULL);
}
_g int parse_callname_error_(addr *ret, addr name)
{
	if (parse_callname_heap(ret, name))
		return fmte_("Invalid function name ~S.", name, NULL);
	return 0;
}


/*
 *  boolean
 */
_g int callnamep(addr pos)
{
	return GetType(pos) == LISPTYPE_CALLNAME;
}

_g int symbolp_callname(addr call)
{
	CheckType(call, LISPTYPE_CALLNAME);
	return RefCallNameType(call) == CALLNAME_SYMBOL;
}

_g int setfp_callname(addr call)
{
	CheckType(call, LISPTYPE_CALLNAME);
	return RefCallNameType(call) == CALLNAME_SETF;
}

_g int constantp_callname(addr pos)
{
	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallName(pos, &pos);
	return GetStatusReadOnly(pos);
}

_g int function_name_p(addr name)
{
	if (GetType(name) == LISPTYPE_CALLNAME)
		return 1;
	return parse_callname(name, &name) != CALLNAME_ERROR;
}

_g int equal_callname(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_CALLNAME, "type left error");
	Check(GetType(right) != LISPTYPE_CALLNAME, "type right error");
	return (RefCallNameType_Low(left) == RefCallNameType_Low(right))
		&& (RefCallName_Low(left) == RefCallName_Low(right));
}


/*
 *  function
 */
_g CallNameType getglobal_callname(addr pos, addr *ret)
{
	CallNameType type;

	GetCallNameType(pos, &type);
	GetCallName(pos, &pos);
	switch (type) {
		case CALLNAME_SYMBOL:
			GetFunctionSymbol(pos, ret);
			break;

		case CALLNAME_SETF:
			getsetf_symbol(pos, ret);
			break;

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}

	return type;
}

_g CallNameType getglobalcheck_callname(addr pos, addr *ret)
{
	CallNameType value;

	value = getglobal_callname(pos, ret);
	if (*ret == Unbound) {
		name_callname_heap(pos, &pos);
		undefined_function(pos);
	}

	return value;
}

_g void setglobal_callname(addr pos, addr value)
{
	CallNameType type;

	GetCallNameType(pos, &type);
	GetCallName(pos, &pos);
	switch (type) {
		case CALLNAME_SYMBOL:
			SetFunctionSymbol(pos, value);
			break;

		case CALLNAME_SETF:
			setsetf_symbol(pos, value);
			break;

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}
}

_g void remtype_global_callname(addr pos)
{
	CallNameType type;

	GetCallNameType(pos, &type);
	GetCallName(pos, &pos);
	switch (type) {
		case CALLNAME_SYMBOL:
			remtype_function_symbol(pos);
			break;

		case CALLNAME_SETF:
			remtype_setf_symbol(pos);
			break;

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}
}

static CallNameType callnametype(addr pos, addr *value)
{
	CallNameType type;

	if (GetType(pos) == LISPTYPE_CALLNAME) {
		GetCallName_Low(pos, value);
		GetCallNameType_Low(pos, &type);
		return type;
	}
	else {
		return parse_callname(pos, value);
	}
}

_g void getglobal_parse_callname(addr pos, addr *value)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			GetFunctionSymbol_Low(pos, value);
			break;

		case CALLNAME_SETF:
			getsetf_symbol(pos, value);
			break;

		case CALLNAME_ERROR:
		default:
			*value = NULL;
			fmte("Invalid function name.", NULL);
			break;
	}
}
_g void setglobal_parse_callname(addr pos, addr value)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			SetFunctionSymbol_Low(pos, value);
			break;

		case CALLNAME_SETF:
			setsetf_symbol(pos, value);
			break;

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
	}
}
_g void getglobalcheck_parse_callname(addr pos, addr *ret)
{
	getglobal_parse_callname(pos, ret);
	if (*ret == Unbound) {
		name_callname_heap(pos, &pos);
		undefined_function(pos);
	}
}


/*
 *  name
 */
_g void name_callname_alloc(LocalRoot local, addr pos, addr *ret)
{
	CallNameType type;
	addr setf;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallNameType_Low(pos, &type);
	switch (type) {
		case CALLNAME_SYMBOL:
			GetCallName_Low(pos, ret);
			break;

		case CALLNAME_SETF:
			GetConst(COMMON_SETF, &setf);
			GetCallName_Low(pos, &pos);
			list_alloc(local, ret, setf, pos, NULL);
			break;

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
	}
}

_g void name_callname_local(LocalRoot local, addr pos, addr *ret)
{
	Check(local == NULL, "local error");
	name_callname_alloc(local, pos, ret);
}

_g void name_callname_heap(addr pos, addr *ret)
{
	name_callname_alloc(NULL, pos, ret);
}

