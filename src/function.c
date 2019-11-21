#include <string.h>
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "function.h"
#include "heap.h"
#include "local.h"
#include "memory.h"
#include "object.h"
#include "strtype.h"
#include "symbol.h"

/*
 *  callname
 */
_g void make_callname_alloc(LocalRoot local, addr *ret)
{
	alloc_array2(local, ret, LISPTYPE_CALLNAME, 1);
}

_g addr callname_allocr(LocalRoot local, addr name, enum CALLNAME_TYPE type)
{
	addr pos;

	Check(! symbolp(name), "name error.");
	make_callname_alloc(local, &pos);
	SetCallName_Low(pos, name);
	SetCallNameType_Low(pos, type);

	return pos;
}
_g addr callname_localr(LocalRoot local, addr name, enum CALLNAME_TYPE type)
{
	Check(local == NULL, "local error");
	return callname_allocr(local, name, type);
}
_g addr callname_heapr(addr name, enum CALLNAME_TYPE type)
{
	return callname_allocr(NULL, name, type);
}

_g void callname_alloc(LocalRoot local, addr *ret, addr name, enum CALLNAME_TYPE type)
{
	*ret = callname_allocr(local, name, type);
}
_g void callname_local(LocalRoot local, addr *ret, addr name, enum CALLNAME_TYPE type)
{
	Check(local == NULL, "local error");
	*ret = callname_allocr(local, name, type);
}
_g void callname_heap(addr *ret, addr name, enum CALLNAME_TYPE type)
{
	*ret = callname_allocr(NULL, name, type);
}

_g enum CALLNAME_TYPE parse_callname(addr name, addr *ret)
{
	enum CALLNAME_TYPE type;
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
			if (name != setf) goto error;
			Check(cons == Unbound, "unbound cons error.");
			if (GetType(cons) != LISPTYPE_CONS) goto error;
			GetCons(cons, &name, &cons);
			if (! symbolp(name)) goto error;
			if (cons != Nil) goto error;
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

_g int function_name_p(addr name)
{
	if (GetType(name) == LISPTYPE_CALLNAME) return 1;
	return parse_callname(name, &name) != CALLNAME_ERROR;
}

_g int callnamep(addr pos)
{
	return GetType(pos) == LISPTYPE_CALLNAME;
}

_g int symbol_callname_p(addr call)
{
	CheckType(call, LISPTYPE_CALLNAME);
	return RefCallNameType(call) == CALLNAME_SYMBOL;
}

_g int setf_callname_p(addr call)
{
	CheckType(call, LISPTYPE_CALLNAME);
	return RefCallNameType(call) == CALLNAME_SETF;
}

_g int parse_callname_alloc(LocalRoot local, addr *ret, addr name)
{
	enum CALLNAME_TYPE type;

	type = parse_callname(name, &name);
	if (type == CALLNAME_ERROR) return 1;
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
_g void parse_callname_error(addr *ret, addr name)
{
	if (parse_callname_heap(ret, name))
		fmte("Invalid function name ~S.", name, NULL);
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

_g int parse_setcallname(addr pos, addr name)
{
	enum CALLNAME_TYPE type;

	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	type = parse_callname(name, &name);
	if (type == CALLNAME_ERROR) return 1;
	SetCallName_Low(pos, name);
	SetCallNameType_Low(pos, type);

	return 0;
}

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

_g enum CALLNAME_TYPE refcallnametype(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	return RefCallNameType_Low(pos);
}
_g void getcallnametype(addr pos, enum CALLNAME_TYPE *value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	GetCallNameType_Low(pos, value);
}
_g void setcallnametype(addr pos, enum CALLNAME_TYPE value)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	Check(GetStatusReadOnly(pos), "readonly error");
	SetCallNameType_Low(pos, value);
}

_g int callname_constant_p(addr pos)
{
	Check(GetType(pos) != LISPTYPE_CALLNAME, "type error");
	GetCallName(pos, &pos);
	return GetStatusReadOnly(pos);
}

_g int equal_callname(addr left, addr right)
{
	Check(GetType(left) != LISPTYPE_CALLNAME, "type left error");
	Check(GetType(right) != LISPTYPE_CALLNAME, "type right error");
	return (RefCallNameType_Low(left) == RefCallNameType_Low(right))
		&& (RefCallName_Low(left) == RefCallName_Low(right));
}

_g enum CALLNAME_TYPE getfunction_callname_global(addr pos, addr *ret)
{
	enum CALLNAME_TYPE type;

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

_g enum CALLNAME_TYPE getfunction_callname_local(Execute ptr, addr pos, addr *ret)
{
	enum CALLNAME_TYPE type;

	GetCallNameType(pos, &type);
	GetCallName(pos, &pos);
	switch (type) {
		case CALLNAME_SYMBOL:
			getfunction_local(ptr, pos, ret);
			break;

		case CALLNAME_SETF:
			getsetf_local(ptr, pos, ret);
			break;

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}

	return type;
}

_g enum CALLNAME_TYPE getfunctioncheck_callname_local(Execute ptr, addr pos, addr *ret)
{
	enum CALLNAME_TYPE type;

	type = getfunction_callname_local(ptr, pos, ret);
	if (*ret == Unbound) {
		name_callname_heap(pos, &pos);
		undefined_function(pos);
	}

	return type;
}

_g void setfunction_callname_global(addr pos, addr value)
{
	enum CALLNAME_TYPE type;

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

static enum CALLNAME_TYPE callnametype(addr pos, addr *value)
{
	enum CALLNAME_TYPE type;

	if (GetType(pos) == LISPTYPE_CALLNAME) {
		GetCallName_Low(pos, value);
		GetCallNameType_Low(pos, &type);
		return type;
	}
	else {
		return parse_callname(pos, value);
	}
}

_g addr refcallname_local(Execute ptr, addr pos)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			return reffunction_local(ptr, pos);

		case CALLNAME_SETF:
			return refsetf_local(ptr, pos);

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}

	return NULL;
}
_g void getcallname_local(Execute ptr, addr pos, addr *value)
{
	*value = refcallname_local(ptr, pos);
}
_g void setcallname_local(Execute ptr, addr pos, addr value)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			setfunction_local(ptr, pos, value);
			break;

		case CALLNAME_SETF:
			setsetf_local(ptr, pos, value);
			break;

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}
}
_g addr refcallnamecheck_local(Execute ptr, addr pos)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			return reffunctioncheck_local(ptr, pos);

		case CALLNAME_SETF:
			return refsetfcheck_local(ptr, pos);

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}

	return NULL;
}
_g void getcallnamecheck_local(Execute ptr, addr pos, addr *value)
{
	*value = refcallnamecheck_local(ptr, pos);
}

_g addr refcallname_global(addr pos)
{
	Check(pos == Unbound, "unbound error");
	switch (callnametype(pos, &pos)) {
		case CALLNAME_SYMBOL:
			return RefFunctionSymbol_Low(pos);

		case CALLNAME_SETF:
			return refsetf_symbol(pos);

		case CALLNAME_ERROR:
		default:
			fmte("Invalid function name.", NULL);
			break;
	}

	return NULL;
}
_g void getcallname_global(addr pos, addr *value)
{
	*value = refcallname_global(pos);
}
_g void setcallname_global(addr pos, addr value)
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
_g addr refcallnamecheck_global(addr pos)
{
	addr value;
	getcallname_global(pos, &value);
	if (value == Unbound) {
		name_callname_heap(pos, &pos);
		undefined_function(pos);
	}
	return value;
}
_g void getcallnamecheck_global(addr pos, addr *ret)
{
	getcallname_global(pos, ret);
	if (*ret == Unbound) {
		name_callname_heap(pos, &pos);
		undefined_function(pos);
	}
}

_g void name_callname_alloc(LocalRoot local, addr pos, addr *ret)
{
	enum CALLNAME_TYPE type;
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


/*
 *  function
 */
static addr alloc_function(LocalRoot local,
		addr name, addr code, int macro, int compiled)
{
	addr pos;
	struct function_struct *ptr;

	if (name != Nil) {
		if (GetType(name) != LISPTYPE_CALLNAME &&
				parse_callname_alloc(local, &name, name))
			fmte("Invalid function name ~S.", name, NULL);
	}
	alloc_smallsize(local, &pos,
			LISPTYPE_FUNCTION,
			FUNCTION_INDEX_SIZE,
			sizeoft(struct function_struct));
	SetFunction_Low(pos, code);
	SetNameFunction_Low(pos, name);
	ptr = StructFunction_Low(pos);
	memset(ptr, 0, sizeoft(struct function_struct));
	ptr->macro = macro;
	ptr->compiled = compiled;
	ptr->system = 0;
	ptr->index = p_empty;

	return pos;
}

_g addr function_allocr(LocalRoot local, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	return alloc_function(local, name, code, 0, 0);
}
_g addr function_localr(LocalRoot local, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	return alloc_function(local, name, code, 0, 0);
}
_g addr function_heapr(addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	return alloc_function(NULL, name, code, 0, 0);
}

_g void function_alloc(LocalRoot local, addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	*ret = alloc_function(local, name, code, 0, 0);
}
_g void function_local(LocalRoot local, addr *ret, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	*ret = alloc_function(local, name, code, 0, 0);
}
_g void function_heap(addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	*ret = alloc_function(NULL, name, code, 0, 0);
}

_g addr macro_allocr(LocalRoot local, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	return alloc_function(local, name, code, 1, 0);
}
_g addr macro_localr(LocalRoot local, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	return alloc_function(local, name, code, 1, 0);
}
_g addr macro_heapr(addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	return alloc_function(NULL, name, code, 1, 0);
}

_g void macro_alloc(LocalRoot local, addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	*ret = alloc_function(local, name, code, 1, 0);
}
_g void macro_local(LocalRoot local, addr *ret, addr name, addr code)
{
	Check(local == NULL, "local error");
	CheckType(code, LISPTYPE_CODE);
	*ret = alloc_function(local, name, code, 1, 0);
}
_g void macro_heap(addr *ret, addr name, addr code)
{
	CheckType(code, LISPTYPE_CODE);
	*ret = alloc_function(NULL, name, code, 1, 0);
}

_g addr compiled_allocr(LocalRoot local, addr name)
{
	return alloc_function(local, name, Nil, 0, 1);
}
_g addr compiled_localr(LocalRoot local, addr name)
{
	Check(local == NULL, "local error");
	return alloc_function(local, name, Nil, 0, 1);
}
_g addr compiled_heapr(addr name)
{
	return alloc_function(NULL, name, Nil, 0, 1);
}

_g void compiled_alloc(LocalRoot local, addr *ret, addr name)
{
	*ret = alloc_function(local, name, Nil, 0, 1);
}
_g void compiled_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	*ret = alloc_function(local, name, Nil, 0, 1);
}
_g void compiled_heap(addr *ret, addr name)
{
	*ret = alloc_function(NULL, name, Nil, 0, 1);
}

_g void compiled_setf_heap(addr *ret, addr symbol)
{
	Check(! symbolp(symbol), "type error.");
	setf_callname_heap(&symbol, symbol);
	*ret = alloc_function(NULL, symbol, Nil, 0, 1);
}

_g addr compiled_macro_allocr(LocalRoot local, addr name)
{
	return alloc_function(local, name, Nil, 1, 1);
}
_g addr compiled_macro_localr(LocalRoot local, addr name)
{
	Check(local == NULL, "local error");
	return alloc_function(local, name, Nil, 1, 1);
}
_g addr compiled_macro_heapr(addr name)
{
	return alloc_function(NULL, name, Nil, 1, 1);
}

_g void compiled_macro_alloc(LocalRoot local, addr *ret, addr name)
{
	*ret = alloc_function(local, name, Nil, 1, 1);
}
_g void compiled_macro_local(LocalRoot local, addr *ret, addr name)
{
	Check(local == NULL, "local error");
	*ret = alloc_function(local, name, Nil, 1, 1);
}
_g void compiled_macro_heap(addr *ret, addr name)
{
	*ret = alloc_function(NULL, name, Nil, 1, 1);
}

_g void setcompiled_code(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_code, "type error");
	StructFunction(pos)->index = p;
}

_g void getcompiled_code(addr pos, pointer *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	*ret = StructFunction(pos)->index;
	Check(pointer_table[*ret].type != CallBind_code, "type error");
}

_g void setcompiled_macro(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_macro, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_none(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_none, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_any(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_any, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_empty(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_empty, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var4(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var5(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var5, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var6(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var6, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt3, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt4(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt4, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt5(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt5, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var3opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var4opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var5opt1(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var5opt1, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1opt2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2opt2(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt2, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2opt3(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2opt3, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt1rest(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1rest, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var1dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var1dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var2dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var2dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var3dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var3dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_var4dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_var4dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void setcompiled_opt1dynamic(addr pos, pointer p)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(pointer_table[p].type != CallBind_opt1dynamic, "type error");
	StructFunction(pos)->index = p;
}

_g void function_heap_for_develop(addr *ret, addr name)
{
	*ret = alloc_function(NULL, name, Nil, 0, 0);
}

_g struct function_struct *structfunction(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	return StructFunction_Low(pos);
}

_g addr reffunction(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	return RefFunction_Low(pos);
}

_g void getfunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetFunction_Low(pos, ret);
}

_g void setfunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunction_Low(pos, value);
}

_g addr refnamefunction(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	return RefNameFunction_Low(pos);
}

_g void getnamefunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetNameFunction_Low(pos, ret);
}

_g void setnamefunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	CheckType(value, LISPTYPE_CALLNAME);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNameFunction_Low(pos, value);
}

_g addr refdatafunction(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	return RefDataFunction_Low(pos);
}

_g void getdatafunction(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetDataFunction_Low(pos, ret);
}

_g void setdatafunction(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDataFunction_Low(pos, value);
}

_g void getclosure_value_function(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureValueFunction_Low(pos, ret);
}

_g void setclosure_value_function(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClosureValueFunction_Low(pos, value);
}

_g void getclosure_function_function(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureFunctionFunction_Low(pos, ret);
}

_g void setclosure_function_function(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClosureFunctionFunction_Low(pos, value);
}

_g void getclosure_tagbody_function(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureTagbodyFunction_Low(pos, ret);
}

_g void setclosure_tagbody_function(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClosureTagbodyFunction_Low(pos, value);
}

_g void getclosure_block_function(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureBlockFunction_Low(pos, ret);
}

_g void setclosure_block_function(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClosureBlockFunction_Low(pos, value);
}

_g void pushclosure_value_function(addr pos, addr key, addr value)
{
	addr cons;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureValueFunction(pos, &cons);
	cons_heap(&value, key, value);
	cons_heap(&cons, value, cons);
	SetClosureValueFunction_Low(pos, cons);
}

_g void pushclosure_function_function(addr pos, addr key, addr value)
{
	addr cons;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureFunctionFunction(pos, &cons);
	cons_heap(&value, key, value);
	cons_heap(&cons, value, cons);
	SetClosureFunctionFunction_Low(pos, cons);
}

_g void pushclosure_tagbody_function(addr pos, addr key, addr value)
{
	addr cons;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureTagbodyFunction(pos, &cons);
	cons_heap(&value, key, value);
	cons_heap(&cons, value, cons);
	SetClosureTagbodyFunction_Low(pos, cons);
}

_g void pushclosure_block_function(addr pos, addr value)
{
	addr cons;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetClosureBlockFunction(pos, &cons);
	cons_heap(&cons, value, cons);
	SetClosureBlockFunction_Low(pos, cons);
}

static void getplist_function(addr pos, constindex index, addr *ret)
{
	addr type;

	CheckType(pos, LISPTYPE_FUNCTION);
	GetTableFunction_Low(pos, &pos);
	GetConstant(index, &type);
	getplist(pos, type, ret);
}
_g void gettype_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_SYSTEM_TYPE_FUNCTION, ret);
}

static void setplist_function(addr pos, constindex index, addr value)
{
	addr table, type;

	CheckType(pos, LISPTYPE_FUNCTION);
	CheckReadOnly(pos);
	GetTableFunction_Low(pos, &table);
	GetConstant(index, &type);
	if (setplist_heap(table, type, value, &table))
		SetTableFunction_Low(pos, table);
}
_g void settype_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_SYSTEM_TYPE_FUNCTION, value);
}

_g void getdocumentation_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_DOCUMENTATION, ret);
}

_g void setdocumentation_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_DOCUMENTATION, value);
}

_g void getlambda_expression_function(addr pos, addr *ret)
{
	getplist_function(pos, CONSTANT_COMMON_LAMBDA, ret);
}

_g void setlambda_expression_function(addr pos, addr value)
{
	setplist_function(pos, CONSTANT_COMMON_LAMBDA, value);
}

_g void setsystem_function(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	StructFunction_Low(pos)->system = 1;
}

_g int functionp(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION;
}

_g int funcall_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->macro == 0;
}

_g int macro_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->macro;
}

_g int interpreted_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->compiled == 0;
}

_g int interpreted_funcall_function_p(addr pos)
{
	struct function_struct *ptr;
	if (GetType(pos) != LISPTYPE_FUNCTION) return 0;
	ptr = StructFunction_Low(pos);
	return ptr->compiled == 0 && ptr->macro == 0;
}

_g int interpreted_macro_function_p(addr pos)
{
	struct function_struct *ptr;
	if (GetType(pos) != LISPTYPE_FUNCTION) return 0;
	ptr = StructFunction_Low(pos);
	return ptr->compiled == 0 && ptr->macro;
}

_g int compiled_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->compiled;
}

_g int compiled_funcall_function_p(addr pos)
{
	struct function_struct *ptr;
	if (GetType(pos) != LISPTYPE_FUNCTION) return 0;
	ptr = StructFunction_Low(pos);
	return ptr->compiled && ptr->macro == 0;
}

_g int compiled_macro_function_p(addr pos)
{
	struct function_struct *ptr;
	if (GetType(pos) != LISPTYPE_FUNCTION) return 0;
	ptr = StructFunction_Low(pos);
	return ptr->compiled && ptr->macro;
}

_g int system_function_p(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->system;
}

_g void setrecursive_function(addr pos)
{
	CheckType(pos, LISPTYPE_FUNCTION);
	Check(GetStatusReadOnly(pos), "readonly error");
	StructFunction_Low(pos)->recursive = 1;
}

_g int recursivep_function(addr pos)
{
	return GetType(pos) == LISPTYPE_FUNCTION &&
		StructFunction_Low(pos)->recursive;
}

