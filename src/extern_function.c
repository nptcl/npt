#include "callname.h"
#include "condition.h"
#include "control_object.h"
#include "extern_control.h"
#include "extern_develop.h"
#include "extern_error.h"
#include "extern_function.h"
#include "extern_object.h"
#include "function.h"
#include "integer.h"
#include "hold.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  function
 */
void lisp0_get_function(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		Lisp_abort_type(symbol, SYMBOL);
		return;
	}
	GetFunctionSymbol(symbol, &symbol);
	*ret = (symbol == Unbound)? NULL: symbol;
}

void lisp0_get_setf(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		Lisp_abort_type(symbol, SYMBOL);
		return;
	}
	getsetf_symbol(symbol, &symbol);
	*ret = (symbol == Unbound)? NULL: symbol;
}

void lisp_get_function(addr x, addr symbol)
{
	lisp0_get_function(&symbol, symbol);
	hold_set(x, symbol);
}

void lisp_get_setf(addr x, addr symbol)
{
	lisp0_get_setf(&symbol, symbol);
	hold_set(x, symbol);
}

int lisp0_get_function_(addr *ret, addr value)
{
	hold_value(value, &value);
	if (functionp(value))
		return Result(ret, value);
	if (symbolp(value))
		return getfunction_global_(value, ret);

	/* error */
	*ret = Nil;
	return TypeError_(value, SYMBOL);
}

int lisp0_get_function8_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp0_get_function16_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp0_get_function32_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return getfunction_global_(pos, ret);
}

int lisp_get_function_(addr x, addr value)
{
	Return(lisp0_get_function_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_get_function8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_function16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_function32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_function32_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp0_get_setf_(addr *ret, addr value)
{
	hold_value(value, &value);
	if (functionp(value))
		return Result(ret, value);
	if (symbolp(value))
		return getsetf_global_(value, ret);

	/* error */
	*ret = Nil;
	return TypeError_(value, SYMBOL);
}

int lisp0_get_setf8_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp0_get_setf16_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp0_get_setf32_(addr *ret, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return getsetf_global_(pos, ret);
}

int lisp_get_setf_(addr x, addr value)
{
	Return(lisp0_get_setf_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_get_setf8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_setf16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_get_setf32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_get_setf32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  compiled
 */
static void lisp_compiled_index_check(int index)
{
	addr pos;

	if (index < 0 || LISP_POINTER_EXTEND <= index) {
		fixnum_heap(&pos, (fixnum)index);
		lisp_abort8("Invalid index value ~S.", pos, NULL);
	}
}

void lisp_compiled_dynamic(int index, lisp_calltype_dynamic call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_dynamic(index, call);
}

void lisp_compiled_rest(int index, lisp_calltype_rest call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_rest(index, call);
}

void lisp_compiled_empty(int index, lisp_calltype_empty call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_empty(index, call);
}

void lisp_compiled_var1(int index, lisp_calltype_var1 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var1(index, call);
}

void lisp_compiled_var2(int index, lisp_calltype_var2 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var2(index, call);
}

void lisp_compiled_var3(int index, lisp_calltype_var3 call)
{
	lisp_compiled_index_check(index);
	index += (int)p_size;
	SetPointer_extend_var3(index, call);
}

static int lisp_compiled_function_set(addr pos, int index)
{
	switch (pointer_table[index].type) {
		case CallBind_extend_dynamic:
			setcompiled_extend_dynamic(pos, (pointer)index);
			break;

		case CallBind_extend_rest:
			setcompiled_extend_rest(pos, (pointer)index);
			break;

		case CallBind_extend_empty:
			setcompiled_extend_empty(pos, (pointer)index);
			break;

		case CallBind_extend_var1:
			setcompiled_extend_var1(pos, (pointer)index);
			break;

		case CallBind_extend_var2:
			setcompiled_extend_var2(pos, (pointer)index);
			break;

		case CallBind_extend_var3:
			setcompiled_extend_var3(pos, (pointer)index);
			break;

		default:
			return 1;
	}

	return 0;
}

int lisp0_compiled_function_(addr *ret, int index, addr symbol)
{
	addr pos;

	lisp_compiled_index_check(index);
	index += (int)p_size;
	if (symbol == NULL)
		symbol = Nil;
	hold_value(symbol, &symbol);

	if (! symbolp(symbol))
		return TypeError_(symbol, SYMBOL);

	/* function */
	compiled_system(&pos, symbol);
	if (lisp_compiled_function_set(pos, index)) {
		*ret = Nil;
		fixnum_heap(&pos, (fixnum)index);
		return lisp_error8_("Invalid callbind type, index = ~A.", pos, NULL);
	}

	return Result(ret, pos);
}

int lisp0_compiled_function8_(addr *ret, int index, const void *str)
{
	addr pos;
	Return(lisp0_intern8_(&pos, NULL, str));
	return lisp0_compiled_function_(ret, index, pos);
}

int lisp0_compiled_function16_(addr *ret, int index, const void *str)
{
	addr pos;
	Return(lisp0_intern16_(&pos, NULL, str));
	return lisp0_compiled_function_(ret, index, pos);
}

int lisp0_compiled_function32_(addr *ret, int index, const void *str)
{
	addr pos;
	Return(lisp0_intern32_(&pos, NULL, str));
	return lisp0_compiled_function_(ret, index, pos);
}

int lisp_compiled_function_(addr x, int index, addr symbol)
{
	addr pos;
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_function8_(addr x, int index, const void *str)
{
	addr pos;
	Return(lisp0_compiled_function8_(&pos, index, str));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_function16_(addr x, int index, const void *str)
{
	addr pos;
	Return(lisp0_compiled_function16_(&pos, index, str));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_function32_(addr x, int index, const void *str)
{
	addr pos;
	Return(lisp0_compiled_function32_(&pos, index, str));
	hold_set(x, pos);
	return 0;
}

int lisp_compiled_defun_(int index, addr symbol)
{
	addr pos;
	hold_value(symbol, &symbol);
	Return(lisp0_compiled_function_(&pos, index, symbol));
	return setfunction_symbol_(symbol, pos);
}

int lisp_compiled_defun8_(int index, const void *str)
{
	addr symbol, pos;

	Return(lisp0_intern8_(&symbol, NULL, str));
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_value(symbol, &symbol);
	return setfunction_symbol_(symbol, pos);
}

int lisp_compiled_defun16_(int index, const void *str)
{
	addr symbol, pos;

	Return(lisp0_intern16_(&symbol, NULL, str));
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_value(symbol, &symbol);
	return setfunction_symbol_(symbol, pos);
}

int lisp_compiled_defun32_(int index, const void *str)
{
	addr symbol, pos;

	Return(lisp0_intern32_(&symbol, NULL, str));
	Return(lisp0_compiled_function_(&pos, index, symbol));
	hold_value(symbol, &symbol);
	return setfunction_symbol_(symbol, pos);
}

static int lisp0_compiled_function_setf_(addr *ret, addr *rcall, int index, addr call)
{
	addr pos;

	lisp_compiled_index_check(index);
	index += (int)p_size;
	if (call == NULL)
		call = Nil;
	hold_value(call, &call);

	Return(parse_callname_error_(&call, call));
	SetCallNameType(call, CALLNAME_SETF);

	/* function */
	compiled_system(&pos, call);
	if (lisp_compiled_function_set(pos, index)) {
		*ret = *rcall = Nil;
		fixnum_heap(&pos, (fixnum)index);
		return lisp_error8_("Invalid callbind type, index = ~A.", pos, NULL);
	}

	*rcall = call;
	return Result(ret, pos);
}

int lisp_compiled_defun_setf_(int index, addr call)
{
	addr pos;
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

int lisp_compiled_defun_setf8_(int index, const void *str)
{
	addr pos, call;

	Return(lisp0_intern8_(&call, NULL, str));
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

int lisp_compiled_defun_setf16_(int index, const void *str)
{
	addr pos, call;

	Return(lisp0_intern16_(&call, NULL, str));
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

int lisp_compiled_defun_setf32_(int index, const void *str)
{
	addr pos, call;

	Return(lisp0_intern32_(&call, NULL, str));
	Return(lisp0_compiled_function_setf_(&pos, &call, index, call));
	return setglobal_callname_(call, pos);
}

void lisp_compiled_setvalue(addr pos, addr value)
{
	if (value == NULL)
		value = Nil;
	hold_value(pos, &pos);
	hold_value(value, &value);
	if (! functionp(pos)) {
		lisp_abort8("Invalid function object ~S.", pos, NULL);
		return;
	}
	SetDataFunction(pos, value);
}

void lisp0_compiled_getvalue(addr *ret)
{
	addr value;

	getdata_control(Execute_Thread, &value);
	if (value == Unbound)
		value = Nil;
	*ret = value;
}

