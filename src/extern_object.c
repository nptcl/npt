#include <stdarg.h>
#include "array.h"
#include "array_vector.h"
#include "bignum.h"
#include "build.h"
#include "character.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "float_object.h"
#include "function.h"
#include "extern_error.h"
#include "memory.h"
#include "number.h"
#include "number_equal.h"
#include "object.h"
#include "package.h"
#include "package_symbol.h"
#include "pathname_common.h"
#include "pathname.h"
#include "ratio.h"
#include "reader.h"
#include "real_equal.h"
#include "sequence.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type_constant.h"
#include "type_table.h"
#include "unicode.h"

/*
 *  nil, t
 */
addr lisp_nil(void)
{
	return Nil;
}

addr lisp_t(void)
{
	return T;
}


/*
 *  type
 */
int lisp_nil_p(addr x)
{
	return x == Nil;
}

int lisp_t_p(addr x)
{
	return x == T;
}

int lisp_character_p(addr x)
{
	return characterp(x);
}

int lisp_cons_p(addr x)
{
	return consp(x);
}

int lisp_list_p(addr x)
{
	return listp(x);
}

int lisp_string_p(addr x)
{
	return stringp(x);
}

int lisp_symbol_p(addr x)
{
	return symbolp(x);
}

int lisp_array_p(addr x)
{
	return arrayp(x);
}

int lisp_vector_p(addr x)
{
	return vector_type_p(x);
}


/*
 *  cons
 */
addr lisp_cons(addr car, addr cdr)
{
	cons_heap(&car, car, cdr);
	return car;
}

addr lisp_list(addr car, ...)
{
	va_list args;

	va_start(args, car);
	list_stdarg_alloc(NULL, &car, args);
	va_end(args);

	return car;
}

addr lisp_lista(addr car, ...)
{
	va_list args;
	addr cdr;

	va_start(args, car);
	lista_stdarg_noerror(NULL, &cdr, args);
	va_end(args);
	cons_heap(&cdr, car, cdr);

	return cdr;
}

addr lisp_vector(size_t size)
{
	addr x;
	vector_heap(&x, size);
	return x;
}

addr lisp0_car(addr list)
{
	Check(! listp(list), "type error");
	GetCar(list, &list);
	return list;
}

addr lisp0_cdr(addr list)
{
	Check(! listp(list), "type error");
	GetCdr(list, &list);
	return list;
}

void lisp0_carcdr(addr list, addr *car, addr *cdr)
{
	Check(! listp(list), "type error");
	GetCons(list, car, cdr);
}

void lisp_car(addr list, addr *ret)
{
	if (! listp(list))
		Lisp_abort_type(list, LIST);
	GetCar(list, ret);
}

void lisp_cdr(addr list, addr *ret)
{
	if (! listp(list))
		Lisp_abort_type(list, LIST);
	GetCdr(list, ret);
}

void lisp_carcdr(addr list, addr *car, addr *cdr)
{
	if (! listp(list))
		Lisp_abort_type(list, LIST);
	GetCons(list, car, cdr);
}

void lisp0_setf_car(addr cons, addr value)
{
	SetCar(cons, value);
}

void lisp0_setf_cdr(addr cons, addr value)
{
	SetCdr(cons, value);
}

void lisp0_setf_carcdr(addr cons, addr car, addr cdr)
{
	SetCons(cons, car, cdr);
}

void lisp_setf_car(addr cons, addr value)
{
	if (! consp(cons))
		Lisp_abort_type(cons, CONS);
	SetCar(cons, value);
}

void lisp_setf_cdr(addr cons, addr value)
{
	if (! consp(cons))
		Lisp_abort_type(cons, CONS);
	SetCdr(cons, value);
}

void lisp_setf_carcdr(addr cons, addr car, addr cdr)
{
	if (! consp(cons))
		Lisp_abort_type(cons, CONS);
	SetCons(cons, car, cdr);
}


/*
 *  list
 */
int lisp_reverse_(addr *ret, addr list)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	reverse_list_heap_safe(ret, list);
	return 0;
}

int lisp_nreverse_(addr *ret, addr list)
{
	if (! listp(list))
		return TypeError_(list, LIST);
	nreverse_list_safe(ret, list);
	return 0;
}


/*
 *  sequence
 */
int lisp_getelt_(addr pos, size_t index, addr *ret)
{
	if (! sequencep(pos))
		return TypeError_(pos, SEQUENCE);
	return getelt_sequence_(NULL, pos, index, ret);
}

int lisp_setelt_(addr pos, size_t index, addr value)
{
	if (! sequencep(pos))
		return TypeError_(pos, SEQUENCE);
	return setelt_sequence_(pos, index, value);
}

int lisp_length_(addr pos, size_t *ret)
{
	if (! sequencep(pos))
		return TypeError_(pos, SEQUENCE);
	return length_sequence_(pos, 1, ret);
}


/*
 *  string
 */
int lisp_string8_(addr *ret, const void *str)
{
	return string8_null_heap_(ret, (const char *)str);
}

int lisp_string16_(addr *ret, const void *str)
{
	return string16_null_heap_(ret, (const byte16 *)str);
}


/*
 *  package
 */
int lisp_package_(addr *ret, addr pos)
{
	if (! stringp(pos))
		return TypeError_(pos, STRING);
	return find_package_(pos, ret);
}

int lisp_package8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(find_package_(x, ret));
	rollback_local(local, stack);

	return 0;
}

int lisp_package16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(find_package_(x, ret));
	rollback_local(local, stack);

	return 0;
}


/*
 *  intern
 */
int lisp_intern_(addr *ret, addr package, addr name)
{
	if (package == Nil || package == NULL) {
		Return(getpackage_(Execute_Thread, &package));
	}
	else if (! packagep(package)) {
		Return(lisp_package_(&package, package));
	}

	return intern_package_(package, name, ret, NULL);
}

int lisp_intern8_(addr *ret, const void *package, const void *name)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	if (package == NULL) {
		x = Nil;
	}
	else {
		Return(string8_null_local_(local, &x, (const char *)package));
	}
	Return(string8_null_heap_(&y, (const char *)name));
	Return(lisp_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}

int lisp_intern16_(addr *ret, const void *package, const void *name)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	if (package == NULL) {
		x = Nil;
	}
	else {
		Return(string16_null_local_(local, &x, (const byte16 *)package));
	}
	Return(string16_null_heap_(&y, (const byte16 *)name));
	Return(lisp_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}


/* reader */
int lisp_reader_(addr *ret, addr str)
{
	int check;
	addr value;

	Return(read_from_string(Execute_Thread, &check, &value, str));
	*ret = check? NULL: value;

	return 0;
}

int lisp_reader8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(lisp_reader_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp_reader16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(lisp_reader_(ret, x));
	rollback_local(local, stack);

	return 0;
}


/*
 *  let
 */
int lisp_push_special_(addr symbol, addr value)
{
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	pushspecial_control(Execute_Thread, symbol, value);
	return 0;
}

int lisp_push_special8_(const void *name, addr value)
{
	addr symbol;
	Return(lisp_intern8_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_push_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp_intern16_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_get_special_(addr symbol, addr *ret)
{
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	getspecial_local(Execute_Thread, symbol, &symbol);
	return Result(ret, (symbol == Unbound)? NULL: symbol);
}

int lisp_get_special8_(const void *name, addr *ret)
{
	addr symbol;
	Return(lisp_intern8_(&symbol, NULL, name));
	return lisp_get_special_(symbol, ret);
}

int lisp_get_special16_(const void *name, addr *ret)
{
	addr symbol;
	Return(lisp_intern16_(&symbol, NULL, name));
	return lisp_get_special_(symbol, ret);
}

int lisp_set_special_(addr symbol, addr value)
{
	if (! symbolp(symbol))
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	setspecial_local(Execute_Thread, symbol, value);
	return 0;
}

int lisp_set_special8_(const void *name, addr value)
{
	addr symbol;
	Return(lisp_intern8_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}

int lisp_set_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp_intern16_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}


/*
 *  pathname
 */
int lisp_pathname_(addr *ret, addr name)
{
	return pathname_designer_heap_(Execute_Thread, name, ret);
}

int lisp_namestring_(addr *ret, addr path)
{
	return namestring_pathname_(Execute_Thread, ret, path);
}


/*
 *  number
 */
int lisp_character_(addr *ret, unicode value)
{
	character_heap(ret, value);
	return 0;
}

void lisp_fixnum(addr *ret, fixnum value)
{
	fixnum_heap(ret, value);
}

int lisp_float_(addr *ret, float value)
{
	single_float_heap(ret, value);
	return 0;
}

int lisp_double_(addr *ret, double value)
{
	double_float_heap(ret, value);
	return 0;
}

int lisp_long_double_(addr *ret, long double value)
{
	long_float_heap(ret, value);
	return 0;
}

int lisp_zerop(addr value)
{
	int check;

	if (value == NULL)
		return 0;
	if (! numberp(value))
		return 0;
	(void)zerop_number_(value, &check);
	return check;
}

int lisp_plusp(addr value)
{
	return value && realp(value) && plusp_real_inplace(value);
}

int lisp_minusp(addr value)
{
	return value && realp(value) && minusp_real_inplace(value);
}

int lisp_get_character_(addr pos, unicode *ret)
{
	if (! characterp(pos))
		return fmte_("The argument ~S must be a character type.", pos, NULL);
	GetCharacter(pos, ret);
	return 0;
}

int lisp_get_float_(addr pos, float *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_ss_value(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_ds_value(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ls_value(pos);
			break;

		case LISPTYPE_FIXNUM:
			*ret = single_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = single_float_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = single_float_ratio(pos);
			break;

		default:
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}

	return 0;
}

int lisp_get_double_(addr pos, double *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_sd_value(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_dd_value(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ld_value(pos);
			break;

		case LISPTYPE_FIXNUM:
			*ret = double_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = double_float_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = double_float_ratio(pos);
			break;

		default:
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}

	return 0;
}

int lisp_get_long_double_(addr pos, long double *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			*ret = cast_sl_value(pos);
			break;

		case LISPTYPE_DOUBLE_FLOAT:
			*ret = cast_dl_value(pos);
			break;

		case LISPTYPE_LONG_FLOAT:
			*ret = cast_ll_value(pos);
			break;

		case LISPTYPE_FIXNUM:
			*ret = long_float_fixnum(pos);
			break;

		case LISPTYPE_BIGNUM:
			*ret = long_float_bignum(pos);
			break;

		case LISPTYPE_RATIO:
			*ret = long_float_ratio(pos);
			break;

		default:
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}

	return 0;
}


/*
 *  call
 */
int lisp_function_(addr value, addr *ret)
{
	if (functionp(value))
		return Result(ret, value);
	if (! symbolp(value))
		return TypeError_(value, SYMBOL);
	getfunction_global(value, ret);
	return 0;
}

int lisp_function8_(const void *str, addr *ret)
{
	addr value;
	Return(lisp_intern8_(&value, NULL, str));
	return lisp_function_(value, ret);
}

int lisp_function16_(const void *str, addr *ret)
{
	addr value;
	Return(lisp_intern16_(&value, NULL, str));
	return lisp_function_(value, ret);
}

int lisp_funcall_(addr *ret, addr call, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, call);
	list_stdarg_alloc(local, &args, va);
	va_end(va);

	Return(lisp_function_(call, &call));
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall8_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	list_stdarg_alloc(local, &args, va);
	va_end(va);

	Return(lisp_function8_(str, &call));
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall16_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	list_stdarg_alloc(local, &args, va);
	va_end(va);

	Return(lisp_function16_(str, &call));
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply_(addr *ret, addr call, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, call);
	lista_stdarg_safe(local, &args, va);
	va_end(va);

	Return(lisp_function_(call, &call));
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply8_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lista_stdarg_safe(local, &args, va);
	va_end(va);

	Return(lisp_function8_(str, &call));
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply16_(addr *ret, const void *str, ...)
{
	Execute ptr;
	LocalRoot local;
	LocalStack stack;
	addr call, args;
	va_list va;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	va_start(va, str);
	lista_stdarg_safe(local, &args, va);
	va_end(va);

	Return(lisp_function16_(str, &call));
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

