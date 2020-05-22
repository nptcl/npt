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
#include "function.h"
#include "memory.h"
#include "number.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "ratio.h"
#include "reader.h"
#include "real_float.h"
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
	list_alloc_stdarg(NULL, &car, args);
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

static void lisp_typecheck_list(addr pos)
{
	if (! listp(pos))
		TypeError(pos, LIST);
}

static void lisp_typecheck_cons(addr pos)
{
	if (! consp(pos))
		TypeError(pos, CONS);
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

addr lisp_car(addr list)
{
	lisp_typecheck_list(list);
	GetCar(list, &list);
	return list;
}

addr lisp_cdr(addr list)
{
	lisp_typecheck_list(list);
	GetCdr(list, &list);
	return list;
}

void lisp_carcdr(addr list, addr *car, addr *cdr)
{
	lisp_typecheck_list(list);
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
	lisp_typecheck_cons(cons);
	SetCar(cons, value);
}

void lisp_setf_cdr(addr cons, addr value)
{
	lisp_typecheck_cons(cons);
	SetCdr(cons, value);
}

void lisp_setf_carcdr(addr cons, addr car, addr cdr)
{
	lisp_typecheck_cons(cons);
	SetCons(cons, car, cdr);
}


/*
 *  list
 */
addr lisp_reverse(addr list)
{
	lisp_typecheck_list(list);
	reverse_list_heap_safe(&list, list);
	return list;
}

addr lisp_nreverse(addr list)
{
	lisp_typecheck_list(list);
	return nreverse_list_safe_inplace(list);
}


/*
 *  sequence
 */
static void lisp_typecheck_sequence(addr pos)
{
	if (GetType(pos) != LISPTYPE_VECTOR)
		TypeError(pos, VECTOR);
}

addr lisp_getelt(addr pos, size_t index)
{
	lisp_typecheck_sequence(pos);
	getelt_sequence(NULL, pos, index, &pos);
	return pos;
}

void lisp_setelt(addr pos, size_t index, addr value)
{
	lisp_typecheck_sequence(pos);
	setelt_sequence(pos, index, value);
}

size_t lisp_length(addr pos)
{
	lisp_typecheck_sequence(pos);
	return length_sequence(pos, 1);
}


/*
 *  string
 */
addr lisp_string8(const void *str)
{
	addr x;
	string8_null_heap(&x, (const char *)str);
	return x;
}

addr lisp_string16(const void *str)
{
	addr x;
	string16_null_heap(&x, (const byte16 *)str);
	return x;
}


/*
 *  package
 */
addr lisp_package(addr pos)
{
	if (! stringp(pos))
		TypeError(pos, STRING);
	find_package(pos, &pos);
	return pos;
}

addr lisp_package8(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	string8_null_local(local, &x, (const char *)str);
	find_package(x, &x);
	rollback_local(local, stack);

	return x;
}

addr lisp_package16(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	string16_null_local(local, &x, (const byte16 *)str);
	find_package(x, &x);
	rollback_local(local, stack);

	return x;
}


/*
 *  intern
 */
addr lisp_intern(addr package, addr name)
{
	if (package == Nil || package == NULL)
		getpackage(Execute_Thread, &package);
	else if (! packagep(package))
		package = lisp_package(package);
	intern_package(package, name, &name);

	return name;
}

addr lisp_intern8(const void *package, const void *name)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	if (package == NULL)
		x = Nil;
	else
		string8_null_local(local, &x, (const char *)package);
	string8_null_heap(&y, (const char *)name);
	x = lisp_intern(x, y);
	rollback_local(local, stack);

	return x;
}

addr lisp_intern16(const void *package, const void *name)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	if (package == NULL)
		x = Nil;
	else
		string16_null_local(local, &x, (const byte16 *)package);
	string16_null_heap(&y, (const byte16 *)name);
	x = lisp_intern(x, y);
	rollback_local(local, stack);

	return x;
}


/* reader */
int lisp_reader(addr *ret, addr str)
{
	int check;
	addr value;

	Return(read_from_string(Execute_Thread, &check, &value, str));
	*ret = check? NULL: value;

	return 0;
}

int lisp_reader8(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	string8_null_local(local, &x, (const char *)str);
	Return(lisp_reader(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp_reader16(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	string16_null_local(local, &x, (const byte16 *)str);
	Return(lisp_reader(ret, x));
	rollback_local(local, stack);

	return 0;
}


/*
 *  let
 */
void lisp_push_special(addr symbol, addr value)
{
	if (! symbolp(symbol))
		fmte("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	pushspecial_control(Execute_Thread, symbol, value);
}

void lisp_push_special8(const void *name, addr value)
{
	addr symbol;
	symbol = lisp_intern8(NULL, name);
	lisp_push_special(symbol, value);
}

void lisp_push_special16(const void *name, addr value)
{
	addr symbol;
	symbol = lisp_intern16(NULL, name);
	lisp_push_special(symbol, value);
}

addr lisp_get_special(addr symbol)
{
	if (! symbolp(symbol))
		fmte("The argument ~S must be a symbol type.", symbol, NULL);
	getspecial_local(Execute_Thread, symbol, &symbol);
	return (symbol == Unbound)? NULL: symbol;
}

addr lisp_get_special8(const void *name)
{
	addr symbol;
	symbol = lisp_intern8(NULL, name);
	return lisp_get_special(symbol);
}

addr lisp_get_special16(const void *name)
{
	addr symbol;
	symbol = lisp_intern16(NULL, name);
	return lisp_get_special(symbol);
}

void lisp_set_special(addr symbol, addr value)
{
	if (! symbolp(symbol))
		fmte("The argument ~S must be a symbol type.", symbol, NULL);
	if (value == NULL)
		value = Unbound;
	setspecial_local(Execute_Thread, symbol, value);
}

void lisp_set_special8(const void *name, addr value)
{
	addr symbol;
	symbol = lisp_intern8(NULL, name);
	lisp_set_special(symbol, value);
}

void lisp_set_special16(const void *name, addr value)
{
	addr symbol;
	symbol = lisp_intern16(NULL, name);
	lisp_set_special(symbol, value);
}


/*
 *  pathname
 */
addr lisp_pathname(addr name)
{
	pathname_designer_heap(Execute_Thread, name, &name);
	return name;
}

addr lisp_namestring(addr path)
{
	namestring_pathname(Execute_Thread, &path, path);
	return path;
}


/*
 *  number
 */
addr lisp_character(unicode value)
{
	addr x;
	character_heap(&x, value);
	return x;
}

addr lisp_fixnum(fixnum value)
{
	addr x;
	fixnum_heap(&x, value);
	return x;
}

addr lisp_float(float value)
{
	addr x;
	single_float_heap(&x, value);
	return x;
}

addr lisp_double(double value)
{
	addr x;
	double_float_heap(&x, value);
	return x;
}

addr lisp_long_double(long double value)
{
	addr x;
	long_float_heap(&x, value);
	return x;
}

int lisp_zerop(addr value)
{
	return zerop_number(value);
}

int lisp_plusp(addr value)
{
	return plusp_number(value);
}

int lisp_minusp(addr value)
{
	return minusp_number(value);
}

unicode lisp_get_character(addr pos)
{
	if (! characterp(pos))
		fmte("The argument ~S must be a character type.", pos, NULL);
	return RefCharacter(pos);
}

float lisp_get_float(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_ss_value(pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_ds_value(pos);

		case LISPTYPE_LONG_FLOAT:
			return cast_ls_value(pos);

		case LISPTYPE_FIXNUM:
			return single_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return single_float_bignum(pos);

		case LISPTYPE_RATIO:
			return single_float_ratio(pos);

		default:
			fmte("The argument ~S must be a real type.", pos, NULL);
			return 0.0f;
	}
}

double lisp_get_double(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sd_value(pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dd_value(pos);

		case LISPTYPE_LONG_FLOAT:
			return cast_ld_value(pos);

		case LISPTYPE_FIXNUM:
			return double_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return double_float_bignum(pos);

		case LISPTYPE_RATIO:
			return double_float_ratio(pos);

		default:
			fmte("The argument ~S must be a real type.", pos, NULL);
			return 0.0;
	}
}

long double lisp_get_long_double(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sl_value(pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dl_value(pos);

		case LISPTYPE_LONG_FLOAT:
			return cast_ll_value(pos);

		case LISPTYPE_FIXNUM:
			return long_float_fixnum(pos);

		case LISPTYPE_BIGNUM:
			return long_float_bignum(pos);

		case LISPTYPE_RATIO:
			return long_float_ratio(pos);

		default:
			fmte("The argument ~S must be a real type.", pos, NULL);
			return 0.0;
	}
}


/*
 *  call
 */
addr lisp_function(addr value)
{
	if (functionp(value))
		return value;
	getfunction_global(value, &value);
	return value;
}

addr lisp_function8(const void *str)
{
	addr value = lisp_intern8(NULL, str);
	return lisp_function(value);
}

addr lisp_function16(const void *str)
{
	addr value = lisp_intern16(NULL, str);
	return lisp_function(value);
}

int lisp_funcall(addr *ret, addr call, ...)
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
	list_alloc_stdarg(local, &args, va);
	va_end(va);

	call = lisp_function(call);
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall8(addr *ret, const void *str, ...)
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
	list_alloc_stdarg(local, &args, va);
	va_end(va);

	call = lisp_function8(str);
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_funcall16(addr *ret, const void *str, ...)
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
	list_alloc_stdarg(local, &args, va);
	va_end(va);

	call = lisp_function16(str);
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply(addr *ret, addr call, ...)
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

	call = lisp_function(call);
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply8(addr *ret, const void *str, ...)
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

	call = lisp_function8(str);
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

int lisp_apply16(addr *ret, const void *str, ...)
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

	call = lisp_function16(str);
	Return(callclang_apply(ptr, ret, call, args));
	rollback_local(local, stack);

	return 0;
}

