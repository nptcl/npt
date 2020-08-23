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
#include "extern_error.h"
#include "float_object.h"
#include "function.h"
#include "hold.h"
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
 *  hold
 */
int lisp_hold_p(addr x)
{
	return holdp(x);
}

void lisp_hold_value(addr x, addr *ret)
{
	hold_value(x, ret);
}

void lisp_hold_set(addr x, addr value)
{
	hold_set(x, value);
}

addr lisp_holdv(addr x)
{
	return holdv(x);
}

void lisp_hold(addr *ret)
{
	Hold_local(ret, Nil);
}

addr Lisp_hold(void)
{
	addr x;
	Hold_local(&x, Nil);
	return x;
}


/*
 *  nil, t
 */
void lisp0_nil(addr *ret)
{
	*ret = Nil;
}

void lisp0_t(addr *ret)
{
	*ret = T;
}

void lisp_nil(addr x)
{
	hold_set(x, Nil);
}

void lisp_t(addr x)
{
	hold_set(x, T);
}


/*
 *  type
 */
int lisp_nil_p(addr x)
{
	hold_value(x, &x);
	return x == Nil;
}

int lisp_t_p(addr x)
{
	hold_value(x, &x);
	return x == T;
}

int lisp_null_p(addr x)
{
	hold_value(x, &x);
	return x == NULL;
}

int lisp_character_p(addr x)
{
	hold_value(x, &x);
	return characterp(x);
}

int lisp_cons_p(addr x)
{
	hold_value(x, &x);
	return consp(x);
}

int lisp_list_p(addr x)
{
	hold_value(x, &x);
	return listp(x);
}

int lisp_string_p(addr x)
{
	hold_value(x, &x);
	return stringp(x);
}

int lisp_symbol_p(addr x)
{
	hold_value(x, &x);
	return symbolp(x);
}

int lisp_array_p(addr x)
{
	hold_value(x, &x);
	return arrayp(x);
}

int lisp_vector_p(addr x)
{
	hold_value(x, &x);
	return vector_type_p(x);
}


/*
 *  cons
 */
void lisp0_cons(addr *ret, addr car, addr cdr)
{
	hold_value(car, &car);
	hold_value(cdr, &cdr);
	cons_heap(ret, car, cdr);
}

void lisp_cons(addr x, addr car, addr cdr)
{
	hold_value(car, &car);
	hold_value(cdr, &cdr);
	cons_heap(&car, car, cdr);
	hold_set(x, car);
}

void lisp_vector(addr x, size_t size)
{
	addr pos;
	vector_heap(&pos, size);
	hold_set(x, pos);
}

void lisp0_car(addr *ret, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCar(list, ret);
}

void lisp0_cdr(addr *ret, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCdr(list, ret);
}

void lisp0_carcdr(addr *car, addr *cdr, addr list)
{
	hold_value(list, &list);
	Check(! listp(list), "type error");
	GetCons(list, car, cdr);
}

void lisp_car(addr x, addr list)
{
	hold_value(list, &list);
	if (! listp(list))
		Lisp_abort_type(list, LIST);
	GetCar(list, &list);
	hold_set(x, list);
}

void lisp_cdr(addr x, addr list)
{
	hold_value(list, &list);
	if (! listp(list))
		Lisp_abort_type(list, LIST);
	GetCdr(list, &list);
	hold_set(x, list);
}

void lisp_carcdr(addr x, addr y, addr list)
{
	addr car, cdr;

	hold_value(list, &list);
	if (! listp(list))
		Lisp_abort_type(list, LIST);
	GetCons(list, &car, &cdr);
	hold_set(x, car);
	hold_set(y, cdr);
}

void lisp_setf_car(addr cons, addr value)
{
	hold_value(cons, &cons);
	if (! consp(cons))
		Lisp_abort_type(cons, CONS);
	hold_value(value, &value);
	SetCar(cons, value);
}

void lisp0_setf_cdr(addr cons, addr value)
{
	hold_value(cons, &cons);
	if (! consp(cons))
		Lisp_abort_type(cons, CONS);
	hold_value(value, &value);
	SetCdr(cons, value);
}

void lisp_setf_carcdr(addr cons, addr car, addr cdr)
{
	hold_value(cons, &cons);
	if (! consp(cons))
		Lisp_abort_type(cons, CONS);
	hold_value(car, &car);
	hold_value(cdr, &cdr);
	SetCons(cons, car, cdr);
}


/*
 *  list
 */
static void lisp0_list_va_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr x, y, next;

	x = va_arg(args, addr);
	if (x == NULL) {
		*ret = Nil;
		return;
	}
	hold_value(x, &x);
	conscar_alloc(local, &y, x);
	*ret = y;

	for (;;) {
		x = va_arg(args, addr);
		if (x == NULL)
			break;
		hold_value(x, &x);
		conscar_alloc(local, &next, x);
		SetCdr(y, next);
		y = next;
	}
}

void lisp0_list_va(addr *ret, va_list args)
{
	lisp0_list_va_alloc(NULL, ret, args);
}

static void lisp0_lista_va_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr pos1, pos2, pos3, cons;

	pos1 = va_arg(args, addr);
	/* nil */
	if (pos1 == NULL) {
		*ret = Nil; /* error */
		return;
	}
	hold_value(pos1, &pos1);

	/* dot list */
	pos2 = va_arg(args, addr);
	if (pos2 == NULL) {
		*ret = pos1;
		return;
	}
	hold_value(pos2, &pos2);

	/* result */
	conscar_alloc(local, &cons, pos1);
	*ret = cons;

	/* loop */
	for (;;) {
		pos3 = va_arg(args, addr);
		if (pos3 == NULL) {
			/* (pos1 . pos2) */
			SetCdr(cons, pos2);
			return;
		}
		hold_value(pos3, &pos3);

		/* (pos1 pos2 . ?) */
		conscar_alloc(local, &pos1, pos2);
		SetCdr(cons, pos1);
		cons = pos1;
		pos2 = pos3;
	}
}

void lisp0_lista_va(addr *ret, va_list args)
{
	lisp0_lista_va_alloc(NULL, ret, args);
}

void lisp0_list(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lisp0_list_va(ret, args);
	va_end(args);
}

void lisp_list(addr x, ...)
{
	addr pos;
	va_list args;

	va_start(args, x);
	lisp0_list_va(&pos, args);
	va_end(args);

	hold_set(x, pos);
}

void lisp0_lista(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	lisp0_lista_va(ret, args);
	va_end(args);
}

void lisp_lista(addr x, ...)
{
	addr pos;
	va_list args;

	va_start(args, x);
	lisp0_lista_va(&pos, args);
	va_end(args);

	hold_set(x, pos);
}

int lisp0_reverse_(addr *ret, addr list)
{
	hold_value(list, &list);
	if (! listp(list))
		return TypeError_(list, LIST);

	return reverse_list_heap_safe_(ret, list);
}

int lisp0_nreverse_(addr *ret, addr list)
{
	hold_value(list, &list);
	if (! listp(list))
		return TypeError_(list, LIST);

	return nreverse_list_safe_(ret, list);
}

int lisp_reverse_(addr x, addr list)
{
	addr pos;

	Return(lisp0_reverse_(&pos, list));
	hold_set(x, pos);
	return 0;
}

int lisp_nreverse_(addr x, addr list)
{
	addr pos;

	Return(lisp0_nreverse_(&pos, list));
	hold_set(x, pos);
	return 0;
}


/*
 *  sequence
 */
int lisp0_getelt_(addr *ret, addr pos, size_t index)
{
	hold_value(pos, &pos);
	if (! sequencep(pos))
		return TypeError_(pos, SEQUENCE);

	return getelt_sequence_(NULL, pos, index, ret);
}

int lisp_getelt_(addr x, addr pos, size_t index)
{
	Return(lisp0_getelt_(&pos, pos, index));
	hold_set(x, pos);
	return 0;
}

int lisp_setelt_(addr pos, size_t index, addr value)
{
	hold_value(pos, &pos);
	if (! sequencep(pos))
		return TypeError_(pos, SEQUENCE);

	hold_value(value, &value);
	return setelt_sequence_(pos, index, value);
}

int lisp_length_(addr pos, size_t *ret)
{
	hold_value(pos, &pos);
	if (! sequencep(pos))
		return TypeError_(pos, SEQUENCE);

	return length_sequence_(pos, 1, ret);
}


/*
 *  string
 */
int lisp0_string8_(addr *ret, const void *str)
{
	return string8_null_heap_(ret, (const char *)str);
}

int lisp0_string16_(addr *ret, const void *str)
{
	return string16_null_heap_(ret, (const byte16 *)str);
}

int lisp0_string32_(addr *ret, const void *str)
{
	return string32_null_heap_(ret, (const unicode *)str);
}

int lisp_string8_(addr x, const void *str)
{
	addr pos;

	Return(string8_null_heap_(&pos, (const char *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string16_(addr x, const void *str)
{
	addr pos;

	Return(string16_null_heap_(&pos, (const byte16 *)str));
	hold_set(x, pos);
	return 0;
}

int lisp_string32_(addr x, const void *str)
{
	addr pos;

	Return(string32_null_heap_(&pos, (const unicode *)str));
	hold_set(x, pos);
	return 0;
}


/*
 *  package
 */
int lisp0_package_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
	if (! stringp(pos))
		return TypeError_(pos, STRING);

	return find_package_(pos, ret);
}

int lisp0_package8_(addr *ret, const void *str)
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

int lisp0_package16_(addr *ret, const void *str)
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

int lisp0_package32_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(find_package_(x, ret));
	rollback_local(local, stack);

	return 0;
}

int lisp_package_(addr x, addr pos)
{
	hold_value(pos, &pos);
	Return(lisp0_package_(&pos, pos));
	hold_set(x, pos);
	return 0;
}

int lisp_package8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_package8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_package16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_package16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_package32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_package32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  intern
 */
int lisp0_intern_(addr *ret, addr package, addr name)
{
	hold_value(package, &package);
	hold_value(name, &name);
	if (package == Nil || package == NULL) {
		Return(getpackage_(Execute_Thread, &package));
	}
	else if (! packagep(package)) {
		Return(lisp0_package_(&package, package));
	}

	return intern_package_(package, name, ret, NULL);
}

int lisp0_intern8_(addr *ret, const void *package, const void *name)
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
	Return(lisp0_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}

int lisp0_intern16_(addr *ret, const void *package, const void *name)
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
	Return(lisp0_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}

int lisp0_intern32_(addr *ret, const void *package, const void *name)
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
		Return(string32_null_local_(local, &x, (const unicode *)package));
	}
	Return(string32_null_heap_(&y, (const unicode *)name));
	Return(lisp0_intern_(ret, x, y));
	rollback_local(local, stack);

	return 0;
}

int lisp_intern_(addr x, addr package, addr name)
{
	addr pos;

	hold_value(package, &package);
	hold_value(name, &name);
	Return(lisp0_intern_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}

int lisp_intern8_(addr x, const void *package, const void *name)
{
	addr pos;

	Return(lisp0_intern8_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}

int lisp_intern16_(addr x, const void *package, const void *name)
{
	addr pos;

	Return(lisp0_intern16_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}

int lisp_intern32_(addr x, const void *package, const void *name)
{
	addr pos;

	Return(lisp0_intern32_(&pos, package, name));
	hold_set(x, pos);
	return 0;
}


/* reader */
int lisp0_reader_(addr *ret, addr str)
{
	int check;
	addr value;

	hold_value(str, &str);
	Return(read_from_string(Execute_Thread, &check, &value, str));
	return Result(ret, check? NULL: value);
}

int lisp0_reader8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(lisp0_reader_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_reader16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(lisp0_reader_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_reader32_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(lisp0_reader_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp_reader_(addr x, addr str)
{
	Return(lisp0_reader_(&str, str));
	hold_set(x, str);
	return 0;
}

int lisp_reader8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_reader8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_reader16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_reader16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_reader32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_reader32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  let
 */
int lisp_push_special_(addr symbol, addr value)
{
	hold_value(symbol, &symbol);
	hold_value(value, &value);
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
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_push_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp_push_special32_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp_push_special_(symbol, value);
}

int lisp0_get_special_(addr *ret, addr symbol)
{
	hold_value(symbol, &symbol);
	if (! symbolp(symbol)) {
		*ret = Nil;
		return fmte_("The argument ~S must be a symbol type.", symbol, NULL);
	}
	getspecial_local(Execute_Thread, symbol, &symbol);
	return Result(ret, (symbol == Unbound)? NULL: symbol);
}

int lisp0_get_special8_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp0_get_special16_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp0_get_special32_(addr *ret, const void *name)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp0_get_special_(ret, symbol);
}

int lisp_get_special_(addr x, addr symbol)
{
	Return(lisp0_get_special_(&symbol, symbol));
	hold_set(x, symbol);
	return 0;
}

int lisp_get_special8_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special8_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_get_special16_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special16_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_get_special32_(addr x, const void *name)
{
	addr pos;

	Return(lisp0_get_special32_(&pos, name));
	hold_set(x, pos);
	return 0;
}

int lisp_set_special_(addr symbol, addr value)
{
	hold_value(symbol, &symbol);
	hold_value(value, &value);
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
	Return(lisp0_intern8_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}

int lisp_set_special16_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern16_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}

int lisp_set_special32_(const void *name, addr value)
{
	addr symbol;
	Return(lisp0_intern32_(&symbol, NULL, name));
	return lisp_set_special_(symbol, value);
}


/*
 *  pathname
 */
int lisp0_pathname_(addr *ret, addr name)
{
	hold_value(name, &name);
	return pathname_designer_heap_(Execute_Thread, name, ret);
}

int lisp0_namestring_(addr *ret, addr path)
{
	hold_value(path, &path);
	return namestring_pathname_(Execute_Thread, ret, path);
}

int lisp_pathname_(addr x, addr name)
{
	Return(lisp0_pathname_(&name, name));
	hold_set(x, name);
	return 0;
}

int lisp_namestring_(addr x, addr path)
{
	Return(lisp0_namestring_(&path, path));
	hold_set(x, path);
	return 0;
}


/*
 *  number
 */
int lisp0_character_(addr *ret, unicode value)
{
	return character_unicode_heap(ret, value);
}

void lisp0_fixnum(addr *ret, fixnum value)
{
	fixnum_heap(ret, value);
}

int lisp0_float_(addr *ret, float value)
{
	return single_float_check_heap_(ret, value);
}

int lisp0_double_(addr *ret, double value)
{
	return double_float_check_heap_(ret, value);
}

int lisp0_long_double_(addr *ret, long double value)
{
	return long_float_check_heap_(ret, value);
}

int lisp_character(addr x, unicode value)
{
	addr pos;

	Return(character_unicode_heap(&pos, value));
	hold_set(x, pos);
	return 0;
}

void lisp_fixnum(addr x, fixnum value)
{
	addr pos;
	fixnum_heap(&pos, value);
	hold_set(x, pos);
}

int lisp_float(addr x, float value)
{
	addr pos;

	Return(single_float_check_heap_(&pos, value));
	hold_set(x, pos);
	return 0;
}

int lisp_double_(addr x, double value)
{
	addr pos;

	Return(double_float_check_heap_(&pos, value));
	hold_set(x, pos);
	return 0;
}

int lisp_long_double_(addr x, long double value)
{
	addr pos;

	Return(long_float_check_heap_(&pos, value));
	hold_set(x, pos);
	return 0;
}

int lisp_zerop(addr value)
{
	int check;

	hold_value(value, &value);
	if (! numberp(value))
		return 0;
	Error(zerop_number_(value, &check));
	return check;
}

int lisp_plusp(addr value)
{
	int check;

	hold_value(value, &value);
	if (! realp(value))
		return 0;
	Error(plusp_real_(value, &check));
	return check;
}

int lisp_minusp(addr value)
{
	int check;

	hold_value(value, &value);
	if (! realp(value))
		return 0;
	Error(minusp_real_(value, &check));
	return check;
}

int lisp_get_character_(addr pos, unicode *ret)
{
	hold_value(pos, &pos);
	if (! characterp(pos)) {
		*ret = 0;
		return fmte_("The argument ~S must be a character type.", pos, NULL);
	}
	GetCharacter(pos, ret);
	return 0;
}

int lisp_get_float_(addr pos, float *ret)
{
	hold_value(pos, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_ss_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_ds_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ls_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			return Result(ret, single_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return single_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return single_float_ratio_(pos, ret);

		default:
			*ret = 0.0f;
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}
}

int lisp_get_double_(addr pos, double *ret)
{
	hold_value(pos, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sd_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dd_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ld_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			return Result(ret, double_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return double_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return double_float_ratio_(pos, ret);

		default:
			*ret = 0.0;
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}
}

int lisp_get_long_double_(addr pos, long double *ret)
{
	hold_value(pos, &pos);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return cast_sl_value_(pos, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return cast_dl_value_(pos, ret);

		case LISPTYPE_LONG_FLOAT:
			return cast_ll_value_(pos, ret);

		case LISPTYPE_FIXNUM:
			return Result(ret, long_float_fixnum(pos));

		case LISPTYPE_BIGNUM:
			return long_float_bignum_(pos, ret);

		case LISPTYPE_RATIO:
			return long_float_ratio_(pos, ret);

		default:
			*ret = 0.0L;
			return fmte_("The argument ~S must be a real type.", pos, NULL);
	}
}


/*
 *  function
 */
int lisp0_function_(addr *ret, addr value)
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

int lisp0_function8_(addr *ret, const void *str)
{
	addr value;
	Return(lisp0_intern8_(&value, NULL, str));
	return lisp0_function_(ret, value);
}

int lisp0_function16_(addr *ret, const void *str)
{
	addr value;
	Return(lisp0_intern16_(&value, NULL, str));
	return lisp0_function_(ret, value);
}

int lisp0_function32_(addr *ret, const void *str)
{
	addr value;
	Return(lisp0_intern32_(&value, NULL, str));
	return lisp0_function_(ret, value);
}

int lisp_function_(addr x, addr value)
{
	Return(lisp0_function_(&value, value));
	hold_set(x, value);
	return 0;
}

int lisp_function8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_function8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_function16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_function16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_function32_(const addr x, void *str)
{
	addr pos;

	Return(lisp0_function32_(&pos, str));
	hold_set(x, pos);
	return 0;
}


/*
 *  funcall
 */
int lisp0_funcall_(addr *ret, addr call, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_funcall8_(addr *ret, const void *str, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_funcall16_(addr *ret, const void *str, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_funcall32_(addr *ret, const void *str, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp_funcall_(addr x, addr call, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_funcall8_(addr x, const void *str, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_funcall16_(addr x, const void *str, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_funcall32_(addr x, const void *str, ...)
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
	lisp0_list_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}


/*
 *  apply
 */
int lisp0_apply_(addr *ret, addr call, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_apply8_(addr *ret, const void *str, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_apply16_(addr *ret, const void *str, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp0_apply32_(addr *ret, const void *str, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);
	if (ret)
		*ret = call;

	return 0;
}

int lisp_apply_(addr x, addr call, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	hold_value(call, &call);
	Return(lisp0_function_(&call, call));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_apply8_(addr x, const void *str, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function8_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_apply16_(addr x, const void *str, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function16_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

int lisp_apply32_(addr x, const void *str, ...)
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
	lisp0_lista_va_alloc(local, &args, va);
	va_end(va);

	Return(lisp0_function32_(&call, str));
	Return(callclang_apply(ptr, &call, call, args));
	rollback_local(local, stack);

	hold_set_null(x, call);
	return 0;
}

