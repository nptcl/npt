#include "bignum.h"
#include "call_filenames.h"
#include "character.h"
#include "condition.h"
#include "control_object.h"
#include "extern_develop.h"
#include "extern_error.h"
#include "extern_object.h"
#include "extern_type.h"
#include "float_object.h"
#include "local.h"
#include "hold.h"
#include "integer.h"
#include "number_equal.h"
#include "object.h"
#include "package.h"
#include "package_intern.h"
#include "paper.h"
#include "pathname.h"
#include "ratio.h"
#include "reader.h"
#include "real.h"
#include "symbol.h"
#include "typedef.h"
#include "unicode.h"

/*
 *  object
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

int lisp_character_(addr x, unicode value)
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

int lisp_float_(addr x, float value)
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

int lisp_zero_p(addr value)
{
	int check;

	hold_value(value, &value);
	if (zerop_numberp(value, &check))
		return 0;

	return check;
}

int lisp_plus_p(addr value)
{
	int check;

	hold_value(value, &value);
	if (plusp_realp(value, &check))
		return 0;

	return check;
}

int lisp_minus_p(addr value)
{
	int check;

	hold_value(value, &value);
	if (! realp(value))
		return 0;
	Error(minusp_real_(value, &check));
	return check;
}

void lisp_get_character(addr pos, unicode *ret)
{
	hold_value(pos, &pos);
	if (! characterp(pos)) {
		*ret = 0;
		Lisp_abort_type(pos, CHARACTER);
		return;
	}
	GetCharacter(pos, ret);
}

void lisp_get_fixnum(addr pos, fixnum *ret)
{
	hold_value(pos, &pos);
	if (! fixnump(pos)) {
		*ret = 0;
		Lisp_abort_type(pos, FIXNUM);
		return;
	}
	GetFixnum(pos, ret);
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
 *  package
 */
int lisp0_package_(addr *ret, addr pos)
{
	hold_value(pos, &pos);
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

int lisp_in_package_(addr value)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	value = holdv(value);
	if (! packagep(value))
		return fmte_("Value ~S must be a package object.", value, NULL);
	setspecial_local(Execute_Thread, symbol, value);

	return 0;
}

int lisp_in_package8_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(find_package_(x, &value));
	Return(lisp_in_package_(value));
	rollback_local(local, stack);

	return 0;
}

int lisp_in_package16_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(find_package_(x, &value));
	Return(lisp_in_package_(value));
	rollback_local(local, stack);

	return 0;
}

int lisp_in_package32_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(find_package_(x, &value));
	Return(lisp_in_package_(value));
	rollback_local(local, stack);

	return 0;
}

int lisp_push_and_in_package_(addr value)
{
	addr symbol;

	GetConst(SPECIAL_PACKAGE, &symbol);
	value = holdv(value);
	if (! packagep(value))
		return fmte_("Value ~S must be a package object.", value, NULL);
	pushspecial_control(Execute_Thread, symbol, value);

	return 0;
}

int lisp_push_and_in_package8_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(find_package_(x, &value));
	rollback_local(local, stack);
	Return(lisp_push_and_in_package_(value));

	return 0;
}

int lisp_push_and_in_package16_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(find_package_(x, &value));
	rollback_local(local, stack);
	Return(lisp_push_and_in_package_(value));

	return 0;
}

int lisp_push_and_in_package32_(const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x, value;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(find_package_(x, &value));
	rollback_local(local, stack);
	Return(lisp_push_and_in_package_(value));

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


/*
 *  reader
 */
int lisp0_reader_(addr *ret, addr str)
{
	int check;
	addr value;

	hold_value(str, &str);
	Return(read_from_string_(Execute_Thread, &check, &value, str));
	return Result(ret, check? NULL: value);
}

int lisp0_reader8_(addr *ret, const void *str)
{
	addr pos;
	Return(string8_null_heap_(&pos, (const char *)str));
	return lisp0_reader_(ret, pos);
}

int lisp0_reader16_(addr *ret, const void *str)
{
	addr pos;
	Return(string16_null_heap_(&pos, (const byte16 *)str));
	return lisp0_reader_(ret, pos);
}

int lisp0_reader32_(addr *ret, const void *str)
{
	addr pos;
	Return(string32_null_heap_(&pos, (const unicode *)str));
	return lisp0_reader_(ret, pos);
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
 *  pathname
 */
int lisp0_pathname_(addr *ret, addr name)
{
	hold_value(name, &name);
	return pathname_designer_heap_(Execute_Thread, name, ret);
}

int lisp0_pathname8_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string8_null_local_(local, &x, (const char *)str));
	Return(lisp0_pathname_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_pathname16_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string16_null_local_(local, &x, (const byte16 *)str));
	Return(lisp0_pathname_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_pathname32_(addr *ret, const void *str)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	Return(string32_null_local_(local, &x, (const unicode *)str));
	Return(lisp0_pathname_(ret, x));
	rollback_local(local, stack);

	return 0;
}

int lisp0_namestring_(addr *ret, addr path)
{
	hold_value(path, &path);
	return namestring_common_(Execute_Thread, ret, path);
}

int lisp_pathname_(addr x, addr name)
{
	Return(lisp0_pathname_(&name, name));
	hold_set(x, name);
	return 0;
}

int lisp_pathname8_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_pathname8_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_pathname16_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_pathname16_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_pathname32_(addr x, const void *str)
{
	addr pos;

	Return(lisp0_pathname32_(&pos, str));
	hold_set(x, pos);
	return 0;
}

int lisp_namestring_(addr x, addr path)
{
	Return(lisp0_namestring_(&path, path));
	hold_set(x, path);
	return 0;
}


/*
 *  paper
 */
int lisp0_paper_(addr *ret, size_t array, size_t body)
{
	return paper_arraybody_heap_(ret, array, body);
}

int lisp_paper_(addr x, size_t array, size_t body)
{
	addr pos;

	Return(lisp0_paper_(&pos, array, body));
	hold_set(x, pos);
	return 0;
}

int lisp_paper_gettype_(addr x, byte *ret)
{
	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_get_type(x, ret);

	return 0;
}

int lisp_paper_settype_(addr x, byte value)
{
	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_set_type(x, value);

	return 0;
}

int lisp_paper_lenarray_(addr x, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_len_array(x, ret);

	return 0;
}

int lisp_paper_lenbody_(addr x, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_len_body(x, ret);

	return 0;
}

int lisp0_paper_getarray_(addr *ret, addr pos, size_t index)
{
	size_t size;

	hold_value(pos, &pos);
	if (! paperp(pos)) {
		*ret = Nil;
		return fmte_("Not paper object, ~S.", pos, NULL);
	}
	paper_len_array(pos, &size);
	if (size <= index) {
		*ret = Nil;
		return fmte_("paper size error, ~S.", pos, NULL);
	}
	paper_get_array(pos, index, ret);

	return 0;
}

int lisp_paper_getarray_(addr x, addr pos, size_t index)
{
	addr value;

	Return(lisp0_paper_getarray_(&value, pos, index));
	hold_set(x, value);
	return 0;
}

int lisp_paper_setarray_(addr x, size_t index, addr value)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_len_array(x, &size);
	if (size <= index)
		return fmte_("paper size error, ~S.", x, NULL);
	hold_value(value, &value);
	paper_set_array(x, index, value);

	return 0;
}

int lisp_paper_getbody_(addr x, size_t index, byte *ret)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x)) {
		*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}
	paper_len_body(x, &size);
	if (size <= index) {
		*ret = 0;
		return fmte_("paper size error, ~S.", x, NULL);
	}
	paper_get_body(x, index, ret);

	return 0;
}

int lisp_paper_setbody_(addr x, size_t index, byte value)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_len_body(x, &size);
	if (size <= index)
		return fmte_("paper size error, ~S.", x, NULL);
	paper_set_body(x, index, value);

	return 0;
}

int lisp_paper_getmemory_(addr x, size_t a, size_t b, void *output, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_get_memory(x, a, b, output, ret);

	return 0;
}

int lisp_paper_setmemory_(addr x, size_t a, size_t b, const void *input, size_t *ret)
{
	hold_value(x, &x);
	if (! paperp(x))
		return fmte_("Not paper object, ~S.", x, NULL);
	paper_set_memory(x, a, b, input, ret);

	return 0;
}

int lisp_paper_body_unsafe_(addr x, byte **ptr, size_t *ret)
{
	size_t size;

	hold_value(x, &x);
	if (! paperp(x)) {
		if (ptr)
			*ptr = NULL;
		if (ret)
			*ret = 0;
		return fmte_("Not paper object, ~S.", x, NULL);
	}

	paper_len_body(x, &size);
	if (size == 0) {
		if (ptr)
			*ptr = NULL;
		if (ret)
			*ret = 0;
		return 0;
	}

	if (ptr) {
		posbody(x, &x);
		*ptr = (byte *)x;
	}
	if (ret)
		*ret = size;

	return 0;
}

