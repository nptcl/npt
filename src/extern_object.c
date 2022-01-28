#include "bignum.h"
#include "call_filenames.h"
#include "character.h"
#include "condition.h"
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
#include "pathname.h"
#include "ratio.h"
#include "reader.h"
#include "real.h"
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
 *  user
 */
#ifdef LISP_DEBUG
static void lisp0_user_alloc(LocalRoot local, addr *ret, size_t size)
{
	addr pos;
	size_t size1, alloc;
	byte *body;

	size1 = size + IdxSize;
	alloc = size + IdxSize + 8UL;
	if (size1 < size || alloc < size) {
		*ret = Nil;
		lisp_abortf("lisp0_user_heap error");
		return;
	}

	alloc_body(local, &pos, LISPSYSTEM_USER, alloc);
	SetUser(pos, 0);
	posbody(pos, (addr *)&body);
	*((size_t *)body) = size;
	memcpy(body + size1, "\xFF\x00\xAA\xAA\xAA\xAA\xAA\xAA", 8);
	*ret = pos;
}

static void lisp_object_check(addr pos)
{
	byte *body;
	size_t size;

	posbody(pos, (addr *)&body);
	size = *((size_t *)body) + 8UL;
	body += size;
	if (body[0] != 0xFF || body[1] != 0x00) {
		lisp_abortf("The user object may be destroyed.");
		return;
	}
}
#define Lisp_object_check(x) lisp_object_check(x)
#else
static void lisp0_user_alloc(LocalRoot local, addr *ret, size_t size)
{
	addr pos;
	size_t alloc;
	byte *body;

	alloc = size + IdxSize;
	if (alloc < size) {
		*ret = Nil;
		lisp_abortf("lisp0_user_heap error");
		return;
	}

	alloc_body(local, &pos, LISPSYSTEM_USER, alloc);
	SetUser(pos, 0);
	posbody(pos, (addr *)&body);
	*((size_t *)body) = size;
	*ret = pos;
}
#define Lisp_object_check(x)
#endif

void lisp0_user_heap(addr *ret, size_t size)
{
	lisp0_user_alloc(NULL, ret, size);
}

void lisp0_user_local(addr *ret, size_t size)
{
	lisp0_user_alloc(Local_Thread, ret, size);
}

static void lisp0_user_resize_alloc(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr value;
	byte *body1, *body2;
	size_t copy;

	/* check */
	hold_value(pos, &pos);
	if (! lisp_user_p(pos)) {
		*ret = Nil;
		lisp_abortf("The object is not user type.");
		return;
	}
	Lisp_object_check(pos);

	lisp0_user_alloc(local, &value, size);
	posbody(pos, (addr *)&body1);
	posbody(value, (addr *)&body2);
	copy = *((size_t *)body1);
	memcpy(body2 + IdxSize, body1 + IdxSize, (copy < size)? copy: size);
	SetUser(value, GetUser(pos));
	*ret = value;
}

void lisp0_user_resize_heap(addr *ret, addr pos, size_t size)
{
	lisp0_user_resize_alloc(NULL, ret, pos, size);
}

void lisp0_user_resize_local(addr *ret, addr pos, size_t size)
{
	lisp0_user_resize_alloc(Local_Thread, ret, pos, size);
}

void lisp_user_heap(addr x, size_t size)
{
	addr pos;
	lisp0_user_heap(&pos, size);
	hold_set(x, pos);
}

void lisp_user_local(addr x, size_t size)
{
	addr pos;
	lisp0_user_local(&pos, size);
	hold_set(x, pos);
}

void lisp_user_resize_heap(addr x, addr pos, size_t size)
{
	lisp0_user_resize_heap(&pos, pos, size);
	hold_set(x, pos);
}

void lisp_user_resize_local(addr x, addr pos, size_t size)
{
	lisp0_user_resize_local(&pos, pos, size);
	hold_set(x, pos);
}

int lisp_user_p(addr pos)
{
	if (pos == NULL || pos == Unbound)
		return 0;
	hold_value(pos, &pos);
	return GetType(pos) == LISPSYSTEM_USER;
}

void lisp_user_getsize(addr pos, size_t *ret)
{
	addr body;

	hold_value(pos, &pos);
	if (! lisp_user_p(pos)) {
		*ret = 0;
		lisp_abortf("The object is not user type.");
		return;
	}
	Lisp_object_check(pos);

	posbody(pos, &body);
	*ret = *((size_t *)body);
}

int lisp_user_getvalue(addr pos)
{
	hold_value(pos, &pos);
	if (! lisp_user_p(pos)) {
		lisp_abortf("The object is not user type.");
		return 0;
	}
	Lisp_object_check(pos);

	return GetUser(pos);
}

void lisp_user_setvalue(addr pos, byte c)
{
	hold_value(pos, &pos);
	if (! lisp_user_p(pos)) {
		lisp_abortf("The object is not user type.");
		return;
	}

	Lisp_object_check(pos);
	SetUser(pos, c);
}

byte *lisp_object_body(addr pos)
{
	byte *body;

	hold_value(pos, &pos);
	Lisp_object_check(pos);
	posbody(pos, (addr *)&body);

	return body + IdxSize;
}

