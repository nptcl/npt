#include "bignum.h"
#include "character.h"
#include "cmpl.h"
#include "condition.h"
#include "copy.h"
#include "function.h"
#include "integer.h"
#include "memory.h"
#include "object.h"
#include "pathname.h"
#include "random_state.h"
#include "ratio.h"
#include "real.h"
#include "strvect.h"
#include "type_copy.h"

typedef void (*copyhard_calltype)(LocalRoot local, addr *ret, addr pos);
typedef int (*checklocal_calltype)(LocalRoot local, addr pos);
typedef void (*copylocal_calltype)(LocalRoot local, addr *ret, addr pos);
static copyhard_calltype TableCopy[LISPTYPE_SIZE];
static checklocal_calltype TableCheckSoft[LISPTYPE_SIZE];
static copylocal_calltype TableCopySoft[LISPTYPE_SIZE];


/*
 *  copy
 */
static void copyhard_error(LocalRoot local, addr *ret, addr pos)
{
	_fmte("copy error", NULL);
}

static void copyhard_moveonly(LocalRoot local, addr *ret, addr pos)
{
	*ret = pos;
}

static void copyhard_type(LocalRoot local, addr *ret, addr pos)
{
	type_copy_alloc(local, ret, pos);
}

_g void copyhard_cons(LocalRoot local, addr *ret, addr pos)
{
	addr left, right;

	CheckType(pos, LISPTYPE_CONS);
	GetCons(pos, &left, &right);
	copyhard_object(local, &left, left);
	copyhard_object(local, &right, right);
	cons_alloc(local, ret, left, right);
}

_g void copyhard_vectorA2(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	CheckType(left, LISPTYPE_VECTOR);
	Check(GetStatusSize(left) != LISPSIZE_ARRAY2, "size error");
	LenArrayA2(left, &size);
	vector2_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA2(left, i, &pos);
		copyhard_object(local, &pos, pos);
		SetArrayA2(right, i, pos);
	}
	*ret = right;
}

_g void copyhard_vectorA4(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	CheckType(left, LISPTYPE_VECTOR);
	Check(GetStatusSize(left) != LISPSIZE_ARRAY4, "size error");
	LenArrayA4(left, &size);
	vector4_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &pos);
		copyhard_object(local, &pos, pos);
		SetArrayA4(right, i, pos);
	}
	*ret = right;
}

#ifdef LISP_ARCH_64BIT
_g void copyhard_vectorA8(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	CheckType(left, LISPTYPE_VECTOR);
	Check(GetStatusSize(left) != LISPSIZE_ARRAY8, "size error");
	LenArrayA8(left, &size);
	vector8_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA8(left, i, &pos);
		copyhard_object(local, &pos, pos);
		SetArrayA8(right, i, pos);
	}
	*ret = right;
}
#endif

_g void copyhard_vector(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_VECTOR);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			copyhard_vectorA2(local, ret, pos);
			break;

		case LISPSIZE_ARRAY4:
			copyhard_vectorA4(local, ret, pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			copyhard_vectorA8(local, ret, pos);
			break;
#endif

		default:
			_fmte("size error", NULL);
			break;
	}
}

_g void copyhard_character(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_CHARACTER);
	character_alloc(local, ret, RefCharacter(pos));
}

_g void copyhard_string(LocalRoot local, addr *ret, addr pos)
{
	const unicode *body;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	strvect_posbodylen(pos, &body, &size);
	strvect_sizeu_alloc(local, ret, body, size);
}

_g void copyhard_fixnum(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_FIXNUM);
	fixnum_alloc(local, ret, RefFixnum(pos));
}

_g void copyhard_bignum(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_BIGNUM);
	bignum_copy_alloc(local, ret, pos);
}

_g void copyhard_ratio(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_RATIO);
	ratio_copy_alloc(local, ret, pos);
}

_g void copyhard_float(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	single_float_alloc(local, ret, RefSingleFloat(pos));
}

_g void copyhard_double(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	double_float_alloc(local, ret, RefDoubleFloat(pos));
}

_g void copyhard_long_double(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_LONG_FLOAT);
	long_float_alloc(local, ret, RefLongFloat(pos));
}

_g void copyhard_complex(LocalRoot local, addr *ret, addr pos)
{
	enum ComplexType type;
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	type = GetTypeComplex(pos);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	copyhard_object(local, &real, real);
	copyhard_object(local, &imag, imag);

	make_complex_unsafe(local, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	*ret = pos;
}

_g void copyhard_callname(LocalRoot local, addr *ret, addr pos)
{
	enum CALLNAME_TYPE type;
	addr name;

	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallName(pos, &name);
	copyhard_object(local, &name, name);
	GetCallNameType(pos, &type);
	callname_alloc(local, ret, name, type);
}

_g void copyhard_random_state(LocalRoot local, addr *ret, addr pos)
{
	addr one;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	random_state_alloc(local, &one);
	copy_random_state(one, pos);
	*ret = one;
}

_g void copyhard_pathname(LocalRoot local, addr *ret, addr pos)
{
	int i;
	addr one, child;

	Check(! pathnamep(pos), "type error");
	make_pathname_alloc(local, &one, pathname_logical_p(pos));
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetPathname(pos, i, &child);
		copyhard_object(local, &child, child);
		SetPathname(pos, i, child);
	}
	*ret = one;
}

_g void copyhard_object(LocalRoot local, addr *ret, addr pos)
{
	int index;

	Check(pos == Unbound, "unbound error");
	index = (int)GetType(pos);
	Check(LISPTYPE_SIZE <= index, "index error");
	(TableCopy[index])(local, ret, pos);
}

static void init_copyhard_call(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		TableCopy[i] = copyhard_error;

	TableCopy[LISPTYPE_NIL] = copyhard_moveonly;
	TableCopy[LISPTYPE_T] = copyhard_moveonly;
	TableCopy[LISPTYPE_TYPE] = copyhard_type;
	TableCopy[LISPTYPE_CLOS] = copyhard_error;
	TableCopy[LISPTYPE_CONS] = copyhard_cons;
	TableCopy[LISPTYPE_ARRAY] = copyhard_error;
	TableCopy[LISPTYPE_VECTOR] = copyhard_vector;
	TableCopy[LISPTYPE_CHARACTER] = copyhard_character;
	TableCopy[LISPTYPE_STRING] = copyhard_string;
	TableCopy[LISPTYPE_HASHTABLE] = copyhard_error;
	TableCopy[LISPTYPE_READTABLE] = copyhard_error;
	TableCopy[LISPTYPE_SYMBOL] = copyhard_moveonly;
	TableCopy[LISPTYPE_FIXNUM] = copyhard_fixnum;
	TableCopy[LISPTYPE_BIGNUM] = copyhard_bignum;
	TableCopy[LISPTYPE_RATIO] = copyhard_ratio;
	TableCopy[LISPTYPE_SHORT_FLOAT] = copyhard_error;
	TableCopy[LISPTYPE_SINGLE_FLOAT] = copyhard_float;
	TableCopy[LISPTYPE_DOUBLE_FLOAT] = copyhard_double;
	TableCopy[LISPTYPE_LONG_FLOAT] = copyhard_long_double;
	TableCopy[LISPTYPE_COMPLEX] = copyhard_complex;
	TableCopy[LISPTYPE_CONTROL] = copyhard_error;
	TableCopy[LISPTYPE_CODE] = copyhard_error;
	TableCopy[LISPTYPE_CALLNAME] = copyhard_callname;
	TableCopy[LISPTYPE_FUNCTION] = copyhard_error;
	TableCopy[LISPTYPE_INDEX] = copyhard_error;
	TableCopy[LISPTYPE_PACKAGE] = copyhard_error;
	TableCopy[LISPTYPE_RANDOM_STATE] = copyhard_random_state;
	TableCopy[LISPTYPE_PATHNAME] = copyhard_pathname;
	TableCopy[LISPTYPE_STREAM] = copyhard_error;
	TableCopy[LISPTYPE_QUOTE] = copyhard_error;
	TableCopy[LISPTYPE_RESTART] = copyhard_error;
	TableCopy[LISPTYPE_EVAL] = copyhard_error;
}


/*
 *  checklocal
 */
_g int copylocalp(LocalRoot local, addr pos)
{
	return local == NULL && GetStatusDynamic(pos);
}

static int checklocal_error(LocalRoot local, addr pos)
{
	if (copylocalp(local, pos))
		_fmte("checklocal-copyerror", NULL);
	return 0;
}

static int checklocal_false(LocalRoot local, addr pos)
{
	return 0;
}

static int checklocal_default(LocalRoot local, addr pos)
{
	return copylocalp(local, pos);
}

static int checklocal_cons(LocalRoot local, addr right)
{
	addr left;
	if (copylocalp(local, right)) return 1;
	GetCons(right, &left, &right);
	return copylocalp(local, left) || copylocalp(local, right);
}

static int checklocal_vector(LocalRoot local, addr pos)
{
	addr child;
	size_t size, i;

	if (copylocalp(local, pos)) return 1;
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &child);
		if (copylocal_check(local, child)) return 1;
	}

	return 0;
}

static int checklocal_complex(LocalRoot local, addr right)
{
	addr real;
	if (copylocalp(local, right)) return 1;
	GetRealComplex(right, &real);
	GetImagComplex(right, &right);
	return copylocalp(local, real) || copylocalp(local, right);
}

static int checklocal_callname(LocalRoot local, addr pos)
{
	if (copylocalp(local, pos)) return 1;
	GetCallName(pos, &pos);
	return copylocalp(local, pos);
}

static int checklocal_function(LocalRoot local, addr pos)
{
	addr check;

	if (copylocalp(local, pos)) return 1;
	GetFunction(pos, &check);
	if (copylocalp(local, check)) return 1;
	GetNameFunction(pos, &check);
	if (copylocalp(local, check)) return 1;
	GetDataFunction(pos, &check);
	if (copylocalp(local, check)) return 1;
	GetTableFunction_Low(pos, &check);
	return copylocalp(local, check);
}

static int checklocal_pathname(LocalRoot local, addr pos)
{
	addr child;
	size_t i;

	if (copylocalp(local, pos)) return 1;
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetPathname(pos, i, &child);
		if (copylocalp(local, child)) return 1;
	}

	return 0;
}

_g int copylocal_check(LocalRoot local, addr pos)
{
	int index;

	Check(pos == Unbound, "unbound error");
	index = (int)GetType(pos);
	Check(LISPTYPE_SIZE <= index, "index error");
	return (TableCheckSoft[index])(local, pos);
}

static void init_checklocal_call(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		TableCheckSoft[i] = checklocal_error;

	TableCheckSoft[LISPTYPE_NIL] = checklocal_false;
	TableCheckSoft[LISPTYPE_T] = checklocal_false;
	TableCheckSoft[LISPTYPE_TYPE] = checklocal_default;
	TableCheckSoft[LISPTYPE_CLOS] = checklocal_error;
	TableCheckSoft[LISPTYPE_CONS] = checklocal_cons;
	TableCheckSoft[LISPTYPE_ARRAY] = checklocal_error;
	TableCheckSoft[LISPTYPE_VECTOR] = checklocal_vector;
	TableCheckSoft[LISPTYPE_CHARACTER] = checklocal_default;
	TableCheckSoft[LISPTYPE_STRING] = checklocal_default;
	TableCheckSoft[LISPTYPE_HASHTABLE] = checklocal_error;
	TableCheckSoft[LISPTYPE_READTABLE] = checklocal_error;
	TableCheckSoft[LISPTYPE_SYMBOL] = checklocal_false;
	TableCheckSoft[LISPTYPE_FIXNUM] = checklocal_default;
	TableCheckSoft[LISPTYPE_BIGNUM] = checklocal_default;
	TableCheckSoft[LISPTYPE_RATIO] = checklocal_default;
	TableCheckSoft[LISPTYPE_SHORT_FLOAT] = checklocal_error;
	TableCheckSoft[LISPTYPE_SINGLE_FLOAT] = checklocal_default;
	TableCheckSoft[LISPTYPE_DOUBLE_FLOAT] = checklocal_default;
	TableCheckSoft[LISPTYPE_LONG_FLOAT] = checklocal_default;
	TableCheckSoft[LISPTYPE_COMPLEX] = checklocal_complex;
	TableCheckSoft[LISPTYPE_CONTROL] = checklocal_error;
	TableCheckSoft[LISPTYPE_CODE] = checklocal_error;
	TableCheckSoft[LISPTYPE_CALLNAME] = checklocal_callname;
	TableCheckSoft[LISPTYPE_FUNCTION] = checklocal_function;
	TableCheckSoft[LISPTYPE_INDEX] = checklocal_error;
	TableCheckSoft[LISPTYPE_PACKAGE] = checklocal_error;
	TableCheckSoft[LISPTYPE_RANDOM_STATE] = checklocal_default;
	TableCheckSoft[LISPTYPE_PATHNAME] = checklocal_pathname;
	TableCheckSoft[LISPTYPE_STREAM] = checklocal_error;
	TableCheckSoft[LISPTYPE_QUOTE] = checklocal_error;
	TableCheckSoft[LISPTYPE_RESTART] = checklocal_error;
	TableCheckSoft[LISPTYPE_EVAL] = checklocal_error;
}


/*
 *  copylocal
 */
static void copylocal_error(LocalRoot local, addr *ret, addr pos)
{
	_fmte("copylocal error", NULL);
}

static void copylocal_type(LocalRoot local, addr *ret, addr pos)
{
	type_copy_alloc(local, ret, pos);
}

static void copylocal_cons(LocalRoot local, addr *ret, addr right)
{
	addr left;

	GetCons(right, &left, &right);
	copylocal_object(local, &left, left);
	copylocal_object(local, &right, right);
	cons_alloc(local, ret, left, right);
}

static void copylocal_vectorA2(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	LenArrayA2(left, &size);
	vector2_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA2(left, i, &pos);
		copylocal_object(local, &pos, pos);
		SetArrayA2(right, i, pos);
	}
	*ret = right;
}

static void copylocal_vectorA4(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	LenArrayA4(left, &size);
	vector4_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA4(left, i, &pos);
		copylocal_object(local, &pos, pos);
		SetArrayA4(right, i, pos);
	}
	*ret = right;
}

#ifdef LISP_ARCH_64BIT
static void copylocal_vectorA8(LocalRoot local, addr *ret, addr left)
{
	addr right, pos;
	size_t size, i;

	LenArrayA8(left, &size);
	vector8_alloc(local, &right, size);
	for (i = 0; i < size; i++) {
		GetArrayA8(left, i, &pos);
		copylocal_object(local, &pos, pos);
		SetArrayA8(right, i, pos);
	}
	*ret = right;
}
#endif

static void copylocal_vector(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPTYPE_VECTOR);
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			copylocal_vectorA2(local, ret, pos);
			break;

		case LISPSIZE_ARRAY4:
			copylocal_vectorA4(local, ret, pos);
			break;

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			copylocal_vectorA8(local, ret, pos);
			break;
#endif

		default:
			_fmte("size error", NULL);
			break;
	}
}

static void copylocal_character(LocalRoot local, addr *ret, addr pos)
{
	character_alloc(local, ret, RefCharacter(pos));
}

static void copylocal_fixnum(LocalRoot local, addr *ret, addr pos)
{
	fixnum_alloc(local, ret, RefFixnum(pos));
}

static void copylocal_bignum(LocalRoot local, addr *ret, addr pos)
{
	bignum_copy_alloc(local, ret, pos);
}

static void copylocal_ratio(LocalRoot local, addr *ret, addr pos)
{
	ratio_copy_alloc(local, ret, pos);
}

static void copylocal_single_float(LocalRoot local, addr *ret, addr pos)
{
	single_float_alloc(local, ret, RefSingleFloat(pos));
}

static void copylocal_double_float(LocalRoot local, addr *ret, addr pos)
{
	double_float_alloc(local, ret, RefDoubleFloat(pos));
}

static void copylocal_long_float(LocalRoot local, addr *ret, addr pos)
{
	long_float_alloc(local, ret, RefLongFloat(pos));
}

static void copylocal_complex(LocalRoot local, addr *ret, addr pos)
{
	enum ComplexType type;
	addr real, imag;

	CheckType(pos, LISPTYPE_COMPLEX);
	type = GetTypeComplex(pos);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	if (copylocalp(local, real))
		copyhard_object(local, &real, real);
	if (copylocalp(local, imag))
		copyhard_object(local, &imag, imag);

	make_complex_unsafe(local, &pos, type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	*ret = pos;
}

static void copylocal_callname(LocalRoot local, addr *ret, addr pos)
{
	enum CALLNAME_TYPE type;
	addr name, one;

	GetCallName(pos, &name);
	GetCallNameType(pos, &type);
	if (copylocalp(local, name))
		copyhard_object(local, &name, name);
	make_callname_alloc(local, &one);
	SetCallName(one, name);
	SetCallNameType(one, type);
	*ret = one;
}

static void copylocal_pathname(LocalRoot local, addr *ret, addr pos)
{
	addr one, child;
	size_t i;

	make_pathname_alloc(local, &one, pathname_logical_p(pos));
	for (i = 0; i < PATHNAME_INDEX_SIZE; i++) {
		GetPathname(pos, i, &child);
		if (copylocalp(local, child))
			copyhard_object(local, &child, child);
		SetPathname(one, i, child);
	}
	*ret = one;
}

_g int copylocal_object(LocalRoot local, addr *ret, addr pos)
{
	int index;

	Check(pos == Unbound, "unbound error");
	if (copylocal_check(local, pos)) {
		index = (int)GetType(pos);
		Check(LISPTYPE_SIZE <= index, "index error");
		(TableCopySoft[index])(local, ret, pos);
		return 1;
	}
	else {
		*ret = pos;
		return 0;
	}
}

_g void copylocal_list_stdarg(LocalRoot local, addr *ret, va_list args)
{
	addr left, right, next;

	left = va_arg(args, addr);
	if (left == NULL) {
		*ret = Nil;
		return;
	}
	copylocal_object(local, &left, left);
	conscar_alloc(local, &right, left);
	*ret = right;

	for (;;) {
		left = va_arg(args, addr);
		if (left == NULL) break;
		copylocal_object(local, &left, left);
		conscar_alloc(local, &next, left);
		SetCdr(right, next);
		right = next;
	}
}

_g int copyheap(addr *ret, addr pos)
{
	return copylocal_object(NULL, ret, pos);
}

_g addr copyheapr(addr pos)
{
	copyheap(&pos, pos);
	return pos;
}

static void init_copylocal_call(void)
{
	int i;

	for (i = 0; i < LISPTYPE_SIZE; i++)
		TableCopySoft[i] = copylocal_error;

	TableCopySoft[LISPTYPE_NIL] = copylocal_error;
	TableCopySoft[LISPTYPE_T] = copylocal_error;
	TableCopySoft[LISPTYPE_TYPE] = copylocal_type;
	TableCopySoft[LISPTYPE_CLOS] = copylocal_error;
	TableCopySoft[LISPTYPE_CONS] = copylocal_cons;
	TableCopySoft[LISPTYPE_ARRAY] = copylocal_error;
	TableCopySoft[LISPTYPE_VECTOR] = copylocal_vector;
	TableCopySoft[LISPTYPE_CHARACTER] = copylocal_character;
	TableCopySoft[LISPTYPE_STRING] = copylocal_error;
	TableCopySoft[LISPTYPE_HASHTABLE] = copylocal_error;
	TableCopySoft[LISPTYPE_READTABLE] = copylocal_error;
	TableCopySoft[LISPTYPE_SYMBOL] = copylocal_error;
	TableCopySoft[LISPTYPE_FIXNUM] = copylocal_fixnum;
	TableCopySoft[LISPTYPE_BIGNUM] = copylocal_bignum;
	TableCopySoft[LISPTYPE_RATIO] = copylocal_ratio;
	TableCopySoft[LISPTYPE_SHORT_FLOAT] = copylocal_error;
	TableCopySoft[LISPTYPE_SINGLE_FLOAT] = copylocal_single_float;
	TableCopySoft[LISPTYPE_DOUBLE_FLOAT] = copylocal_double_float;
	TableCopySoft[LISPTYPE_LONG_FLOAT] = copylocal_long_float;
	TableCopySoft[LISPTYPE_COMPLEX] = copylocal_complex;
	TableCopySoft[LISPTYPE_CONTROL] = copylocal_error;
	TableCopySoft[LISPTYPE_CODE] = copylocal_error;
	TableCopySoft[LISPTYPE_CALLNAME] = copylocal_callname;
	TableCopySoft[LISPTYPE_FUNCTION] = copylocal_error;
	TableCopySoft[LISPTYPE_INDEX] = copylocal_error;
	TableCopySoft[LISPTYPE_PACKAGE] = copylocal_error;
	TableCopySoft[LISPTYPE_RANDOM_STATE] = copylocal_error;
	TableCopySoft[LISPTYPE_PATHNAME] = copylocal_pathname;
	TableCopySoft[LISPTYPE_STREAM] = copylocal_error;
	TableCopySoft[LISPTYPE_QUOTE] = copylocal_error;
	TableCopySoft[LISPTYPE_RESTART] = copylocal_error;
	TableCopySoft[LISPTYPE_EVAL] = copylocal_error;
}


/*
 *  build
 */
_g void init_copy(void)
{
	init_copyhard_call();
	init_checklocal_call();
	init_copylocal_call();
}

