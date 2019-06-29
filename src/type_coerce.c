#include "array.h"
#include "array_coerce.h"
#include "array_common.h"
#include "array_object.h"
#include "bigdata.h"
#include "bignum.h"
#include "bit.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "copy.h"
#include "execute.h"
#include "integer.h"
#include "local.h"
#include "ratio.h"
#include "real.h"
#include "real_float.h"
#include "strtype.h"
#include "symbol.h"
#include "type.h"
#include "type_object.h"
#include "type_optimize.h"
#include "type_parse.h"
#include "type_typep.h"
#include "type_value.h"

static int coerce_type(addr pos, addr type, addr *ret);

/*
 *  type
 */
static void coerce_error(addr pos, addr type)
{
	copyheap(&pos, pos);
	type_object(&type, type);
	type_error_stdarg(pos, type,
			"Cannot covert value ~A to a ~S type.", pos, type, NULL);
}

static int coerce_typep(addr pos, addr value, addr type, addr *ret)
{
	int check;

	if (typep_clang(value, type, &check))
		return 1;
	if (! check)
		coerce_error(pos, type);
	*ret = value;
	return 0;
}


/*
 *  float
 */
static int coerce_fixnum_single(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_fixnum_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_bignum_single(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	single_float_bignum_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_ratio_single(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	single_float_ratio_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_float(addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return coerce_typep(pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single(pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single(pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}


/*
 *  single-float
 */
static int coerce_double_single(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	single_float_heap(&value, cast_ds_value(pos));
	return coerce_typep(pos, value, type, ret);
}

static int coerce_long_single(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	single_float_heap(&value, cast_ls_value(pos));
	return coerce_typep(pos, value, type, ret);
}

static int coerce_single(addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_typep(pos, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_single(pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_single(pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single(pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single(pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}


/*
 *  double-float
 */
static int coerce_single_double(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	double_float_heap(&value, cast_sd_value(pos));
	return coerce_typep(pos, value, type, ret);
}

static int coerce_long_double(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	double_float_heap(&value, cast_ld_value(pos));
	return coerce_typep(pos, value, type, ret);
}

static int coerce_fixnum_double(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_fixnum_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_bignum_double(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	double_float_bignum_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_ratio_double(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	double_float_ratio_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_double(addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_double(pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_typep(pos, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_double(pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_double(pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_double(pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_double(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}


/*
 *  long-float
 */
static int coerce_single_long(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	long_float_heap(&value, cast_sl_value(pos));
	return coerce_typep(pos, value, type, ret);
}

static int coerce_double_long(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	long_float_heap(&value, cast_dl_value(pos));
	return coerce_typep(pos, value, type, ret);
}

static int coerce_fixnum_long(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_fixnum_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_bignum_long(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	long_float_bignum_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_ratio_long(addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	long_float_ratio_heap(&value, pos);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_long(addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_long(pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_long(pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_typep(pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_long(pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_long(pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_long(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}


/*
 *  complex
 */
static int coerce_complex_complex(addr pos, addr type, addr *ret)
{
	addr real, imag;

	GetArrayType(type, 0, &type);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);
	if (coerce_type(real, type, &real)) return 1;
	if (coerce_type(imag, type, &imag)) return 1;
	complex_heap(ret, real, imag);

	return 0;
}

static int coerce_complex_real(addr pos, addr type, addr *ret)
{
	GetArrayType(type, 0, &type);
	if (! type_asterisk_p(type)) {
		if (coerce_type(pos, type, &pos))
			return 1;
	}
	complex_heap(ret, pos, fixnumh(0));

	return 0;
}

static int coerce_complex(addr pos, addr type, addr *ret)
{
	if (complexp(pos))
		return coerce_complex_complex(pos, type, ret);
	if (realp(pos))
		return coerce_complex_real(pos, type, ret);
	else
		return coerce_typep(pos, pos, type, ret);
}


/*
 *  charcter
 */
static int coerce_unicode_character(addr pos, unicode c, addr type, addr *ret)
{
	addr value;
	character_heap(&value, c);
	return coerce_typep(pos, value, type, ret);
}

static int coerce_string_character(addr pos, addr type, addr *ret)
{
	unicode c;
	size_t size;

	string_length(pos, &size);
	if (size != 1)
		coerce_error(pos, type);
	string_getc(pos, 0, &c);
	return coerce_unicode_character(pos, c, type, ret);
}

static int coerce_symbol_character(addr pos, addr type, addr *ret)
{
	addr name;
	unicode c;
	size_t size;

	GetNameSymbol(pos, &name);
	string_length(name, &size);
	if (size != 1)
		coerce_error(pos, type);
	string_getc(name, 0, &c);
	return coerce_unicode_character(pos, c, type, ret);
}

static int coerce_character(addr pos, addr type, addr *ret)
{
	/* (or symbol string character) */
	if (stringp(pos))
		return coerce_string_character(pos, type, ret);
	else if (symbolp(pos))
		return coerce_symbol_character(pos, type, ret);
	else
		return coerce_typep(pos, pos, type, ret);
}


/*
 *  function
 */
static int coerce_function(addr pos, addr type, addr *ret)
{
	addr call;

	if (symbolp(pos)) {
		getfunctioncheck_local(Execute_Thread, pos, &call);
		return coerce_typep(pos, call, type, ret);
	}
	else {
		return coerce_typep(pos, pos, type, ret);
	}
}


/*
 *  list
 */
static int coerce_vector_list(addr pos, addr type, addr *ret)
{
	addr list, x;
	size_t size, i;

	lenarray(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		getarray(pos, i, &x);
		cons_heap(&list, x, list);
	}
	nreverse_list_unsafe(&list, list);

	return coerce_typep(pos, list, type, ret);
}

static int coerce_string_list(addr pos, addr type, addr *ret)
{
	unicode c;
	addr list, x;
	size_t size, i;

	strvect_length(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		character_heap(&x, c);
		cons_heap(&list, x, list);
	}
	nreverse_list_unsafe(&list, list);

	return coerce_typep(pos, list, type, ret);
}

static int coerce_array_list(addr pos, addr type, addr *ret)
{
	addr list, x;
	size_t size, i;

	/* not vector */
	if (! array_vector_p(pos))
		return coerce_typep(pos, type, type, ret);

	/* cast list */
	list = Nil;
	size = array_vector_length(pos, 1);
	for (i = 0; i < size; i++) {
		array_get(NULL, pos, i, &x);
		cons_heap(&list, x, list);
	}
	nreverse_list_unsafe(&list, list);

	return coerce_typep(pos, list, type, ret);
}

static int coerce_bitvector_list(addr pos, addr type, addr *ret)
{
	int v;
	addr list, x;
	size_t size, i;

	bitvector_length(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		bitvector_getint(pos, i, &v);
		fixnum_heap(&x, (fixnum)v);
		cons_heap(&list, x, list);
	}
	nreverse_list_unsafe(&list, list);

	return coerce_typep(pos, list, type, ret);
}

static int coerce_list(addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_VECTOR:
			return coerce_vector_list(pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_string_list(pos, type, ret);

		case LISPTYPE_ARRAY:
			return coerce_array_list(pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_bitvector_list(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}


/*
 *  array
 */
/* array.bit -> array.t */
static int coerce_aa_bit_t(addr pos, addr type, addr *ret)
{
	int bit;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_bit(pos, i, &bit)) {
			coerce_error(pos, type);
			return 0;
		}
		fixnum_heap(&value, (fixnum)bit);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.character -> array.t */
static int coerce_aa_character_t(addr pos, addr type, addr *ret)
{
	unicode c;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_character(pos, i, &c)) {
			coerce_error(pos, type);
			return 0;
		}
		character_heap(&value, c);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.signed8 -> array.t */
static int coerce_aa_signed8_t(addr pos, addr type, addr *ret)
{
	int8_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed8(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		fixnum_heap(&value, (fixnum)v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.signed16 -> array.t */
static int coerce_aa_signed16_t(addr pos, addr type, addr *ret)
{
	int16_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed16(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		fixnum_heap(&value, (fixnum)v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.signed32 -> array.t */
static int coerce_aa_signed32_t(addr pos, addr type, addr *ret)
{
	int32_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed32(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		fixnum_heap(&value, (fixnum)v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.signed64 -> array.t */
static int coerce_aa_signed64_t(addr pos, addr type, addr *ret)
{
	int64_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed64(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		fixnum_heap(&value, (fixnum)v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* array.signed -> array.t */
static int coerce_aa_signed_t(addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8_t(pos, type, ret);

		case 16:
			return coerce_aa_signed16_t(pos, type, ret);

		case 32:
			return coerce_aa_signed32_t(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64_t(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
}

/* array.unsigned8 -> array.t */
static int coerce_aa_unsigned8_t(addr pos, addr type, addr *ret)
{
	uint8_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned8(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		fixnum_heap(&value, (fixnum)v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.unsigned16 -> array.t */
static int coerce_aa_unsigned16_t(addr pos, addr type, addr *ret)
{
	uint16_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned16(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		fixnum_heap(&value, (fixnum)v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.unsigned32 -> array.t */
static int coerce_aa_unsigned32_t(addr pos, addr type, addr *ret)
{
	uint32_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned32(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
#ifdef LISP_64BIT
		fixnum_heap(&value, (fixnum)v);
#else
		integer_fixed_heap(&value, SignPlus, (fixed)v);
#endif
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.unsigned64 -> array.t */
static int coerce_aa_unsigned64_t(addr pos, addr type, addr *ret)
{
	uint64_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned64(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		integer_fixed_heap(&value, SignPlus, (fixed)v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* array.unsigned -> array.t */
static int coerce_aa_unsigned_t(addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8_t(pos, type, ret);

		case 16:
			return coerce_aa_unsigned16_t(pos, type, ret);

		case 32:
			return coerce_aa_unsigned32_t(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64_t(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
}

/* array.single-float -> array.t */
static int coerce_aa_single_t(addr pos, addr type, addr *ret)
{
	single_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_single(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		single_float_heap(&value, v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.double-float -> array.t */
static int coerce_aa_double_t(addr pos, addr type, addr *ret)
{
	double_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_double(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		double_float_heap(&value, v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.long-float -> array.t */
static int coerce_aa_long_t(addr pos, addr type, addr *ret)
{
	long_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	array_coerce_t_heap(&array, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (array_coerce_long(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		long_float_heap(&value, v);
		array_set(array, i, value);
	}

	return coerce_typep(pos, array, type, ret);
}

/* array -> array.t */
static int coerce_aa_t(addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_typep(pos, pos, type, ret);

		case ARRAY_TYPE_BIT:
			return coerce_aa_bit_t(pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
			return coerce_aa_character_t(pos, type, ret);

		case ARRAY_TYPE_SIGNED:
			return coerce_aa_signed_t(pos, type, str->bytesize, ret);

		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_unsigned_t(pos, type, str->bytesize, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return coerce_aa_single_t(pos, type, ret);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return coerce_aa_double_t(pos, type, ret);

		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_long_t(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array.t -> bitvector */
static int coerce_aa_bitvector(addr pos, addr type, addr *ret)
{
	int v;
	addr vector;
	size_t size, i;

	array_rowlength(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		if (array_coerce_bit(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		bitmemory_setint(vector, i, v);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* array.* -> array.bit */
static int coerce_aa_type_bit(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_bitvector(pos, type, ret);

	/* array.bit */
	array_rowlength(pos, &size);
	array_coerce_bit_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_bit(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_bit(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array -> array.bit */
static int coerce_aa_bit(addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_bit(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array.t -> string */
static int coerce_aa_string(addr pos, addr type, addr *ret)
{
	unicode v;
	addr vector;
	size_t size, i;

	array_rowlength(pos, &size);
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		if (array_coerce_character(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		strvect_setc(vector, i, v);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* array.t -> array.character */
static int coerce_aa_t_character(addr pos, addr type, addr *ret)
{
	unicode v;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_string(pos, type, ret);

	/* array.bit */
	array_rowlength(pos, &size);
	array_coerce_character_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_character(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_character(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array -> array.character */
static int coerce_aa_character(addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_aa_t_character(pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array.* -> array.signed8 */
static int coerce_aa_signed8(addr pos, addr type, addr *ret)
{
	int8_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_signed8_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed8(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed8(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.* -> array.signed16 */
static int coerce_aa_signed16(addr pos, addr type, addr *ret)
{
	int16_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_signed16_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed16(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed16(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.* -> array.signed32 */
static int coerce_aa_signed32(addr pos, addr type, addr *ret)
{
	int32_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_signed32_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed32(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed32(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.signed64 */
static int coerce_aa_signed64(addr pos, addr type, addr *ret)
{
	int64_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_signed64_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_signed64(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed64(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* array.* -> array.signed */
static int coerce_aa_type_signed(addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8(pos, type, ret);

		case 16:
			return coerce_aa_signed16(pos, type, ret);

		case 32:
			return coerce_aa_signed32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
}

/* array -> array.signed */
static int type_second_size(addr type, unsigned *ret)
{
	enum LISPDECL decl;
	addr check;
	size_t size;

	/* type check */
	decl = LispDecl(type);
	if (decl != LISPDECL_ARRAY && decl != LISPDECL_SIMPLE_ARRAY)
		return 1;

	/* upgraded type check */
	GetArrayType(type, 0, &check);
	decl = LispDecl(check);
	if (decl != LISPDECL_SIGNED_BYTE && decl != LISPDECL_UNSIGNED_BYTE)
		return 1;

	/* (array (signed-byte size) *) */
	GetArrayType(check, 0, &check);
	if (getindex_integer(check, &size))
		return 1;

	switch (size) {
		case 8:
		case 16:
		case 32:
#ifdef LISP_64BIT
		case 64:
#endif
			*ret = (unsigned)size;
			return 0;

		default:
			return 1;
	}
}

static int coerce_aa_signed(addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_signed(pos, type, size, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array.* -> array.unsigned8 */
static int coerce_aa_unsigned8(addr pos, addr type, addr *ret)
{
	uint8_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_unsigned8_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned8(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned8(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.* -> array.unsigned16 */
static int coerce_aa_unsigned16(addr pos, addr type, addr *ret)
{
	uint16_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_unsigned16_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned16(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned16(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array.* -> array.unsigned32 */
static int coerce_aa_unsigned32(addr pos, addr type, addr *ret)
{
	uint32_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_unsigned32_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned32(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned32(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.unsigned64 */
static int coerce_aa_unsigned64(addr pos, addr type, addr *ret)
{
	uint64_t v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_unsigned64_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_unsigned64(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned64(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* array.* -> array.unsigned */
static int coerce_aa_type_unsigned(addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8(pos, type, ret);

		case 16:
			return coerce_aa_unsigned16(pos, type, ret);

		case 32:
			return coerce_aa_unsigned32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
}

/* array -> array.unsigned */
static int coerce_aa_unsigned(addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_unsigned(pos, type, size, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array.float -> array.single */
static int coerce_aa_type_single(addr pos, addr type, addr *ret)
{
	single_float v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_single_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_single(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_single(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array -> array.single-float */
static int coerce_aa_single(addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_type_single(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array.float -> array.double */
static int coerce_aa_type_double(addr pos, addr type, addr *ret)
{
	double_float v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_double_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_double(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_double(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array -> array.double-float */
static int coerce_aa_double(addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_type_double(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array.float -> array.long */
static int coerce_aa_type_long(addr pos, addr type, addr *ret)
{
	long_float v;
	addr array;
	size_t size, i;

	array_rowlength(pos, &size);
	array_coerce_long_heap(&array, pos);
	for (i = 0; i < size; i++) {
		if (array_coerce_long(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_long(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* array -> array.long-float */
static int coerce_aa_long(addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
			return coerce_aa_type_long(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* array -> array */
static int coerce_aa(addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_aa_t(pos, type, ret);

		case LISPDECL_BIT:
			return coerce_aa_bit(pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_aa_character(pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_aa_signed(pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_aa_unsigned(pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_aa_single(pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_aa_double(pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_aa_long(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* vector -> array.bit */
static int coerce_av_bit(addr pos, addr type, addr *ret)
{
	int v;
	addr vector;
	size_t size, i;

	lenarray(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_bit(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		bitmemory_setint(vector, i, v);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* vector -> array.character */
static int coerce_av_character(addr pos, addr type, addr *ret)
{
	unicode v;
	addr vector;
	size_t size, i;

	lenarray(pos, &size);
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_character(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		strvect_setc(vector, i, v);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* vector -> array.signed8 */
static int coerce_av_signed8(addr pos, addr type, addr *ret)
{
	int8_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_signed8_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_signed8(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed8(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* vector -> array.signed16 */
static int coerce_av_signed16(addr pos, addr type, addr *ret)
{
	int16_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_signed16_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_signed16(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed16(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* vector -> array.signed32 */
static int coerce_av_signed32(addr pos, addr type, addr *ret)
{
	int32_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_signed32_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_signed32(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed32(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.signed64 */
static int coerce_av_signed64(addr pos, addr type, addr *ret)
{
	int64_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_signed64_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_signed64(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed64(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* vector -> array.signed */
static int coerce_av_signed(addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_signed8(pos, type, ret);

		case 16:
			return coerce_av_signed16(pos, type, ret);

		case 32:
			return coerce_av_signed32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_signed64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
	return 0;
}

/* vector -> array.unsigned8 */
static int coerce_av_unsigned8(addr pos, addr type, addr *ret)
{
	uint8_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_unsigned8_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_unsigned8(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned8(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* vector -> array.unsigned16 */
static int coerce_av_unsigned16(addr pos, addr type, addr *ret)
{
	uint16_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_unsigned16_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_unsigned16(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned16(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* vector -> array.unsigned32 */
static int coerce_av_unsigned32(addr pos, addr type, addr *ret)
{
	uint32_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_unsigned32_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_unsigned32(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned32(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.unsigned64 */
static int coerce_av_unsigned64(addr pos, addr type, addr *ret)
{
	uint64_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_unsigned64_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_unsigned64(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned64(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* vector -> array.unsigned */
static int coerce_av_unsigned(addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_unsigned8(pos, type, ret);

		case 16:
			return coerce_av_unsigned16(pos, type, ret);

		case 32:
			return coerce_av_unsigned32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_unsigned64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
	return 0;
}

/* vector -> array.single-float */
static int coerce_av_single(addr pos, addr type, addr *ret)
{
	single_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_single_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_single(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_single(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* vector -> array.double-float */
static int coerce_av_double(addr pos, addr type, addr *ret)
{
	double_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_double_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_double(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_double(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* vector -> array.long-float */
static int coerce_av_long(addr pos, addr type, addr *ret)
{
	long_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	vector_coerce_long_heap(&array, size);
	for (i = 0; i < size; i++) {
		if (vector_coerce_long(pos, i, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_long(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* vector -> array */
static int coerce_av(addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_typep(pos, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_av_bit(pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_av_character(pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_av_signed(pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_av_unsigned(pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_av_single(pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_av_double(pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_av_long(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* string -> array.t */
static int coerce_as_t(addr pos, addr type, addr *ret)
{
	unicode c;
	addr vector, value;
	size_t size, i;

	strvect_length(pos, &size);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		character_heap(&value, c);
		setarray(vector, i, value);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* string -> array */
static int coerce_as(addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_as_t(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* bit-vector -> array.t */
static int coerce_ab_t(addr pos, addr type, addr *ret)
{
	int v;
	addr vector, value;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		fixnum_heap(&value, (fixnum)v);
		setarray(vector, i, value);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* bit-vector -> array.signed8 */
static int coerce_ab_signed8(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_signed8_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_signed8(NULL, array, i, (int8_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* bit-vector -> array.signed16 */
static int coerce_ab_signed16(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_signed16_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_signed16(NULL, array, i, (int16_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* bit-vector -> array.signed32 */
static int coerce_ab_signed32(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_signed32_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_signed32(NULL, array, i, (int32_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.signed64 */
static int coerce_ab_signed64(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_signed64_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_signed64(NULL, array, i, (int64_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* bit-vector -> array.signed */
static int coerce_ab_signed(addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_signed8(pos, type, ret);

		case 16:
			return coerce_ab_signed16(pos, type, ret);

		case 32:
			return coerce_ab_signed32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_signed64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
	return 0;
}

/* bit-vector -> array.unsigned8 */
static int coerce_ab_unsigned8(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_unsigned8_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_unsigned8(NULL, array, i, (uint8_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* bit-vector -> array.unsigned16 */
static int coerce_ab_unsigned16(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_unsigned16_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_unsigned16(NULL, array, i, (uint16_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* bit-vector -> array.unsigned32 */
static int coerce_ab_unsigned32(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_unsigned32_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_unsigned32(NULL, array, i, (uint32_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.unsigned64 */
static int coerce_ab_unsigned64(addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_coerce_unsigned64_heap(&array, size);
	for (i = 0; i < size; i++) {
		bitmemory_getint(pos, i, &v);
		if (array_set_unsigned64(NULL, array, i, (uint64_t)v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* bit-vector -> array.unsigned */
static int coerce_ab_unsigned(addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_unsigned8(pos, type, ret);

		case 16:
			return coerce_ab_unsigned16(pos, type, ret);

		case 32:
			return coerce_ab_unsigned32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_unsigned64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
	return 0;
}

/* bit-vector -> array */
static int coerce_ab(addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_ab_t(pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_ab_signed(pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_ab_unsigned(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* list -> array.bit */
static int coerce_al_t(addr pos, addr type, addr *ret)
{
	addr vector, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		setarray(vector, i, value);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* list -> array.bit */
static int coerce_al_bit(addr pos, addr type, addr *ret)
{
	int v;
	addr vector, value;
	size_t size, i;

	size = length_list_safe(pos);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_bit_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		bitmemory_setint(vector, i, v);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* list -> array.character */
static int coerce_al_character(addr pos, addr type, addr *ret)
{
	unicode v;
	addr vector, value;
	size_t size, i;

	size = length_list_safe(pos);
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_character_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		strvect_setc(vector, i, v);
	}

	return coerce_typep(pos, vector, type, ret);
}

/* list -> array.signed8 */
static int coerce_al_signed8(addr pos, addr type, addr *ret)
{
	int8_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_signed8_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_signed8_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed8(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* list -> array.signed16 */
static int coerce_al_signed16(addr pos, addr type, addr *ret)
{
	int16_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_signed16_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_signed16_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed16(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* list -> array.signed32 */
static int coerce_al_signed32(addr pos, addr type, addr *ret)
{
	int32_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_signed32_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_signed32_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed32(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.signed64 */
static int coerce_al_signed64(addr pos, addr type, addr *ret)
{
	int64_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_signed64_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_signed64_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_signed64(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* list -> array.signed */
static int coerce_al_signed(addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_signed8(pos, type, ret);

		case 16:
			return coerce_al_signed16(pos, type, ret);

		case 32:
			return coerce_al_signed32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_signed64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
	return 0;
}

/* list -> array.unsigned8 */
static int coerce_al_unsigned8(addr pos, addr type, addr *ret)
{
	uint8_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_unsigned8_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_unsigned8_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned8(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* list -> array.unsigned16 */
static int coerce_al_unsigned16(addr pos, addr type, addr *ret)
{
	uint16_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_unsigned16_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_unsigned16_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned16(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* list -> array.unsigned32 */
static int coerce_al_unsigned32(addr pos, addr type, addr *ret)
{
	uint32_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_unsigned32_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_unsigned32_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned32(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.unsigned64 */
static int coerce_al_unsigned64(addr pos, addr type, addr *ret)
{
	uint64_t v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_unsigned64_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_unsigned64_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_unsigned64(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}
#endif

/* list -> array.unsigned */
static int coerce_al_unsigned(addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_unsigned8(pos, type, ret);

		case 16:
			return coerce_al_unsigned16(pos, type, ret);

		case 32:
			return coerce_al_unsigned32(pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_unsigned64(pos, type, ret);
#endif

		default:
			coerce_error(pos, type);
			return 0;
	}
	return 0;
}

/* list -> array.single-float */
static int coerce_al_single(addr pos, addr type, addr *ret)
{
	single_float v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_single_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_single_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_single(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* list -> array.double-float */
static int coerce_al_double(addr pos, addr type, addr *ret)
{
	double_float v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_double_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_double_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_double(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* list -> array.long-float */
static int coerce_al_long(addr pos, addr type, addr *ret)
{
	long_float v;
	addr array, value;
	size_t size, i;

	size = length_list_safe(pos);
	vector_coerce_long_heap(&array, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		if (array_coerce_long_t(value, &v)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_long(NULL, array, i, v)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(pos, array, type, ret);
}

/* list -> array */
static int coerce_al(addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_al_t(pos, type, ret);

		case LISPDECL_BIT:
			return coerce_al_bit(pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_al_character(pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_al_signed(pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_al_unsigned(pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_al_single(pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_al_double(pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_al_long(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

/* ? -> array */
static int coerce_array(addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return coerce_aa(pos, type, ret);

		case LISPTYPE_VECTOR:
			return coerce_av(pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_as(pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_ab(pos, type, ret);

		case LISPTYPE_CONS:
		case LISPTYPE_NIL:
			return coerce_al(pos, type, ret);

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}


/*
 *  table
 */
typedef int (*coerce_call)(addr pos, addr type, addr *ret);
static coerce_call CoerceTable[LISPDECL_SIZE];

_g void init_type_coerce(void)
{
	cleartype(CoerceTable);
	/* number */
	CoerceTable[LISPDECL_FLOAT] = coerce_float;
	CoerceTable[LISPDECL_SINGLE_FLOAT] = coerce_single;
	CoerceTable[LISPDECL_DOUBLE_FLOAT] = coerce_double;
	CoerceTable[LISPDECL_LONG_FLOAT] = coerce_long;
	CoerceTable[LISPDECL_COMPLEX] = coerce_complex;
	CoerceTable[LISPDECL_CHARACTER] = coerce_character;
	CoerceTable[LISPDECL_BASE_CHAR] = coerce_character;
	CoerceTable[LISPDECL_STANDARD_CHAR] = coerce_character;
	CoerceTable[LISPDECL_EXTENDED_CHAR] = coerce_character;
	CoerceTable[LISPDECL_FUNCTION] = coerce_function;
	CoerceTable[LISPDECL_COMPILED_FUNCTION] = coerce_function;
	/* list */
	CoerceTable[LISPDECL_LIST] = coerce_list;
	CoerceTable[LISPDECL_CONS] = coerce_list;
	/* array */
	CoerceTable[LISPDECL_ARRAY] = coerce_array;
	CoerceTable[LISPDECL_SIMPLE_ARRAY] = coerce_array;
}

static int coerce_table(addr pos, addr type, addr *ret)
{
	enum LISPDECL decl;
	coerce_call call;

	CheckType(type, LISPTYPE_TYPE);
	decl = LispDecl(type);
	/* call table */
	call = CoerceTable[(int)decl];
	if (call)
		return (*call)(pos, type, ret);
	/* others */
	*ret = Unbound;
	return 0;
}

static int coerce_optimize(addr pos, addr type, addr *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	type_optimize_local(local, &type, type);
	get_type_optimized(&type, type);
	check = coerce_table(pos, type, ret);
	rollback_local(local, stack);

	return check;
}

static int coerce_type(addr pos, addr type, addr *ret)
{
	addr check;

	CheckType(type, LISPTYPE_TYPE);
	if (! RefNotDecl(type)) {
		if (coerce_table(pos, type, &check))
			return 1;
		if (check != Unbound) {
			*ret = check;
			return 0;
		}
	}
	if (coerce_optimize(pos, type, &check))
		return 1;
	if (check != Unbound) {
		*ret = check;
		return 0;
	}

	return coerce_typep(pos, pos, type, ret);
}

static int coerce_parse(Execute ptr, addr pos, addr type, addr *ret)
{
	if (parse_type(ptr, &type, type, Nil))
		return 1;
	else
		return coerce_type(pos, type, ret);
}

_g int coerce_common(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(type)) {
		case LISPTYPE_NIL:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_CONS:
			return coerce_parse(ptr, pos, type, ret);

		case LISPTYPE_TYPE:
			return coerce_type(pos, type, ret);

		case LISPTYPE_T:
			*ret = pos;
			return 0;

		default:
			return coerce_typep(pos, pos, type, ret);
	}
}

