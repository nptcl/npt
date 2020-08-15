#include "array.h"
#include "array_access.h"
#include "array_coerce.h"
#include "array_make.h"
#include "array_sequence.h"
#include "bignum.h"
#include "bignum_data.h"
#include "bignum_object.h"
#include "bit.h"
#include "character.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "copy.h"
#include "execute.h"
#include "float_object.h"
#include "hold.h"
#include "integer.h"
#include "local.h"
#include "ratio.h"
#include "real.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type.h"
#include "type_object.h"
#include "type_optimize.h"
#include "type_parse.h"
#include "type_typep.h"
#include "type_value.h"

static int coerce_type_(Execute ptr, addr pos, addr type, addr *ret);

/*
 *  type
 */
static int coerce_error(Execute ptr, addr pos, addr type)
{
	copyheap(&pos, pos);
	copyheap(&type, type);
	Return(type_object_(&type, type));
	return call_type_error_va_(ptr, pos, type,
			"Cannot covert value ~A to a ~S type.", pos, type, NULL);
}

static int coerce_typep(Execute ptr, addr pos, addr value, addr type, addr *ret)
{
	int check;

	Return(typep_clang_(ptr, value, type, &check));
	if (! check)
		return coerce_error(ptr, pos, type);

	return Result(ret, value);
}


/*
 *  float
 */
static int coerce_fixnum_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	single_float_fixnum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_bignum_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(single_float_bignum_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(single_float_ratio_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_float(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
		case LISPTYPE_DOUBLE_FLOAT:
		case LISPTYPE_LONG_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  single-float
 */
static int coerce_double_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	single_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(cast_ds_value_(pos, &v));
	single_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	single_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(cast_ls_value_(pos, &v));
	single_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_single(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_single(ptr, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_single(ptr, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_single(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_single(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_single(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  double-float
 */
static int coerce_single_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	double_float v;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(cast_sd_value_(pos, &v));
	double_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	double_float v;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(cast_ld_value_(pos, &v));
	double_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_fixnum_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	double_float_fixnum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_bignum_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(double_float_bignum_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(double_float_ratio_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_double(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_double(ptr, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_long_double(ptr, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_double(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_double(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_double(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  long-float
 */
static int coerce_single_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	long_float v;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(cast_sl_value_(pos, &v));
	long_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_double_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;
	long_float v;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(cast_dl_value_(pos, &v));
	long_float_heap(&value, v);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_fixnum_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_FIXNUM);
	long_float_fixnum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_bignum_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(long_float_bignum_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	Return(long_float_ratio_heap_(&value, pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long(Execute ptr, addr pos, addr type, addr *ret)
{
	CheckType(type, LISPTYPE_TYPE);
	switch (GetType(pos)) {
		case LISPTYPE_SINGLE_FLOAT:
			return coerce_single_long(ptr, pos, type, ret);

		case LISPTYPE_DOUBLE_FLOAT:
			return coerce_double_long(ptr, pos, type, ret);

		case LISPTYPE_LONG_FLOAT:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPTYPE_FIXNUM:
			return coerce_fixnum_long(ptr, pos, type, ret);

		case LISPTYPE_BIGNUM:
			return coerce_bignum_long(ptr, pos, type, ret);

		case LISPTYPE_RATIO:
			return coerce_ratio_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  complex
 */
static int coerce_complex_complex(Execute ptr, addr pos, addr type, addr *ret)
{
	addr real, imag;
	LocalHold hold;

	GetArrayType(type, 0, &type);
	GetRealComplex(pos, &real);
	GetImagComplex(pos, &imag);

	Return(coerce_type_(ptr, real, type, &real));
	hold = LocalHold_local_push(ptr, real);
	Return(coerce_type_(ptr, imag, type, &imag));
	localhold_end(hold);
	Return(complex_heap_(ret, real, imag));

	return 0;
}

static int coerce_complex_real(Execute ptr, addr pos, addr type, addr *ret)
{
	GetArrayType(type, 0, &type);
	if (! type_asterisk_p(type)) {
		Return(coerce_type_(ptr, pos, type, &pos));
	}

	return complex_heap_(ret, pos, fixnumh(0));
}

static int coerce_complex(Execute ptr, addr pos, addr type, addr *ret)
{
	if (complexp(pos))
		return coerce_complex_complex(ptr, pos, type, ret);
	if (realp(pos))
		return coerce_complex_real(ptr, pos, type, ret);
	else
		return coerce_typep(ptr, pos, pos, type, ret);
}


/*
 *  charcter
 */
static int coerce_unicode_character(Execute ptr,
		addr pos, unicode c, addr type, addr *ret)
{
	addr value;
	character_heap(&value, c);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_string_character(Execute ptr, addr pos, addr type, addr *ret)
{
	unicode c;
	size_t size;

	string_length(pos, &size);
	if (size != 1)
		return coerce_error(ptr, pos, type);
	Return(string_getc_(pos, 0, &c));
	return coerce_unicode_character(ptr, pos, c, type, ret);
}

static int coerce_symbol_character(Execute ptr, addr pos, addr type, addr *ret)
{
	addr name;
	unicode c;
	size_t size;

	GetNameSymbol(pos, &name);
	string_length(name, &size);
	if (size != 1)
		return coerce_error(ptr, pos, type);
	Return(string_getc_(name, 0, &c));
	return coerce_unicode_character(ptr, pos, c, type, ret);
}

static int coerce_character(Execute ptr, addr pos, addr type, addr *ret)
{
	/* (or symbol string character) */
	if (stringp(pos))
		return coerce_string_character(ptr, pos, type, ret);
	else if (symbolp(pos))
		return coerce_symbol_character(ptr, pos, type, ret);
	else
		return coerce_typep(ptr, pos, pos, type, ret);
}


/*
 *  function
 */
static int coerce_function(Execute ptr, addr pos, addr type, addr *ret)
{
	addr call;

	if (symbolp(pos)) {
		Return(getfunction_global_(pos, &call));
		return coerce_typep(ptr, pos, call, type, ret);
	}
	else {
		return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  list
 */
static int coerce_vector_list(Execute ptr, addr pos, addr type, addr *ret)
{
	addr list, x;
	size_t size, i;

	lenarray(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		getarray(pos, i, &x);
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_string_list(Execute ptr, addr pos, addr type, addr *ret)
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
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_array_list(Execute ptr, addr pos, addr type, addr *ret)
{
	addr list, x;
	size_t size, i;

	/* not vector */
	if (! array_vector_p(pos))
		return coerce_typep(ptr, pos, type, type, ret);

	/* cast list */
	list = Nil;
	size = array_get_vector_length(pos, 1);
	for (i = 0; i < size; i++) {
		Return(array_get_(NULL, pos, i, &x));
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_bitvector_list(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr list, x;
	size_t size, i;

	Return(bitvector_length_(pos, &size));
	list = Nil;
	for (i = 0; i < size; i++) {
		Return(bitvector_getint_(pos, i, &v));
		fixnum_heap(&x, (fixnum)v);
		cons_heap(&list, x, list);
	}
	nreverse(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_list(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_VECTOR:
			return coerce_vector_list(ptr, pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_string_list(ptr, pos, type, ret);

		case LISPTYPE_ARRAY:
			return coerce_array_list(ptr, pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_bitvector_list(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  array
 */
/* array.bit -> array.t */
static int coerce_aa_bit_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int bit, check;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &bit, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)bit);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.character -> array.t */
static int coerce_aa_character_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode c;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_character_(pos, i, &c, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		character_heap(&value, c);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.signed8 -> array.t */
static int coerce_aa_signed8_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.signed16 -> array.t */
static int coerce_aa_signed16_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.signed32 -> array.t */
static int coerce_aa_signed32_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.signed64 -> array.t */
static int coerce_aa_signed64_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.signed -> array.t */
static int coerce_aa_signed_t(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8_t(ptr, pos, type, ret);

		case 16:
			return coerce_aa_signed16_t(ptr, pos, type, ret);

		case 32:
			return coerce_aa_signed32_t(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64_t(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* array.unsigned8 -> array.t */
static int coerce_aa_unsigned8_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.unsigned16 -> array.t */
static int coerce_aa_unsigned16_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		fixnum_heap(&value, (fixnum)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.unsigned32 -> array.t */
static int coerce_aa_unsigned32_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
#ifdef LISP_64BIT
		fixnum_heap(&value, (fixnum)v);
#else
		integer_fixed_heap(&value, SignPlus, (fixed)v);
#endif
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.unsigned64 -> array.t */
static int coerce_aa_unsigned64_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		integer_fixed_heap(&value, SignPlus, (fixed)v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.unsigned -> array.t */
static int coerce_aa_unsigned_t(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8_t(ptr, pos, type, ret);

		case 16:
			return coerce_aa_unsigned16_t(ptr, pos, type, ret);

		case 32:
			return coerce_aa_unsigned32_t(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64_t(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* array.single-float -> array.t */
static int coerce_aa_single_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_single_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		single_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.double-float -> array.t */
static int coerce_aa_double_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_double_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		double_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.long-float -> array.t */
static int coerce_aa_long_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array, value;
	size_t size, i;

	/* make general array */
	Return(array_coerce_t_heap_(&array, pos));
	array_get_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_long_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		long_float_heap(&value, v);
		Return(array_set_(array, i, value));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.t */
static int coerce_aa_t(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_typep(ptr, pos, pos, type, ret);

		case ARRAY_TYPE_BIT:
			return coerce_aa_bit_t(ptr, pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
			return coerce_aa_character_t(ptr, pos, type, ret);

		case ARRAY_TYPE_SIGNED:
			return coerce_aa_signed_t(ptr, pos, type, str->bytesize, ret);

		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_unsigned_t(ptr, pos, type, str->bytesize, ret);

		case ARRAY_TYPE_SINGLE_FLOAT:
			return coerce_aa_single_t(ptr, pos, type, ret);

		case ARRAY_TYPE_DOUBLE_FLOAT:
			return coerce_aa_double_t(ptr, pos, type, ret);

		case ARRAY_TYPE_LONG_FLOAT:
			return coerce_aa_long_t(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.t -> bitvector */
static int coerce_aa_bitvector(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector;
	size_t size, i;

	array_get_rowlength(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* array.* -> array.bit */
static int coerce_aa_type_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_bitvector(ptr, pos, type, ret);

	/* array.bit */
	array_get_rowlength(pos, &size);
	Return(array_coerce_bit_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_bit_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.bit */
static int coerce_aa_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_bit(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.t -> string */
static int coerce_aa_string(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr vector;
	size_t size, i;

	array_get_rowlength(pos, &size);
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(array_coerce_character_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* array.t -> array.character */
static int coerce_aa_t_character(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr array;
	size_t size, i;

	/* bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_string(ptr, pos, type, ret);

	/* array.bit */
	array_get_rowlength(pos, &size);
	Return(array_coerce_character_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_character_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_character_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.character */
static int coerce_aa_character(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_aa_t_character(ptr, pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.* -> array.signed8 */
static int coerce_aa_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed8_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.signed16 */
static int coerce_aa_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed16_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.signed32 */
static int coerce_aa_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed32_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.signed64 */
static int coerce_aa_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_signed64_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_signed64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.* -> array.signed */
static int coerce_aa_type_signed(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_aa_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_aa_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
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
	if (GetIndex_integer(check, &size))
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

static int coerce_aa_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_signed(ptr, pos, type, size, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.* -> array.unsigned8 */
static int coerce_aa_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned8_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.unsigned16 */
static int coerce_aa_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned16_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array.* -> array.unsigned32 */
static int coerce_aa_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned32_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* array.* -> array.unsigned64 */
static int coerce_aa_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_unsigned64_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_unsigned64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* array.* -> array.unsigned */
static int coerce_aa_type_unsigned(Execute ptr,
		addr pos, addr type, unsigned size, addr *ret)
{
	switch (size) {
		case 8:
			return coerce_aa_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_aa_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_aa_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_aa_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* array -> array.unsigned */
static int coerce_aa_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (str->type) {
		case ARRAY_TYPE_T:
		case ARRAY_TYPE_BIT:
		case ARRAY_TYPE_SIGNED:
		case ARRAY_TYPE_UNSIGNED:
			return coerce_aa_type_unsigned(ptr, pos, type, size, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.single */
static int coerce_aa_type_single(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_single_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_single_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.single-float */
static int coerce_aa_single(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_aa_type_single(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.double */
static int coerce_aa_type_double(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_double_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_double_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.double-float */
static int coerce_aa_double(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_aa_type_double(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array.float -> array.long */
static int coerce_aa_type_long(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array;
	size_t size, i;

	array_get_rowlength(pos, &size);
	Return(array_coerce_long_heap_(&array, pos));
	for (i = 0; i < size; i++) {
		Return(array_coerce_long_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* array -> array.long-float */
static int coerce_aa_long(Execute ptr, addr pos, addr type, addr *ret)
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
			return coerce_aa_type_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* array -> array */
static int coerce_aa(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_aa_t(ptr, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_aa_bit(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_aa_character(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_aa_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_aa_unsigned(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_aa_single(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_aa_double(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_aa_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* vector -> array.bit */
static int coerce_av_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector;
	size_t size, i;

	lenarray(pos, &size);
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		Return(vector_coerce_bit_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* vector -> array.character */
static int coerce_av_character(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr vector;
	size_t size, i;

	lenarray(pos, &size);
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(vector_coerce_character_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* vector -> array.signed8 */
static int coerce_av_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.signed16 */
static int coerce_av_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.signed32 */
static int coerce_av_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.signed64 */
static int coerce_av_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_signed64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_signed64_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* vector -> array.signed */
static int coerce_av_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_av_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_av_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* vector -> array.unsigned8 */
static int coerce_av_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned8_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.unsigned16 */
static int coerce_av_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned16_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.unsigned32 */
static int coerce_av_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned32_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* vector -> array.unsigned64 */
static int coerce_av_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_unsigned64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_unsigned64_(pos, i, &v, &check))
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* vector -> array.unsigned */
static int coerce_av_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_av_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_av_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_av_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_av_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* vector -> array.single-float */
static int coerce_av_single(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_single_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_single_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.double-float */
static int coerce_av_double(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_double_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_double_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array.long-float */
static int coerce_av_long(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array;
	size_t size, i;

	lenarray(pos, &size);
	Return(vector_coerce_long_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(vector_coerce_long_(pos, i, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* vector -> array */
static int coerce_av(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_typep(ptr, pos, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_av_bit(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_av_character(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_av_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_av_unsigned(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_av_single(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_av_double(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_av_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* string -> array.t */
static int coerce_as_t(Execute ptr, addr pos, addr type, addr *ret)
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

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* string -> array */
static int coerce_as(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_as_t(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* bit-vector -> array.t */
static int coerce_ab_t(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr vector, value;
	size_t size, i;

	bitmemory_length(pos, &size);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		fixnum_heap(&value, (fixnum)v);
		setarray(vector, i, value);
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* bit-vector -> array.signed8 */
static int coerce_ab_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed8_(array, i, (int8_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.signed16 */
static int coerce_ab_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed16_(array, i, (int16_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.signed32 */
static int coerce_ab_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed32_(array, i, (int32_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.signed64 */
static int coerce_ab_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_signed64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_signed64_(array, i, (int64_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* bit-vector -> array.signed */
static int coerce_ab_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_ab_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_ab_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* bit-vector -> array.unsigned8 */
static int coerce_ab_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned8_(array, i, (uint8_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.unsigned16 */
static int coerce_ab_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned16_(array, i, (uint16_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* bit-vector -> array.unsigned32 */
static int coerce_ab_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned32_(array, i, (uint32_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* bit-vector -> array.unsigned64 */
static int coerce_ab_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int v;
	addr array;
	size_t size, i;

	bitmemory_length(pos, &size);
	Return(vector_coerce_unsigned64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		Return(bitmemory_getint_(pos, i, &v));
		Return(array_set_unsigned64_(array, i, (uint64_t)v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* bit-vector -> array.unsigned */
static int coerce_ab_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_ab_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_ab_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_ab_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_ab_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* bit-vector -> array */
static int coerce_ab(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_ab_t(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_ab_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_ab_unsigned(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* list -> array.bit */
static int coerce_al_t(Execute ptr, addr pos, addr type, addr *ret)
{
	addr vector, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		setarray(vector, i, value);
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* list -> array.bit */
static int coerce_al_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	int v, check;
	addr vector, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	bitmemory_unsafe(NULL, &vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_bit_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(bitmemory_setint_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* list -> array.character */
static int coerce_al_character(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	unicode v;
	addr vector, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	strvect_heap(&vector, size);
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_character_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(strvect_setc_(vector, i, v));
	}

	return coerce_typep(ptr, pos, vector, type, ret);
}

/* list -> array.signed8 */
static int coerce_al_signed8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int8_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed8_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.signed16 */
static int coerce_al_signed16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int16_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed16_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.signed32 */
static int coerce_al_signed32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int32_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed32_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.signed64 */
static int coerce_al_signed64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	int64_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_signed64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_signed64_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_signed64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* list -> array.signed */
static int coerce_al_signed(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_signed8(ptr, pos, type, ret);

		case 16:
			return coerce_al_signed16(ptr, pos, type, ret);

		case 32:
			return coerce_al_signed32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_signed64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* list -> array.unsigned8 */
static int coerce_al_unsigned8(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint8_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned8_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned8_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.unsigned16 */
static int coerce_al_unsigned16(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint16_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned16_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned16_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned16_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.unsigned32 */
static int coerce_al_unsigned32(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint32_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned32_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned32_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned32_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

#ifdef LISP_64BIT
/* list -> array.unsigned64 */
static int coerce_al_unsigned64(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	uint64_t v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_unsigned64_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_unsigned64_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_unsigned64_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}
#endif

/* list -> array.unsigned */
static int coerce_al_unsigned(Execute ptr, addr pos, addr type, addr *ret)
{
	unsigned size;

	if (type_second_size(type, &size))
		return coerce_typep(ptr, pos, pos, type, ret);
	switch (size) {
		case 8:
			return coerce_al_unsigned8(ptr, pos, type, ret);

		case 16:
			return coerce_al_unsigned16(ptr, pos, type, ret);

		case 32:
			return coerce_al_unsigned32(ptr, pos, type, ret);

#ifdef LISP_64BIT
		case 64:
			return coerce_al_unsigned64(ptr, pos, type, ret);
#endif

		default:
			return coerce_error(ptr, pos, type);
	}
}

/* list -> array.single-float */
static int coerce_al_single(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	single_float v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_single_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_single_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_single_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.double-float */
static int coerce_al_double(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	double_float v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_double_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_double_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_double_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array.long-float */
static int coerce_al_long(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	long_float v;
	addr array, value;
	size_t size, i;

	Return(length_list_safe_(pos, &size));
	Return(vector_coerce_long_heap_(&array, size));
	for (i = 0; i < size; i++) {
		GetCons(pos, &value, &pos);
		Return(array_coerce_long_t_(value, &v, &check));
		if (check)
			return coerce_error(ptr, pos, type);
		Return(array_set_long_(array, i, v));
	}

	return coerce_typep(ptr, pos, array, type, ret);
}

/* list -> array */
static int coerce_al(Execute ptr, addr pos, addr type, addr *ret)
{
	addr upg;

	/* (array * .) */
	GetArrayType(type, 0, &upg);
	if (type_asterisk_p(upg))
		return coerce_al_t(ptr, pos, type, ret);

	/* (array upg .) */
	switch (LispDecl(upg)) {
		case LISPDECL_T:
			return coerce_al_t(ptr, pos, type, ret);

		case LISPDECL_BIT:
			return coerce_al_bit(ptr, pos, type, ret);

		case LISPDECL_CHARACTER:
			return coerce_al_character(ptr, pos, type, ret);

		case LISPDECL_SIGNED_BYTE:
			return coerce_al_signed(ptr, pos, type, ret);

		case LISPDECL_UNSIGNED_BYTE:
			return coerce_al_unsigned(ptr, pos, type, ret);

		case LISPDECL_SINGLE_FLOAT:
			return coerce_al_single(ptr, pos, type, ret);

		case LISPDECL_DOUBLE_FLOAT:
			return coerce_al_double(ptr, pos, type, ret);

		case LISPDECL_LONG_FLOAT:
			return coerce_al_long(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

/* ? -> array */
static int coerce_array(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return coerce_aa(ptr, pos, type, ret);

		case LISPTYPE_VECTOR:
			return coerce_av(ptr, pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_as(ptr, pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_ab(ptr, pos, type, ret);

		case LISPTYPE_CONS:
		case LISPTYPE_NIL:
			return coerce_al(ptr, pos, type, ret);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}


/*
 *  table
 */
typedef int (*coerce_call)(Execute ptr, addr pos, addr type, addr *ret);
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

static int coerce_table(Execute ptr, addr pos, addr type, addr *ret)
{
	enum LISPDECL decl;
	coerce_call call;

	CheckType(type, LISPTYPE_TYPE);
	if (! RefNotDecl(type)) {
		decl = LispDecl(type);
		/* call table */
		call = CoerceTable[(int)decl];
		if (call)
			return (*call)(ptr, pos, type, ret);
	}
	/* others */
	return Result(ret, Unbound);
}

static int coerce_optimize(Execute ptr, addr pos, addr type, addr *ret)
{
	int ignore;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	Return(type_optimize_local_(local, type, &type, &ignore));
	get_type_optimized(&type, type);
	Return(coerce_table(ptr, pos, type, ret));
	rollback_local(local, stack);

	return 0;
}

static int coerce_type_call(Execute ptr, addr pos, addr type, addr *ret)
{
	addr check;

	CheckType(type, LISPTYPE_TYPE);
	if (! RefNotDecl(type)) {
		Return(coerce_table(ptr, pos, type, &check));
		if (check != Unbound)
			return Result(ret, check);
	}
	Return(coerce_optimize(ptr, pos, type, &check));
	if (check != Unbound)
		return Result(ret, check);

	return coerce_typep(ptr, pos, pos, type, ret);
}

static int coerce_type_(Execute ptr, addr pos, addr type, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, pos, type, NULL);
	Return(coerce_type_call(ptr, pos, type, ret));
	localhold_end(hold);

	return 0;
}

static int coerce_parse(Execute ptr, addr pos, addr type, addr *ret)
{
	Return(parse_type(ptr, &type, type, Nil));
	return coerce_type_(ptr, pos, type, ret);
}

static int coerce_execute_(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(type)) {
		case LISPTYPE_NIL:
		case LISPTYPE_SYMBOL:
		case LISPTYPE_CONS:
			return coerce_parse(ptr, pos, type, ret);

		case LISPTYPE_TYPE:
			return coerce_type_(ptr, pos, type, ret);

		case LISPTYPE_T:
			return Result(ret, pos);

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

_g int coerce_common(Execute ptr, addr pos, addr type, addr *ret)
{
	LocalHold hold;

	hold = LocalHold_local(ptr);
	localhold_pushva(hold, pos, type, NULL);
	Return(coerce_execute_(ptr, pos, type, ret));
	localhold_end(hold);

	return 0;
}

