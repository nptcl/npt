#include "bigdata.h"
#include "bignum.h"
#include "bit.h"
#include "cmpl.h"
#include "condition.h"
#include "cons.h"
#include "copy.h"
#include "execute.h"
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

static int coerce_type(Execute ptr, addr pos, addr type, addr *ret);

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

static int coerce_typep(Execute ptr,
		addr pos, addr value, addr type, addr *ret)
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
	single_float_bignum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	single_float_ratio_heap(&value, pos);
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

	return 0;
}


/*
 *  single-float
 */
static int coerce_double_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	single_float_heap(&value, cast_ds_value(pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long_single(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	single_float_heap(&value, cast_ls_value(pos));
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

	return 0;
}


/*
 *  double-float
 */
static int coerce_single_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	double_float_heap(&value, cast_sd_value(pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_long_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	double_float_heap(&value, cast_ld_value(pos));
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
	double_float_bignum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_double(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	double_float_ratio_heap(&value, pos);
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

	return 0;
}


/*
 *  long-float
 */
static int coerce_single_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	long_float_heap(&value, cast_sd_value(pos));
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_double_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	long_float_heap(&value, cast_ld_value(pos));
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
	long_float_bignum_heap(&value, pos);
	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_ratio_long(Execute ptr, addr pos, addr type, addr *ret)
{
	addr value;

	CheckType(pos, LISPTYPE_RATIO);
	long_float_ratio_heap(&value, pos);
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

	return 0;
}


/*
 *  complex
 */
static int coerce_complex(Execute ptr, addr pos, addr type, addr *ret)
{
	addr imag;

	if (! realp(pos))
		return coerce_typep(ptr, pos, pos, type, ret);

	/* (complex *) */
	GetArrayType(type, 0, &type);
	if (type_asterisk_p(type)) {
		*ret = pos;
		return 0;
	}

	/* (complex type) */
	if (coerce_type(ptr, pos, type, &pos))
		return 1;
	fixnum_heap(&imag, 0);
	complex_heap(ret, pos, imag);

	return 0;
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
		coerce_error(pos, type);
	string_getc(pos, 0, &c);
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
		coerce_error(pos, type);
	string_getc(pos, 0, &c);
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
		getfunctioncheck_local(ptr, pos, &call);
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
	nreverse_list_unsafe(&list, list);

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
	nreverse_list_unsafe(&list, list);

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
	size = array_vector_length(pos, 1);
	for (i = 0; i < size; i++) {
		array_get(NULL, pos, i, &x);
		cons_heap(&list, x, list);
	}
	nreverse_list_unsafe(&list, list);

	return coerce_typep(ptr, pos, list, type, ret);
}

static int coerce_bitvector_list(Execute ptr, addr pos, addr type, addr *ret)
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
static int coerce_array_get_bit_t(addr pos, int *ret)
{
	fixnum v;

	switch (GetType(pos)) {
		case LISPTYPE_FIXNUM:
			GetFixnum(pos, &v);
			if (v == 0) {
				*ret = 0;
				return 0;
			}
			if (v == 1) {
				*ret = 1;
				return 0;
			}
			return 1;

		case LISPTYPE_BIGNUM:
			if (zerop_bignum(pos)) {
				*ret = 0;
				return 0;
			}
			if (equal_value_bignum(pos, SignPlus, 1)) {
				*ret = 1;
				return 0;
			}
			return 1;

		case LISPTYPE_RATIO:
			if (zerop_ratio(pos)) {
				*ret = 0;
				return 0;
			}
			if (equal_value_ratio(pos, SignPlus, 1, 1)) {
				*ret = 1;
				return 0;
			}
			return 1;

		default:
			return 1;
	}
}

static int coerce_array_get_bit_signed(struct array_value *ptr, int *ret)
{
	int8_t s8;
	int16_t s16;
	int32_t s32;
#ifdef LISP_64BIT
	int64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.signed8;
			if (s8 == 0 || s8 == 1) {
				*ret = (int)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.signed16;
			if (s16 == 0 || s16 == 1) {
				*ret = (int)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.signed32;
			if (s32 == 0 || s32 == 1) {
				*ret = (int)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.signed64;
			if (s64 == 0 || s64 == 1) {
				*ret = (int)s64;
				return 0;
			}
			return 1;
#endif
		default:
			return 1;
	}
}

static int coerce_array_get_bit_unsigned(struct array_value *ptr, int *ret)
{
	uint8_t s8;
	uint16_t s16;
	uint32_t s32;
#ifdef LISP_64BIT
	uint64_t s64;
#endif

	switch (ptr->size) {
		case 8:
			s8 = ptr->value.unsigned8;
			if (s8 == 0 || s8 == 1) {
				*ret = (int)s8;
				return 0;
			}
			return 1;

		case 16:
			s16 = ptr->value.unsigned16;
			if (s16 == 0 || s16 == 1) {
				*ret = (int)s16;
				return 0;
			}
			return 1;

		case 32:
			s32 = ptr->value.unsigned32;
			if (s32 == 0 || s32 == 1) {
				*ret = (int)s32;
				return 0;
			}
			return 1;

#ifdef LISP_64BIT
		case 64:
			s64 = ptr->value.unsigned64;
			if (s64 == 0 || s64 == 1) {
				*ret = (int)s64;
				return 0;
			}
			return 1;
#endif
		default:
			return 1;
	}
}

static int coerce_array_get_bit(addr pos, size_t i, int *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return coerce_array_get_bit_t(value.value.object, ret);

		case ARRAY_TYPE_BIT:
			*ret = (int)value.value.bit;
			return 0;

		case ARRAY_TYPE_SIGNED:
			return coerce_array_get_bit_signed(&value, ret);

		case ARRAY_TYPE_UNSIGNED:
			return coerce_array_get_bit_unsigned(&value, ret);

		default:
			return 1;
	}
}

static int coerce_aa_t_bitvector(Execute ptr, addr pos, addr type, addr *ret)
{
	int bit;
	addr value;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	bitmemory_heap(&value, str->front);
	size = array_vector_length(pos, 1);
	for (i = 0; i < size; i++) {
		if (coerce_array_get_bit(pos, i, &bit)) {
			coerce_error(pos, type);
			return 0;
		}
		bitmemory_setint(value, i, bit);
	}

	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_aa_t_bit(Execute ptr, addr pos, addr type, addr *ret)
{
	int bit;
	addr value;
	size_t size, i;

	/* make bit-vector */
	if (array_vector_p(pos))
		return coerce_aa_t_bitvector(ptr, pos, type, ret);

	/* make specialized array */
	array_coerce_bit_heap(&value, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (coerce_array_get_bit(pos, i, &bit)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_bit(NULL, value, i, bit)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_array_get_character_t(addr pos, unicode *ret)
{
	size_t size;

	if (symbolp(pos)) {
		GetNameSymbol(pos, &pos);
	}
	if (stringp(pos)) {
		string_length(pos, &size);
		if (size != 1)
			return 1;
		string_getc(pos, 0, ret);
		return 0;
	}
	if (characterp(pos)) {
		GetCharacter(pos, ret);
		return 0;
	}

	return 1;
}

static int coerce_array_get_character(addr pos, size_t i, unicode *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return coerce_array_get_character_t(value.value.object, ret);

		case ARRAY_TYPE_CHARACTER:
			*ret = value.value.character;
			return 0;

		default:
			return 1;
	}
}

static int coerce_aa_t_string(Execute ptr, addr pos, addr type, addr *ret)
{
	unicode c;
	addr value;
	struct array_struct *str;
	size_t size, i;

	str = ArrayInfoStruct(pos);
	strvect_heap(&value, str->front);
	size = array_vector_length(pos, 1);
	for (i = 0; i < size; i++) {
		if (coerce_array_get_character(pos, i, &c)) {
			coerce_error(pos, type);
			return 0;
		}
		strvect_setc(value, i, c);
	}

	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_aa_t_character(Execute ptr, addr pos, addr type, addr *ret)
{
	unicode c;
	addr value;
	size_t size, i;

	/* make string */
	if (array_vector_p(pos))
		return coerce_aa_t_string(ptr, pos, type, ret);

	/* make specialized array */
	array_coerce_character_heap(&value, pos);
	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (coerce_array_get_character(pos, i, &c)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_bit(NULL, value, i, c)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_array_get_signed_t(addr pos, unsigned bs, unicode *ret)
{
	return 0;
}

static int coerce_array_get_signed(addr pos, size_t i, unsigned bs, unicode *ret)
{
	struct array_value value;

	array_get_inplace(pos, i, &value);
	switch (value.type) {
		case ARRAY_TYPE_T:
			return coerce_array_get_signed_t(value.value.object, bs, ret);

		case ARRAY_TYPE_SIGNED:
			*ret = 0;
			return 0; //coerce_array_get_signed_signed(&value, bs, ret);

		case ARRAY_TYPE_UNSIGNED:
			*ret = 0;
			return 0; //coerce_array_get_signed_unsigned(&value, bs, ret);

		default:
			*ret = 0;
			return 1;
	}
}

static int coerce_aa_t_signed_vector(Execute ptr,
		addr pos, addr type, unsigned bs, addr *ret)
{
	return 0;
}

static int array_coerce_signed_heap(addr *ret, addr pos, unsigned bs)
{
	switch (bs) {
		case 8:
			array_coerce_signed8_heap(ret, pos);
			return 0;

		case 16:
			array_coerce_signed16_heap(ret, pos);
			return 0;

		case 32:
			array_coerce_signed32_heap(ret, pos);
			return 0;

#ifdef LISP_64BIT
		case 64:
			array_coerce_signed64_heap(ret, pos);
			return 0;
#endif

		default:
			return 1;
	}
}

static int coerce_aa_t_signed(Execute ptr,
		addr pos, addr type, unsigned bs, addr *ret)
{
	unicode c;
	addr value;
	size_t size, i;

	/* make string */
	if (array_vector_p(pos))
		return coerce_aa_t_signed_vector(ptr, pos, type, bs ,ret);

	/* make specialized array */
	if (array_coerce_signed_heap(&value, pos, bs)) {
		coerce_error(pos, type);
		return 0;
	}

	array_rowlength(pos, &size);
	for (i = 0; i < size; i++) {
		if (coerce_array_get_signed(pos, i, bs, &c)) {
			coerce_error(pos, type);
			return 0;
		}
		if (array_set_bit(NULL, value, i, c)) {
			coerce_error(pos, type);
			return 0;
		}
	}

	return coerce_typep(ptr, pos, value, type, ret);
}

static int coerce_aa_t(Execute ptr, addr pos, addr type, addr *ret)
{
	struct array_struct *str;

	str = ArrayInfoStruct(pos);
	switch (str->type) {
		case ARRAY_TYPE_T:
			return coerce_typep(ptr, pos, pos, type, ret);

		case ARRAY_TYPE_BIT:
			return coerce_aa_t_bit(ptr, pos, type, ret);

		case ARRAY_TYPE_CHARACTER:
			return coerce_aa_t_character(ptr, pos, type, ret);

		case ARRAY_TYPE_SIGNED:
			return coerce_aa_t_signed(ptr, pos, type, str->bytesize, ret);

		case ARRAY_TYPE_UNSIGNED:
		case ARRAY_TYPE_SINGLE_FLOAT:
		case ARRAY_TYPE_DOUBLE_FLOAT:
		case ARRAY_TYPE_LONG_FLOAT:
		default:
			return 0;
	}
}

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
		case LISPDECL_CHARACTER:
		case LISPDECL_SIGNED_BYTE:
		case LISPDECL_UNSIGNED_BYTE:
		case LISPDECL_SINGLE_FLOAT:
		case LISPDECL_DOUBLE_FLOAT:
		case LISPDECL_LONG_FLOAT:
		default:
			return 0;
	}
}

static int coerce_array(Execute ptr, addr pos, addr type, addr *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_ARRAY:
			return coerce_aa(ptr, pos, type, ret);

#if 0
		case LISPTYPE_VECTOR:
			return coerce_av(ptr, pos, type, ret);

		case LISPTYPE_STRING:
			return coerce_as(ptr, pos, type, ret);

		case LISPTYPE_BITVECTOR:
			return coerce_ab(ptr, pos, type, ret);

		case LISPTYPE_CONS:
			return coerce_al(ptr, pos, type, ret);

		case LISPTYPE_NIL:
			return coerce_an(ptr, pos, type, ret);
#endif

		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

static int coerce_vector(Execute ptr, addr pos, addr type, addr *ret)
{
	return 0;
}

static int coerce_bit_vector(Execute ptr, addr pos, addr type, addr *ret)
{
	return 0;
}

static int coerce_string(Execute ptr, addr pos, addr type, addr *ret)
{
	return 0;
}


/*
 *  table
 */
typedef int (*coerce_call)(Execute ptr, addr pos, addr type, addr *ret);
static coerce_call CoerceTable[LISPDECL_SIZE];

void init_type_coerce(void)
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
	/* vector */
	CoerceTable[LISPDECL_VECTOR] = coerce_vector;
	CoerceTable[LISPDECL_SIMPLE_VECTOR] = coerce_vector;
	/* bit-vector */
	CoerceTable[LISPDECL_BIT_VECTOR] = coerce_bit_vector;
	CoerceTable[LISPDECL_SIMPLE_BIT_VECTOR] = coerce_bit_vector;
	/* string */
	CoerceTable[LISPDECL_STRING] = coerce_string;
	CoerceTable[LISPDECL_BASE_STRING] = coerce_string;
	CoerceTable[LISPDECL_SIMPLE_STRING] = coerce_string;
	CoerceTable[LISPDECL_SIMPLE_BASE_STRING] = coerce_string;
}

static int coerce_table(Execute ptr, addr pos, addr type, addr *ret)
{
	enum LISPDECL decl;
	coerce_call call;

	CheckType(type, LISPTYPE_TYPE);
	decl = LispDecl(type);
	/* call table */
	call = CoerceTable[(int)decl];
	if (call)
		return (*call)(ptr, pos, type, ret);
	/* others */
	*ret = Unbound;
	return 0;
}

static int coerce_optimize(Execute ptr, addr pos, addr type, addr *ret)
{
	int check;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	type_optimize_local(local, &type, type);
	get_type_optimized(&type, type);
	check = coerce_table(ptr, pos, type, ret);
	rollback_local(local, stack);

	return check;
}

static int coerce_type(Execute ptr, addr pos, addr type, addr *ret)
{
	addr check;

	CheckType(type, LISPTYPE_TYPE);
	if (! RefNotDecl(type)) {
		if (coerce_table(ptr, pos, type, &check))
			return 1;
		if (check != Unbound) {
			*ret = check;
			return 0;
		}
	}
	if (coerce_optimize(ptr, pos, type, &check))
		return 1;
	if (check != Unbound) {
		*ret = check;
		return 0;
	}

	return coerce_typep(ptr, pos, pos, type, ret);
}

static int coerce_parse(Execute ptr, addr pos, addr type, addr *ret)
{
	if (parse_type(ptr, &pos, pos, Nil))
		return 1;
	else
		return coerce_type(ptr, pos, type, ret);
}

int coerce_common(Execute ptr, addr pos, addr type, addr *ret)
{
	fmte("TODO", NULL);




	switch (GetType(type)) {
		case LISPTYPE_SYMBOL:
		case LISPTYPE_CONS:
			return coerce_parse(ptr, pos, type, ret);

		case LISPTYPE_TYPE:
			return coerce_type(ptr, pos, type, ret);

		case LISPTYPE_T:
			*ret = pos;
			return 0;

		case LISPTYPE_NIL:
		default:
			return coerce_typep(ptr, pos, pos, type, ret);
	}
}

