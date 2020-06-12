#include "bigdata.h"
#include "bignum.h"
#include "character.h"
#include "cmpl.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_type.h"
#include "compile_value.h"
#include "condition.h"
#include "define.h"
#include "execute.h"
#include "package.h"
#include "ratio.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  nil
 */
_g int faslwrite_value_nil(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_NIL);
	faslwrite_type(stream, FaslCode_nil);
	return 0;
}

_g int faslread_value_nil(Execute ptr, addr stream, addr *ret)
{
	*ret = Nil;
	return 0;
}


/*
 *  t
 */
_g int faslwrite_value_t(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_T);
	faslwrite_type(stream, FaslCode_t);
	return 0;
}

_g int faslread_value_t(Execute ptr, addr stream, addr *ret)
{
	*ret = T;
	return 0;
}


/*
 *  cons
 */
_g int faslwrite_value_cons(Execute ptr, addr stream, addr pos)
{
	addr car, cdr;

	CheckType(pos, LISPTYPE_CONS);
	faslwrite_type(stream, FaslCode_cons);
	GetCons(pos, &car, &cdr);
	Return(faslwrite_value(ptr, stream, car));
	Return(faslwrite_value(ptr, stream, cdr));

	return 0;
}

_g int faslread_value_cons(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_object(ptr, stream, &car));
	Return(faslread_object(ptr, stream, &cdr));
	cons_heap(ret, car, cdr);

	return 0;
}


/*
 *  vector
 */
static int faslwrite_value_vector2(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	faslwrite_type(stream, FaslCode_vector2);
	LenArrayA2(pos, &size);
	faslwrite_buffer(stream, &size, IdxSize);
	for (i = 0; i < size; i++) {
		GetArrayA2(pos, i, &value);
		Return(faslwrite_value(ptr, stream, value));
	}

	return 0;
}

static int faslwrite_value_vector4(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	faslwrite_type(stream, FaslCode_vector4);
	LenArrayA4(pos, &size);
	faslwrite_buffer(stream, &size, IdxSize);
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		Return(faslwrite_value(ptr, stream, value));
	}

	return 0;
}

#ifdef LISP_ARCH_64BIT
static int faslwrite_value_vector8(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	faslwrite_type(stream, FaslCode_vector8);
	LenArrayA8(pos, &size);
	faslwrite_buffer(stream, &size, IdxSize);
	for (i = 0; i < size; i++) {
		GetArrayA8(pos, i, &value);
		Return(faslwrite_value(ptr, stream, value));
	}

	return 0;
}
#endif

_g int faslwrite_value_vector(Execute ptr, addr stream, addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			return faslwrite_value_vector2(ptr, stream, pos);

		case LISPSIZE_ARRAY4:
			return faslwrite_value_vector4(ptr, stream, pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			return faslwrite_value_vector8(ptr, stream, pos);
#endif

		default:
			fmte("Invalid vector size.", NULL);
			return 0;
	}
}

_g int faslread_value_vector2(Execute ptr, addr stream, addr *ret)
{
	addr pos, value;
	size_t size, i;

	faslread_buffer(stream, &size, IdxSize);
	vector2_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_object(ptr, stream, &value));
		SetArrayA2(pos, i, value);
	}

	return Result(ret, pos);
}

_g int faslread_value_vector4(Execute ptr, addr stream, addr *ret)
{
	addr pos, value;
	size_t size, i;

	faslread_buffer(stream, &size, IdxSize);
	vector4_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_object(ptr, stream, &value));
		SetArrayA4(pos, i, value);
	}

	return Result(ret, pos);
}

#ifdef LISP_ARCH_64BIT
_g int faslread_value_vector8(Execute ptr, addr stream, addr *ret)
{
	addr pos, value;
	size_t size, i;

	faslread_buffer(stream, &size, IdxSize);
	vector8_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_object(ptr, stream, &value));
		SetArrayA8(pos, i, value);
	}

	return Result(ret, pos);
}
#endif


/*
 *  character
 */
_g int faslwrite_value_character(Execute ptr, addr stream, addr pos)
{
	unicode value;

	CheckType(pos, LISPTYPE_CHARACTER);
	faslwrite_type(stream, FaslCode_character);
	GetCharacter(pos, &value);
	faslwrite_buffer(stream, &value, sizeoft(value));

	return 0;
}

_g int faslread_value_character(Execute ptr, addr stream, addr *ret)
{
	unicode value;

	faslread_buffer(stream, &value, sizeoft(value));
	character_heap(ret, value);

	return 0;
}


/*
 *  string
 */
_g int faslwrite_value_string(Execute ptr, addr stream, addr pos)
{
	const unicode *data;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	faslwrite_type(stream, FaslCode_string);
	strvect_posbodylen(pos, &data, &size);
	faslwrite_buffer(stream, &size, IdxSize);
	faslwrite_buffer(stream, data, sizeoft(unicode) * size);

	return 0;
}

static void faslread_string_code_local(LocalRoot local, addr stream, addr *ret)
{
	addr pos;
	unicode c;
	size_t size, i;

	faslread_type_check(stream, FaslCode_string);
	faslread_buffer(stream, &size, IdxSize);
	strvect_local(local, &pos, size);
	for (i = 0; i < size; i++) {
		faslread_buffer(stream, &c, sizeoft(unicode));
		strvect_setc(pos, i, c);
	}
	*ret = pos;
}

_g int faslread_value_string(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	unicode c;
	size_t size, i;

	faslread_buffer(stream, &size, IdxSize);
	strvect_heap(&pos, size);
	for (i = 0; i < size; i++) {
		faslread_buffer(stream, &c, sizeoft(unicode));
		strvect_setc(pos, i, c);
	}

	return Result(ret, pos);
}


/*
 *  symbol
 */
_g int faslwrite_value_symbol(Execute ptr, addr stream, addr pos)
{
	addr value;

	CheckType(pos, LISPTYPE_SYMBOL);
	faslwrite_type(stream, FaslCode_symbol);
	/* package */
	GetPackageSymbol(pos, &value);
	Return(faslwrite_value(ptr, stream, value));
	/* name */
	GetNameSymbol(pos, &value);
	Return(faslwrite_value_string(ptr, stream, value));

	return 0;
}

_g int faslread_value_symbol(Execute ptr, addr stream, addr *ret)
{
	addr package, name;

	Return(faslread_object(ptr, stream, &package));
	Return(faslread_object(ptr, stream, &name));

	if (package == Nil) {
		/* gensym */
		symbol_heap(ret);
		SetNameSymbol(*ret, name);
	}
	else {
		/* intern */
		(void)intern_package(package, name, ret);
	}

	return 0;
}


/*
 *  fixnum
 */
_g int faslwrite_value_fixnum(Execute ptr, addr stream, addr pos)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	faslwrite_type(stream, FaslCode_fixnum);
	GetFixnum(pos, &value);
	faslwrite_buffer(stream, &value, sizeoft(value));

	return 0;
}

_g int faslread_value_fixnum(Execute ptr, addr stream, addr *ret)
{
	fixnum value;

	faslread_buffer(stream, &value, sizeoft(value));
	fixnum_heap(ret, value);

	return 0;
}


/*
 *  bignum
 */
_g int faslwrite_value_bignum(Execute ptr, addr stream, addr pos)
{
	int sign;
	fixed *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	faslwrite_type(stream, FaslCode_bignum);
	/* sign */
	GetSignBignum(pos, &sign);
	write_byte_stream(stream, (byte)sign);
	/* size */
	GetSizeBignum(pos, &size);
	faslwrite_buffer(stream, &size, IdxSize);
	/* data */
	GetDataBignum(pos, &data);
	faslwrite_buffer(stream, data, sizeoft(bigtype) * size);

	return 0;
}

_g int faslread_value_bignum(Execute ptr, addr stream, addr *ret)
{
	byte sign;
	addr pos;
	fixed *data;
	size_t size;

	/* sign */
	faslread_byte(stream, &sign);
	/* size */
	faslread_buffer(stream, &size, IdxSize);
	/* data */
	bignum_heap(&pos, (sign != 0), size);
	SetSizeBignum(pos, size);
	GetDataBignum(pos, &data);
	faslread_buffer(stream, data, sizeoft(bigtype) * size);

	return Result(ret, pos);
}


/*
 *  ratio
 */
_g int faslwrite_value_ratio(Execute ptr, addr stream, addr pos)
{
	int sign;
	addr numer, denom;

	/* sign */
	CheckType(pos, LISPTYPE_RATIO);
	faslwrite_type(stream, FaslCode_ratio);
	/* sign */
	GetSignRatio(pos, &sign);
	write_byte_stream(stream, (byte)sign);
	/* numer/denom */
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	Return(faslwrite_value_bignum(ptr, stream, numer));
	Return(faslwrite_value_bignum(ptr, stream, denom));

	return 0;
}

_g int faslread_value_ratio(Execute ptr, addr stream, addr *ret)
{
	byte sign;
	addr numer, denom;

	/* sign */
	faslread_byte(stream, &sign);
	/* numer */
	faslread_type_check(stream, FaslCode_bignum);
	Return(faslread_value_bignum(ptr, stream, &numer));
	/* denom */
	faslread_type_check(stream, FaslCode_bignum);
	Return(faslread_value_bignum(ptr, stream, &denom));

	/* result */
	make_ratio_heap(ret, (sign != 0), numer, denom);
	return 0;
}


/*
 *  single-float
 */
_g int faslwrite_value_single_float(Execute ptr, addr stream, addr pos)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	faslwrite_type(stream, FaslCode_single_float);
	GetSingleFloat(pos, &value);
	faslwrite_buffer(stream, &value, sizeoft(value));

	return 0;
}

_g int faslread_value_single_float(Execute ptr, addr stream, addr *ret)
{
	single_float value;

	faslread_buffer(stream, &value, sizeoft(value));
	single_float_heap(ret, value);

	return 0;
}


/*
 *  double-float
 */
_g int faslwrite_value_double_float(Execute ptr, addr stream, addr pos)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	faslwrite_type(stream, FaslCode_double_float);
	GetDoubleFloat(pos, &value);
	faslwrite_buffer(stream, &value, sizeoft(value));

	return 0;
}

_g int faslread_value_double_float(Execute ptr, addr stream, addr *ret)
{
	double_float value;

	faslread_buffer(stream, &value, sizeoft(value));
	double_float_heap(ret, value);

	return 0;
}


/*
 *  long-float
 */
_g int faslwrite_value_long_float(Execute ptr, addr stream, addr pos)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	faslwrite_type(stream, FaslCode_long_float);
	GetLongFloat(pos, &value);
	faslwrite_buffer(stream, &value, sizeoft(value));

	return 0;
}

_g int faslread_value_long_float(Execute ptr, addr stream, addr *ret)
{
	long_float value;

	faslread_buffer(stream, &value, sizeoft(value));
	long_float_heap(ret, value);

	return 0;
}


/*
 *  complex
 */
_g int faslwrite_value_complex(Execute ptr, addr stream, addr pos)
{
	enum ComplexType type;
	addr value;

	CheckType(pos, LISPTYPE_COMPLEX);
	faslwrite_type(stream, FaslCode_complex);
	/* type */
	type = GetTypeComplex(pos);
	write_byte_stream(stream, (byte)type);
	/* real */
	GetRealComplex(pos, &value);
	Return(faslwrite_value(ptr, stream, value));
	/* imag */
	GetImagComplex(pos, &value);
	Return(faslwrite_value(ptr, stream, value));

	return 0;
}

_g int faslread_value_complex(Execute ptr, addr stream, addr *ret)
{
	byte type;
	addr real, imag, pos;

	faslread_byte(stream, &type);
	Return(faslread_object(ptr, stream, &real));
	Return(faslread_object(ptr, stream, &imag));

	make_complex_unsafe(NULL, &pos, (enum ComplexType)type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);

	return Result(ret, pos);
}


/*
 *  package
 */
_g int faslwrite_value_package(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	faslwrite_type(stream, FaslCode_package);
	getname_package(pos, &pos);
	return faslwrite_value_string(ptr, stream, pos);
}

_g int faslread_value_package(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	faslread_string_code_local(local, stream, &pos);
	find_package(pos, ret);
	rollback_local(local, stack);

	return 0;
}


/*
 *  interface
 */
_g int faslwrite_value(Execute ptr, addr stream, addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_NIL:
			return faslwrite_value_nil(ptr, stream, pos);

		case LISPTYPE_T:
			return faslwrite_value_t(ptr, stream, pos);

		case LISPTYPE_TYPE:
			return faslwrite_value_type(ptr, stream, pos);

		case LISPTYPE_CONS:
			return faslwrite_value_cons(ptr, stream, pos);

		case LISPTYPE_VECTOR:
			return faslwrite_value_vector(ptr, stream, pos);

		case LISPTYPE_CHARACTER:
			return faslwrite_value_character(ptr, stream, pos);

		case LISPTYPE_STRING:
			return faslwrite_value_string(ptr, stream, pos);

		case LISPTYPE_SYMBOL:
			return faslwrite_value_symbol(ptr, stream, pos);

		case LISPTYPE_FIXNUM:
			return faslwrite_value_fixnum(ptr, stream, pos);

		case LISPTYPE_BIGNUM:
			return faslwrite_value_bignum(ptr, stream, pos);

		case LISPTYPE_RATIO:
			return faslwrite_value_ratio(ptr, stream, pos);

		case LISPTYPE_SINGLE_FLOAT:
			return faslwrite_value_single_float(ptr, stream, pos);

		case LISPTYPE_DOUBLE_FLOAT:
			return faslwrite_value_double_float(ptr, stream, pos);

		case LISPTYPE_LONG_FLOAT:
			return faslwrite_value_long_float(ptr, stream, pos);

		case LISPTYPE_COMPLEX:
			return faslwrite_value_complex(ptr, stream, pos);

		case LISPTYPE_PACKAGE:
			return faslwrite_value_package(ptr, stream, pos);

		default:
			fmte("Invalid value ~S.", pos, NULL);
			return 0;
	}

	return 0;
}

