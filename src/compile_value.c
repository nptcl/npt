#include "bignum_data.h"
#include "bignum_object.h"
#include "bit.h"
#include "callname.h"
#include "character.h"
#include "cmpl.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_type.h"
#include "compile_value.h"
#include "compile_write.h"
#include "condition.h"
#include "define.h"
#include "eval_object.h"
#include "execute.h"
#include "hashtable.h"
#include "integer.h"
#include "load_code.h"
#include "load_object.h"
#include "load_time_value.h"
#include "package.h"
#include "package_intern.h"
#include "package_object.h"
#include "paper.h"
#include "pathname_object.h"
#include "quote.h"
#include "random_state.h"
#include "ratio.h"
#include "scope_object.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  nil
 */
int faslwrite_value_nil_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_NIL);
	Return(faslwrite_type_(stream, FaslCode_nil));
	return 0;
}

int faslread_value_nil_(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, Nil);
}


/*
 *  t
 */
int faslwrite_value_t_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_T);
	Return(faslwrite_type_(stream, FaslCode_t));
	return 0;
}

int faslread_value_t_(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, T);
}


/*
 *  clos
 */
int faslwrite_value_clos_(Execute ptr, addr stream, addr pos)
{
	size_t index;

	CheckType(pos, LISPTYPE_CLOS);
	Return(get_index_load_table_(ptr, pos, &index));
	Return(faslwrite_type_status_(stream, pos, FaslCode_clos));
	Return(faslwrite_size_(stream, index));

	return 0;
}

int faslread_value_clos_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t index;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &index));
	Return(execute_load_get_(ptr, index, &pos));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  cons
 */
int faslwrite_value_cons_(Execute ptr, addr stream, addr pos)
{
	addr car, cdr;

	CheckType(pos, LISPTYPE_CONS);
	Return(faslwrite_type_status_(stream, pos, FaslCode_cons));
	GetCons(pos, &car, &cdr);
	Return(faslwrite_value_(ptr, stream, car));
	Return(faslwrite_value_(ptr, stream, cdr));

	return 0;
}

int faslread_value_cons_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, car, cdr;

	Return(faslread_status_(stream, &status));
	Return(faslread_value_(ptr, stream, &car));
	Return(faslread_value_(ptr, stream, &cdr));
	cons_heap(&pos, car, cdr);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  vector
 */
static int faslwrite_value_vector2_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	Return(faslwrite_type_status_(stream, pos, FaslCode_vector2));
	LenArrayA2(pos, &size);
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		GetArrayA2(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

static int faslwrite_value_vector4_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	Return(faslwrite_type_status_(stream, pos, FaslCode_vector4));
	LenArrayA4(pos, &size);
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		GetArrayA4(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}

#ifdef LISP_ARCH_64BIT
static int faslwrite_value_vector8_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t size, i;

	Return(faslwrite_type_status_(stream, pos, FaslCode_vector8));
	LenArrayA8(pos, &size);
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		GetArrayA8(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}

	return 0;
}
#endif

int faslwrite_value_vector_(Execute ptr, addr stream, addr pos)
{
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
			return faslwrite_value_vector2_(ptr, stream, pos);

		case LISPSIZE_ARRAY4:
			return faslwrite_value_vector4_(ptr, stream, pos);

#ifdef LISP_ARCH_64BIT
		case LISPSIZE_ARRAY8:
			return faslwrite_value_vector8_(ptr, stream, pos);
#endif

		default:
			return fmte_("Invalid vector size.", NULL);
	}
}

int faslread_value_vector2_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));
	vector2_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayA2(pos, i, value);
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

int faslread_value_vector4_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));
	vector4_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayA4(pos, i, value);
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

#ifdef LISP_ARCH_64BIT
int faslread_value_vector8_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));
	vector8_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &value));
		SetArrayA8(pos, i, value);
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}
#endif


/*
 *  character
 */
int faslwrite_value_character_(Execute ptr, addr stream, addr pos)
{
	unicode value;

	CheckType(pos, LISPTYPE_CHARACTER);
	GetCharacter(pos, &value);
	if (value < 0x80) {
		Return(faslwrite_type_status_(stream, pos, FaslCode_character7));
		Return(faslwrite_byte_(stream, (byte)value));
	}
	else {
		Return(faslwrite_type_status_(stream, pos, FaslCode_character));
		Return(faslwrite_buffer_(stream, &value, sizeoft(value)));
	}

	return 0;
}

int faslread_value_character_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	unicode value;
	addr pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	character_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

int faslread_value_character7_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte value;
	addr pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &value));
	character_heap(&pos, (unicode)value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  string
 */
static int faslwrite_value_string7_p(Execute ptr, addr pos)
{
	const unicode *data;
	size_t size, i;

	CheckType(pos, LISPTYPE_STRING);
	strvect_posbodylen(pos, &data, &size);
	for (i = 0; i < size; i++) {
		if (0x80 <= data[i])
			return 0;
	}

	return 1;
}

static int faslwrite_value_string7_(Execute ptr, addr stream, addr pos)
{
	const unicode *data;
	size_t size, i;

	CheckType(pos, LISPTYPE_STRING);
	Return(faslwrite_type_status_(stream, pos, FaslCode_string7));
	strvect_posbodylen(pos, &data, &size);
	/* write */
	Return(faslwrite_size_(stream, size));
	for (i = 0; i < size; i++) {
		Return(faslwrite_byte_(stream, (byte)data[i]));
	}

	return 0;
}

static int faslwrite_value_string32_(Execute ptr, addr stream, addr pos)
{
	const unicode *data;
	size_t size;

	CheckType(pos, LISPTYPE_STRING);
	Return(faslwrite_type_status_(stream, pos, FaslCode_string));
	strvect_posbodylen(pos, &data, &size);
	/* write */
	Return(faslwrite_size_(stream, size));
	Return(faslwrite_buffer_(stream, data, sizeoft(unicode) * size));

	return 0;
}

int faslwrite_value_string_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_STRING);
	if (faslwrite_value_string7_p(ptr, pos))
		return faslwrite_value_string7_(ptr, stream, pos);
	else
		return faslwrite_value_string32_(ptr, stream, pos);
}

int faslread_value_string_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	unicode *data;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_heap(&pos, size);
	GetStringUnicode(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(unicode) * size));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

int faslread_value_string7_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte c;
	addr pos;
	unicode *data;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_heap(&pos, size);
	GetStringUnicode(pos, &data);
	for (i = 0; i < size; i++) {
		Return(faslread_byte_(stream, &c));
		data[i] = (unicode)c;
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

static int faslread_string32_local_(LocalRoot local, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	unicode *data;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(unicode) * size));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

static int faslread_string7_local_(LocalRoot local, addr stream, addr *ret)
{
	FaslStatus status;
	byte c;
	addr pos;
	unicode *data;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &data);
	for (i = 0; i < size; i++) {
		Return(faslread_byte_(stream, &c));
		data[i] = (unicode)c;
	}
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

static int faslread_string_local_(LocalRoot local, addr stream, addr *ret)
{
	enum FaslCode code;

	Return(faslread_type_(stream, &code));
	switch (code) {
		case FaslCode_string:
			return faslread_string32_local_(local, stream, ret);

		case FaslCode_string7:
			return faslread_string7_local_(local, stream, ret);

		default:
			*ret = Nil;
			return fmte_("Invalid faslcode.", NULL);
	}
}


/*
 *  hashtable
 */
static int faslwrite_value_hashtable_struct_(addr stream,
		const struct StructHashtable *str)
{
	double_float dvalue;

	Return(faslwrite_byte_(stream, (byte)str->resize_float_p));
	Return(faslwrite_byte_(stream, (byte)str->expand_p));
	Return(faslwrite_byte_(stream, (byte)str->test));
	Return(faslwrite_size_(stream, str->count));
	Return(faslwrite_size_(stream, str->size));
	Return(faslwrite_size_(stream, str->limit));
	Return(faslwrite_size_(stream, str->resize_integer));
	dvalue = str->resize_float;
	Return(faslwrite_buffer_(stream, &dvalue, sizeoft(dvalue)));
	dvalue = str->threshold;
	Return(faslwrite_buffer_(stream, &dvalue, sizeoft(dvalue)));

	return 0;
}

static int faslread_value_hashtable_struct_(addr stream,
		struct StructHashtable *str)
{
	byte v;
	size_t size;
	double_float dvalue;

	Return(faslread_byte_(stream, &v));
	str->resize_float_p = (v != 0);
	Return(faslread_byte_(stream, &v));
	str->expand_p = (v != 0);
	Return(faslread_byte_(stream, &v));
	str->test = (enum HASHTABLE_TEST)v;
	Return(faslread_size_(stream, &size));
	str->count = size;
	Return(faslread_size_(stream, &size));
	str->size = size;
	Return(faslread_size_(stream, &size));
	str->limit = size;
	Return(faslread_size_(stream, &size));
	str->resize_integer = size;
	Return(faslread_buffer_(stream, &dvalue, sizeoft(dvalue)));
	str->resize_float = dvalue;
	Return(faslread_buffer_(stream, &dvalue, sizeoft(dvalue)));
	str->threshold = dvalue;

	return 0;
}

int faslwrite_value_hashtable_(Execute ptr, addr stream, addr pos)
{
	addr loop, key, value;
	struct StructHashtable *str;
	size_t i;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(faslwrite_type_status_(stream, pos, FaslCode_hashtable));
	/* struct */
	str = PtrStructHashtable(pos);
	Return(faslwrite_value_hashtable_struct_(stream, str));
	/* interator */
	hash_iterator_heap(&loop, pos);
	for (i = 0; next_hash_iterator(loop, &key, &value); i++) {
		Return(faslwrite_value_(ptr, stream, key));
		Return(faslwrite_value_(ptr, stream, value));
	}
	Check(str->count < i, "count error.");

	return 0;
}

int faslread_value_hashtable_(Execute ptr, addr stream, addr *ret)
{
	addr pos, key, value, cons;
	struct StructHashtable data, *str;
	size_t size, i;

	/* hashtable */
	Return(faslread_value_hashtable_struct_(stream, &data));
	data.count = 0;
	hashtable_size_heap(&pos, data.size);
	str = PtrStructHashtable(pos);
	*str = data;

	/* iterator */
	size = data.count;
	for (i = 0; i < size; i++) {
		Return(faslread_value_(ptr, stream, &key));
		Return(faslread_value_(ptr, stream, &value));
		Return(intern_hashheap_(pos, key, &cons));
		SetCdr(cons, value);
	}

	return Result(ret, pos);
}


/*
 *  gensym
 */
static int faslwrite_value_gensym_(Execute ptr, addr stream, addr pos)
{
	size_t index;

	Check(! gensymp(pos), "type error");
	Return(get_index_load_table_(ptr, pos, &index));
	Return(faslwrite_type_status_(stream, pos, FaslCode_gensym));
	Return(faslwrite_size_(stream, index));

	return 0;
}

int faslread_value_gensym_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t index;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &index));
	Return(execute_load_get_(ptr, index, &pos));
	faslread_status_update(pos, status);
	Check(! gensymp(pos), "type error");

	return Result(ret, pos);
}


/*
 *  symbol
 */
int faslwrite_value_symbol_(Execute ptr, addr stream, addr pos)
{
	addr value;

	CheckType(pos, LISPTYPE_SYMBOL);
	/* gensym */
	if (gensymp(pos))
		return faslwrite_value_gensym_(ptr, stream, pos);
	/* symbol */
	Return(faslwrite_type_(stream, FaslCode_symbol));
	/* package */
	GetPackageSymbol(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	/* name */
	GetNameSymbol(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_symbol_(Execute ptr, addr stream, addr *ret)
{
	addr package, name;

	Return(faslread_value_(ptr, stream, &package));
	Return(faslread_value_(ptr, stream, &name));
	Check(package == Nil, "package error");
	Return(intern_package_(package, name, ret, NULL));

	return 0;
}


/*
 *  fixnum
 */
int faslwrite_value_fixnum_(Execute ptr, addr stream, addr pos)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	Return(faslwrite_type_status_(stream, pos, FaslCode_fixnum));
	GetFixnum(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_fixnum_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	fixnum value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	fixnum_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  bignum
 */
int faslwrite_value_bignum_(Execute ptr, addr stream, addr pos)
{
	int sign;
	fixed *data;
	size_t size;

	CheckType(pos, LISPTYPE_BIGNUM);
	Return(faslwrite_type_status_(stream, pos, FaslCode_bignum));
	/* sign */
	GetSignBignum(pos, &sign);
	Return(faslwrite_byte_(stream, (byte)sign));
	/* size */
	GetSizeBignum(pos, &size);
	Return(faslwrite_size_(stream, size));
	/* data */
	GetDataBignum(pos, &data);
	Return(faslwrite_buffer_(stream, data, sizeoft(fixed) * size));

	return 0;
}

int faslread_value_bignum_(Execute ptr, addr stream, addr *ret)
{
	byte sign;
	FaslStatus status;
	addr pos;
	fixed *data;
	size_t size;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &sign));
	Return(faslread_size_(stream, &size));
	/* data */
	bignum_heap(&pos, (sign != 0), size);
	SetSizeBignum(pos, size);
	GetDataBignum(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(fixed) * size));
	/* result */
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  ratio
 */
int faslwrite_value_ratio_(Execute ptr, addr stream, addr pos)
{
	int sign;
	addr numer, denom;

	CheckType(pos, LISPTYPE_RATIO);
	Return(faslwrite_type_status_(stream, pos, FaslCode_ratio));
	/* sign */
	GetSignRatio(pos, &sign);
	Return(faslwrite_byte_(stream, (byte)sign));
	/* numer/denom */
	GetNumerRatio(pos, &numer);
	GetDenomRatio(pos, &denom);
	Return(faslwrite_value_bignum_(ptr, stream, numer));
	Return(faslwrite_value_bignum_(ptr, stream, denom));

	return 0;
}

int faslread_value_ratio_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte sign;
	addr pos, numer, denom;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &sign));
	/* numer */
	Return(faslread_type_check_(stream, FaslCode_bignum));
	Return(faslread_value_bignum_(ptr, stream, &numer));
	/* denom */
	Return(faslread_type_check_(stream, FaslCode_bignum));
	Return(faslread_value_bignum_(ptr, stream, &denom));
	/* result */
	make_ratio_heap(&pos, (sign != 0), numer, denom);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  single-float
 */
int faslwrite_value_single_float_(Execute ptr, addr stream, addr pos)
{
	single_float value;

	CheckType(pos, LISPTYPE_SINGLE_FLOAT);
	Return(faslwrite_type_status_(stream, pos, FaslCode_single_float));
	GetSingleFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_single_float_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	single_float value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	single_float_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  double-float
 */
int faslwrite_value_double_float_(Execute ptr, addr stream, addr pos)
{
	double_float value;

	CheckType(pos, LISPTYPE_DOUBLE_FLOAT);
	Return(faslwrite_type_status_(stream, pos, FaslCode_double_float));
	GetDoubleFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_double_float_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	double_float value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	double_float_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  long-float
 */
int faslwrite_value_long_float_(Execute ptr, addr stream, addr pos)
{
	long_float value;

	CheckType(pos, LISPTYPE_LONG_FLOAT);
	Return(faslwrite_type_status_(stream, pos, FaslCode_long_float));
	GetLongFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

int faslread_value_long_float_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	long_float value;

	Return(faslread_status_(stream, &status));
	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	long_float_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  complex
 */
int faslwrite_value_complex_(Execute ptr, addr stream, addr pos)
{
	enum ComplexType type;
	addr value;

	CheckType(pos, LISPTYPE_COMPLEX);
	Return(faslwrite_type_status_(stream, pos, FaslCode_complex));
	/* type */
	type = GetTypeComplex(pos);
	Return(faslwrite_byte_(stream, (byte)type));
	/* real */
	GetRealComplex(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	/* imag */
	GetImagComplex(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_complex_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr real, imag, pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	Return(faslread_value_(ptr, stream, &real));
	Return(faslread_value_(ptr, stream, &imag));

	make_complex_unsafe(NULL, &pos, (enum ComplexType)type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  callname
 */
int faslwrite_value_callname_(Execute ptr, addr stream, addr pos)
{
	CallNameType type;
	addr value;

	CheckType(pos, LISPTYPE_CALLNAME);
	Return(faslwrite_type_status_(stream, pos, FaslCode_callname));
	GetCallNameType(pos, &type);
	GetCallName(pos, &value);
	Return(faslwrite_byte_(stream, (byte)type));
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_callname_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr value, pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	Return(faslread_value_(ptr, stream, &value));
	callname_heap(&pos, value, (CallNameType)type);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  index
 */
int faslwrite_value_index_(Execute ptr, addr stream, addr pos)
{
	size_t value;

	CheckType(pos, LISPTYPE_INDEX);
	Return(faslwrite_type_status_(stream, pos, FaslCode_index));
	GetIndex(pos, &value);
	Return(faslwrite_size_(stream, value));

	return 0;
}

int faslread_value_index_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t value;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &value));
	index_heap(&pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  package
 */
int faslwrite_value_package_(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	Return(faslwrite_type_(stream, FaslCode_package));
	getname_package_unsafe(pos, &pos);
	Return(strvect_value_heap_(&pos, pos));
	return faslwrite_value_(ptr, stream, pos);
}

int faslread_value_package_(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(faslread_string_local_(local, stream, &pos));
	Return(find_package_(pos, ret));
	rollback_local(local, stack);

	return 0;
}


/*
 *  random-state
 */
int faslwrite_value_random_state_(Execute ptr, addr stream, addr pos)
{
	struct random_state *str;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	Return(faslwrite_type_status_(stream, pos, FaslCode_random_state));
	str = struct_random_state(pos);
	Return(faslwrite_buffer_(stream, str, sizeoft(struct random_state)));

	return 0;
}

int faslread_value_random_state_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	struct random_state *str;

	Return(faslread_status_(stream, &status));
	random_state_heap(&pos);
	str = struct_random_state(pos);
	Return(faslread_buffer_(stream, str, sizeoft(struct random_state)));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  pathname
 */
int faslwrite_value_pathname_(Execute ptr, addr stream, addr pos)
{
	int type;
	addr value;

	CheckType(pos, LISPTYPE_PATHNAME);
	Return(faslwrite_type_status_(stream, pos, FaslCode_pathname));
	/* type */
	GetLogicalPathname(pos, &type);
	Return(faslwrite_byte_(stream, (byte)type));
	/* array */
	GetHostPathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetDevicePathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetDirectoryPathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetNamePathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetTypePathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));
	GetVersionPathname(pos, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_pathname_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr pos, value;

	/* type */
	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	make_pathname_alloc(NULL, &pos, (int)type);
	/* array */
	Return(faslread_value_(ptr, stream, &value));
	SetHostPathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetDevicePathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetDirectoryPathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetNamePathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetTypePathname(pos, value);
	Return(faslread_value_(ptr, stream, &value));
	SetVersionPathname(pos, value);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  quote
 */
int faslwrite_value_quote_(Execute ptr, addr stream, addr pos)
{
	enum QuoteType type;
	addr value;

	CheckType(pos, LISPTYPE_QUOTE);
	Return(faslwrite_type_status_(stream, pos, FaslCode_quote));
	/* type */
	GetQuoteType(pos, &type);
	Return(faslwrite_byte_(stream, (byte)type));
	/* value */
	GetQuote(pos, QuoteIndex_Value, &value);
	Return(faslwrite_value_(ptr, stream, value));
	/* print */
	GetQuote(pos, QuoteIndex_Print, &value);
	Return(faslwrite_value_(ptr, stream, value));

	return 0;
}

int faslread_value_quote_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	byte type;
	addr value, print, pos;

	Return(faslread_status_(stream, &status));
	Return(faslread_byte_(stream, &type));
	Return(faslread_value_(ptr, stream, &value));
	Return(faslread_value_(ptr, stream, &print));
	quote2_heap(&pos, (enum QuoteType)type, value, print);
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  bitvector
 */
int faslwrite_value_bitvector_(Execute ptr, addr stream, addr pos)
{
	struct bitmemory_struct *str;

	CheckType(pos, LISPTYPE_BITVECTOR);
	Return(faslwrite_type_status_(stream, pos, FaslCode_bitvector));
	str = BitMemoryStruct(pos);
	Return(faslwrite_size_(stream, str->bitsize));
	Return(faslwrite_size_(stream, str->fixedsize));
	Return(faslwrite_buffer_(stream, str->data, sizeoft(fixed) * str->fixedsize));

	return 0;
}

int faslread_value_bitvector_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	struct bitmemory_struct *str, value;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &(value.bitsize)));
	Return(faslread_size_(stream, &(value.fixedsize)));
	bitmemory_unsafe(NULL, &pos, value.bitsize);
	str = BitMemoryStruct(pos);
	*str = value;
	Return(faslread_buffer_(stream, str->data, sizeoft(fixed) * str->fixedsize));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  load
 */
int faslwrite_value_load_time_value_(Execute ptr, addr stream, addr pos)
{
	size_t index;

	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	get_index_load_time_value(pos, &index);
	Return(faslwrite_type_status_(stream, pos, FaslCode_load));
	Return(faslwrite_size_(stream, index));

	return 0;
}

int faslread_value_load_time_value_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos;
	size_t index;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &index));
	Return(execute_load_get_(ptr, index, &pos));
	faslread_status_update(pos, status);

	return Result(ret, pos);
}


/*
 *  paper
 */
int faslwrite_value_paper_(Execute ptr, addr stream, addr pos)
{
	addr value;
	size_t array, body, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &array);
	paper_len_body(pos, &body);
	Return(faslwrite_type_status_(stream, pos, FaslCode_paper));
	Return(faslwrite_size_(stream, array));
	Return(faslwrite_size_(stream, body));
	/* array */
	for (i = 0; i < array; i++) {
		paper_get_array(pos, i, &value);
		Return(faslwrite_value_(ptr, stream, value));
	}
	/* body */
	if (body) {
		posbody(pos, &value);
		Return(faslwrite_buffer_(stream, (const void *)value, body));
	}

	return 0;
}

int faslread_value_paper_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr pos, value;
	size_t array, body, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &array));
	Return(faslread_size_(stream, &body));
	Return(paper_arraybody_heap_(&pos, array, body));
	/* array */
	for (i = 0; i < array; i++) {
		Return(faslread_value_(ptr, stream, &value));
		paper_set_array(pos, i, value);
	}
	/* body */
	if (body) {
		posbody(pos, &value);
		Return(faslwrite_buffer_(stream, (void *)value, body));
	}
	/* result */
	faslread_status_update(pos, status);

	return Result(ret, pos);
}

