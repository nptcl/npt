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
#include "execute.h"
#include "hashtable.h"
#include "integer.h"
#include "load_time_value.h"
#include "make_load_form.h"
#include "package.h"
#include "package_object.h"
#include "package_symbol.h"
#include "pathname_object.h"
#include "random_state.h"
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
	Return(faslwrite_type_(stream, FaslCode_nil));
	return 0;
}

_g int faslread_value_nil(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, Nil);
}


/*
 *  t
 */
_g int faslwrite_value_t(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_T);
	Return(faslwrite_type_(stream, FaslCode_t));
	return 0;
}

_g int faslread_value_t(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, T);
}


/*
 *  cons
 */
_g int faslwrite_value_cons(Execute ptr, addr stream, addr pos)
{
	addr car, cdr;

	CheckType(pos, LISPTYPE_CONS);
	Return(faslwrite_type_(stream, FaslCode_cons));
	GetCons(pos, &car, &cdr);
	Return(faslwrite_value(ptr, stream, car));
	Return(faslwrite_value(ptr, stream, cdr));

	return 0;
}

_g int faslread_value_cons(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_value(ptr, stream, &car));
	Return(faslread_value(ptr, stream, &cdr));
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

	Return(faslwrite_type_(stream, FaslCode_vector2));
	LenArrayA2(pos, &size);
	Return(faslwrite_buffer_(stream, &size, IdxSize));
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

	Return(faslwrite_type_(stream, FaslCode_vector4));
	LenArrayA4(pos, &size);
	Return(faslwrite_buffer_(stream, &size, IdxSize));
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

	Return(faslwrite_type_(stream, FaslCode_vector8));
	LenArrayA8(pos, &size);
	Return(faslwrite_buffer_(stream, &size, IdxSize));
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
			return fmte_("Invalid vector size.", NULL);
	}
}

_g int faslread_value_vector2(Execute ptr, addr stream, addr *ret)
{
	addr pos, value;
	size_t size, i;

	Return(faslread_buffer_(stream, &size, IdxSize));
	vector2_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value(ptr, stream, &value));
		SetArrayA2(pos, i, value);
	}

	return Result(ret, pos);
}

_g int faslread_value_vector4(Execute ptr, addr stream, addr *ret)
{
	addr pos, value;
	size_t size, i;

	Return(faslread_buffer_(stream, &size, IdxSize));
	vector4_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value(ptr, stream, &value));
		SetArrayA4(pos, i, value);
	}

	return Result(ret, pos);
}

#ifdef LISP_ARCH_64BIT
_g int faslread_value_vector8(Execute ptr, addr stream, addr *ret)
{
	addr pos, value;
	size_t size, i;

	Return(faslread_buffer_(stream, &size, IdxSize));
	vector8_heap(&pos, size);
	for (i = 0; i < size; i++) {
		Return(faslread_value(ptr, stream, &value));
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
	Return(faslwrite_type_(stream, FaslCode_character));
	GetCharacter(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

_g int faslread_value_character(Execute ptr, addr stream, addr *ret)
{
	unicode value;

	Return(faslread_buffer_(stream, &value, sizeoft(value)));
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
	Return(faslwrite_type_(stream, FaslCode_string));
	strvect_posbodylen(pos, &data, &size);
	/* write */
	Return(faslwrite_buffer_(stream, &size, IdxSize));
	Return(faslwrite_buffer_(stream, data, sizeoft(unicode) * size));

	return 0;
}

static int faslread_string_code_local_(LocalRoot local, addr stream, addr *ret)
{
	addr pos;
	unicode *data;
	size_t size;

	Return(faslread_type_check_(stream, FaslCode_string));
	Return(faslread_buffer_(stream, &size, IdxSize));

	strvect_local(local, &pos, size);
	GetStringUnicode(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(unicode) * size));

	return Result(ret, pos);
}

_g int faslread_value_string(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	unicode *data;
	size_t size;

	Return(faslread_buffer_(stream, &size, IdxSize));

	strvect_heap(&pos, size);
	GetStringUnicode(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(unicode) * size));

	return Result(ret, pos);
}


/*
 *  hashtable
 */
_g int faslwrite_value_hashtable(Execute ptr, addr stream, addr pos)
{
	addr loop, key, value;
	struct StructHashtable *str;
	size_t i;

	CheckType(pos, LISPTYPE_HASHTABLE);
	Return(faslwrite_type_(stream, FaslCode_hashtable));
	/* struct */
	str = PtrStructHashtable(pos);
	Return(faslwrite_buffer_(stream, str, sizeoft(struct StructHashtable)));
	/* interator */
	hash_iterator_heap(&loop, pos);
	for (i = 0; next_hash_iterator(loop, &key, &value); i++) {
		Return(faslwrite_value(ptr, stream, key));
		Return(faslwrite_value(ptr, stream, value));
	}
	Check(str->count < i, "count error.");

	return 0;
}

_g int faslread_value_hashtable(Execute ptr, addr stream, addr *ret)
{
	addr pos, key, value, cons;
	struct StructHashtable data, *str;
	size_t size, i;

	/* hashtable */
	Return(faslread_buffer_(stream, &data, sizeoft(struct StructHashtable)));
	data.count = 0;
	hashtable_size_heap(&pos, data.size);
	str = PtrStructHashtable(pos);
	*str = data;

	/* iterator */
	size = data.count;
	for (i = 0; i < size; i++) {
		Return(faslread_value(ptr, stream, &key));
		Return(faslread_value(ptr, stream, &value));
		Return(intern_hashheap_(pos, key, &cons));
		SetCdr(cons, value);
	}

	return Result(ret, pos);
}


/*
 *  gensym
 */
static int faslwrite_value_gensym(Execute ptr, addr stream, addr pos)
{
	int check;
	addr symbol, table, cons, value;

	Check(! gensymp(pos), "type error");
	Return(faslwrite_type_(stream, FaslCode_gensym));

	/* gensym table */
	GetConst(SYSTEM_COMPILE_GENSYM, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &table));
	Return(internp_hashheap_(table, pos, &cons, &check));
	if (check) {
		GetCdr(cons, &value);
	}
	else {
		/* increment table */
		GetConst(SYSTEM_COMPILE_GENSYM_INDEX, &symbol);
		Return(getspecialcheck_local_(ptr, symbol, &value));
		Return(oneplus_integer_common_(ptr->local, value, &value));
		setspecial_local(ptr, symbol, value);
		SetCdr(cons, value);
	}

	/* index */
	Return(faslwrite_value(ptr, stream, value));
	/* name */
	GetNameSymbol(pos, &value);
	Return(faslwrite_value_string(ptr, stream, value));

	return 0;
}

_g int faslread_value_gensym(Execute ptr, addr stream, addr *ret)
{
	int check;
	addr index, name, symbol, table, cons, pos;

	/* raed file */
	Return(faslread_value(ptr, stream, &index));
	Return(faslread_value(ptr, stream, &name));

	/* cache hit */
	GetConst(SYSTEM_COMPILE_GENSYM, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &table));
	Return(internp_hashheap_(table, index, &cons, &check));
	if (check) {
		GetCdr(cons, ret);
		return 0;
	}

	/* add cache */
	symbol_heap(&pos);
	SetNameSymbol(pos, name);
	SetCdr(cons, pos);

	return Result(ret, pos);
}


/*
 *  symbol
 */
_g int faslwrite_value_symbol(Execute ptr, addr stream, addr pos)
{
	addr value;

	CheckType(pos, LISPTYPE_SYMBOL);
	/* gensym */
	if (gensymp(pos))
		return faslwrite_value_gensym(ptr, stream, pos);

	/* symbol */
	Return(faslwrite_type_(stream, FaslCode_symbol));
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

	Return(faslread_value(ptr, stream, &package));
	Return(faslread_value(ptr, stream, &name));
	Check(package == Nil, "package error");
	Return(intern_package_(package, name, ret, NULL));

	return 0;
}


/*
 *  fixnum
 */
_g int faslwrite_value_fixnum(Execute ptr, addr stream, addr pos)
{
	fixnum value;

	CheckType(pos, LISPTYPE_FIXNUM);
	Return(faslwrite_type_(stream, FaslCode_fixnum));
	GetFixnum(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

_g int faslread_value_fixnum(Execute ptr, addr stream, addr *ret)
{
	fixnum value;

	Return(faslread_buffer_(stream, &value, sizeoft(value)));
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
	Return(faslwrite_type_(stream, FaslCode_bignum));
	/* sign */
	GetSignBignum(pos, &sign);
	Return(faslwrite_byte_(stream, (byte)sign));
	/* size */
	GetSizeBignum(pos, &size);
	Return(faslwrite_buffer_(stream, &size, IdxSize));
	/* data */
	GetDataBignum(pos, &data);
	Return(faslwrite_buffer_(stream, data, sizeoft(bigtype) * size));

	return 0;
}

_g int faslread_value_bignum(Execute ptr, addr stream, addr *ret)
{
	byte sign;
	addr pos;
	fixed *data;
	size_t size;

	/* sign */
	Return(faslread_byte_(stream, &sign));
	/* size */
	Return(faslread_buffer_(stream, &size, IdxSize));
	/* data */
	bignum_heap(&pos, (sign != 0), size);
	SetSizeBignum(pos, size);
	GetDataBignum(pos, &data);
	Return(faslread_buffer_(stream, data, sizeoft(bigtype) * size));

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
	Return(faslwrite_type_(stream, FaslCode_ratio));
	/* sign */
	GetSignRatio(pos, &sign);
	Return(faslwrite_byte_(stream, (byte)sign));
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
	Return(faslread_byte_(stream, &sign));
	/* numer */
	Return(faslread_type_check_(stream, FaslCode_bignum));
	Return(faslread_value_bignum(ptr, stream, &numer));
	/* denom */
	Return(faslread_type_check_(stream, FaslCode_bignum));
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
	Return(faslwrite_type_(stream, FaslCode_single_float));
	GetSingleFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

_g int faslread_value_single_float(Execute ptr, addr stream, addr *ret)
{
	single_float value;

	Return(faslread_buffer_(stream, &value, sizeoft(value)));
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
	Return(faslwrite_type_(stream, FaslCode_double_float));
	GetDoubleFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

_g int faslread_value_double_float(Execute ptr, addr stream, addr *ret)
{
	double_float value;

	Return(faslread_buffer_(stream, &value, sizeoft(value)));
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
	Return(faslwrite_type_(stream, FaslCode_long_float));
	GetLongFloat(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

_g int faslread_value_long_float(Execute ptr, addr stream, addr *ret)
{
	long_float value;

	Return(faslread_buffer_(stream, &value, sizeoft(value)));
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
	Return(faslwrite_type_(stream, FaslCode_complex));
	/* type */
	type = GetTypeComplex(pos);
	Return(faslwrite_byte_(stream, (byte)type));
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

	Return(faslread_byte_(stream, &type));
	Return(faslread_value(ptr, stream, &real));
	Return(faslread_value(ptr, stream, &imag));

	make_complex_unsafe(NULL, &pos, (enum ComplexType)type);
	SetRealComplex(pos, real);
	SetImagComplex(pos, imag);

	return Result(ret, pos);
}


/*
 *  callname
 */
_g int faslwrite_value_callname(Execute ptr, addr stream, addr pos)
{
	CallNameType type;
	addr value;

	CheckType(pos, LISPTYPE_CALLNAME);
	Return(faslwrite_type_(stream, FaslCode_callname));
	GetCallNameType(pos, &type);
	GetCallName(pos, &value);
	Return(faslwrite_byte_(stream, (byte)type));
	return faslwrite_value(ptr, stream, value);
}

_g int faslread_value_callname(Execute ptr, addr stream, addr *ret)
{
	byte type;
	addr value;

	Return(faslread_byte_(stream, &type));
	Return(faslread_value(ptr, stream, &value));
	callname_heap(ret, value, (CallNameType)type);

	return 0;
}


/*
 *  index
 */
_g int faslwrite_value_index(Execute ptr, addr stream, addr pos)
{
	size_t value;

	CheckType(pos, LISPTYPE_INDEX);
	Return(faslwrite_type_(stream, FaslCode_index));
	GetIndex(pos, &value);
	Return(faslwrite_buffer_(stream, &value, sizeoft(value)));

	return 0;
}

_g int faslread_value_index(Execute ptr, addr stream, addr *ret)
{
	size_t value;

	Return(faslread_buffer_(stream, &value, sizeoft(value)));
	index_heap(ret, value);

	return 0;
}


/*
 *  package
 */
_g int faslwrite_value_package(Execute ptr, addr stream, addr pos)
{
	CheckType(pos, LISPTYPE_PACKAGE);
	Return(faslwrite_type_(stream, FaslCode_package));
	getname_package_unsafe(pos, &pos);
	return faslwrite_value_string(ptr, stream, pos);
}

_g int faslread_value_package(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	Return(faslread_string_code_local_(local, stream, &pos));
	Return(find_package_(pos, ret));
	rollback_local(local, stack);

	return 0;
}


/*
 *  random-state
 */
_g int faslwrite_value_random_state(Execute ptr, addr stream, addr pos)
{
	struct random_state *str;

	CheckType(pos, LISPTYPE_RANDOM_STATE);
	Return(faslwrite_type_(stream, FaslCode_random_state));
	str = struct_random_state(pos);
	Return(faslwrite_buffer_(stream, str, sizeoft(struct random_state)));

	return 0;
}

_g int faslread_value_random_state(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	struct random_state *str;

	random_state_heap(&pos);
	str = struct_random_state(pos);
	Return(faslread_buffer_(stream, str, sizeoft(struct random_state)));

	return Result(ret, pos);
}


/*
 *  pathname
 */
_g int faslwrite_value_pathname(Execute ptr, addr stream, addr pos)
{
	int type;
	addr value;

	CheckType(pos, LISPTYPE_PATHNAME);
	Return(faslwrite_type_(stream, FaslCode_pathname));
	/* type */
	GetLogicalPathname(pos, &type);
	Return(faslwrite_byte_(stream, (byte)type));
	/* array */
	GetHostPathname(pos, &value);
	Return(faslwrite_value(ptr, stream, value));
	GetDevicePathname(pos, &value);
	Return(faslwrite_value(ptr, stream, value));
	GetDirectoryPathname(pos, &value);
	Return(faslwrite_value(ptr, stream, value));
	GetNamePathname(pos, &value);
	Return(faslwrite_value(ptr, stream, value));
	GetTypePathname(pos, &value);
	Return(faslwrite_value(ptr, stream, value));
	GetVersionPathname(pos, &value);
	Return(faslwrite_value(ptr, stream, value));

	return 0;
}

_g int faslread_value_pathname(Execute ptr, addr stream, addr *ret)
{
	byte type;
	addr pos, value;

	/* type */
	Return(faslread_byte_(stream, &type));
	make_pathname_alloc(NULL, &pos, (int)type);
	/* array */
	Return(faslread_value(ptr, stream, &value));
	SetHostPathname(pos, value);
	Return(faslread_value(ptr, stream, &value));
	SetDevicePathname(pos, value);
	Return(faslread_value(ptr, stream, &value));
	SetDirectoryPathname(pos, value);
	Return(faslread_value(ptr, stream, &value));
	SetNamePathname(pos, value);
	Return(faslread_value(ptr, stream, &value));
	SetTypePathname(pos, value);
	Return(faslread_value(ptr, stream, &value));
	SetVersionPathname(pos, value);

	return Result(ret, pos);
}


/*
 *  bitvector
 */
_g int faslwrite_value_bitvector(Execute ptr, addr stream, addr pos)
{
	struct bitmemory_struct *str;

	CheckType(pos, LISPTYPE_BITVECTOR);
	Return(faslwrite_type_(stream, FaslCode_bitvector));
	str = BitMemoryStruct(pos);
	Return(faslwrite_buffer_(stream, str, sizeoft(struct bitmemory_struct)));
	Return(faslwrite_buffer_(stream, str->data, sizeoft(fixed) * str->fixedsize));

	return 0;
}

_g int faslread_value_bitvector(Execute ptr, addr stream, addr *ret)
{
	addr pos;
	struct bitmemory_struct *str, value;

	Return(faslread_buffer_(stream, &value, sizeoft(struct bitmemory_struct)));
	bitmemory_unsafe(NULL, &pos, value.bitsize);
	str = BitMemoryStruct(pos);
	*str = value;
	Return(faslread_buffer_(stream, str->data, sizeoft(fixed) * str->fixedsize));

	return Result(ret, pos);
}


/*
 *  load-time-value
 */
_g int faslwrite_value_load_time_value(Execute ptr, addr stream, addr pos)
{
	addr index;

	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	Return(faslwrite_type_(stream, FaslCode_load_time_value));
	/* index */
	Return(get_write_make_load_form_(ptr, pos, &index));
	Return(faslwrite_value(ptr, stream, index));
	/* value */
	get_load_time_value_heap(pos, &pos);
	return faslwrite_value(ptr, stream, pos);
}

_g int faslread_value_load_time_value(Execute ptr, addr stream, addr *ret)
{
	addr pos, value;

	Return(faslread_value(ptr, stream, &pos));
	Return(get_read_make_load_form_(ptr, pos, &pos));
	Return(faslread_value(ptr, stream, &value));
	set_load_time_value_heap(pos, value);

	return Result(ret, pos);
}

