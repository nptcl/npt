#include <memory.h>
#include <string.h>
#include "character.h"
#include "code_init.h"
#include "code_object.h"
#include "compile_array.h"
#include "compile_stream.h"
#include "compile_type.h"
#include "compile_typedef.h"
#include "compile_value.h"
#include "compile_write.h"
#include "condition.h"
#include "pointer.h"
#include "stream.h"

/*
 *  write function
 */
static int faslwrite_magic_(addr stream)
{
	char a[16];

	memset(a, 0, 8);
	strncpy(a, LISPNAME, 8);
	Return(faslwrite_buffer_(stream, "\0\0\0\0FASL", 8));
	Return(faslwrite_buffer_(stream, a, 8));

	return 0;
}

static int faslwrite_byte16_(addr stream, uint16_t v)
{
	return faslwrite_buffer_(stream, &v, sizeoft(v));
}

int faslwrite_header_(addr stream)
{
	char buffer[32];

	/* 0: magic number */
	Return(faslwrite_magic_(stream));
	/* 16: endian */
	Return(faslwrite_byte16_(stream, 1));
	/* 18: version */
	Return(faslwrite_byte16_(stream, LISP_VERSION_A));
	Return(faslwrite_byte16_(stream, LISP_VERSION_B));
	Return(faslwrite_byte16_(stream, LISP_VERSION_C));
	/* 24: CPU arch */
#ifdef LISP_ARCH_64BIT
	Return(faslwrite_byte_(stream, 1));
#else
	Return(faslwrite_byte_(stream, 0));
#endif
	/* 25: fixnum size */
#ifdef LISP_64BIT
	Return(faslwrite_byte_(stream, 1));
#else
	Return(faslwrite_byte_(stream, 0));
#endif
	/* 26: padding */
	memset(buffer, 0xFF, 32);
	Return(faslwrite_buffer_(stream, buffer, 6));
	/* 32: end */

	return 0;
}

int faslwrite_footer_(addr stream)
{
	char buffer[8];

	Return(faslwrite_break_(stream));
	Return(faslwrite_type_(stream, FaslCode_eof));
	memset(buffer, 0x00, 8);
	Return(faslwrite_buffer_(stream, buffer, 8));
	memset(buffer, 0xFF, 8);
	Return(faslwrite_buffer_(stream, buffer, 8));

	return 0;
}

int faslwrite_break_(addr stream)
{
	return faslwrite_type_(stream, FaslCode_break);
}


/*
 *  unbound
 */
static int faslwrite_unbound_(Execute ptr, addr stream)
{
	return faslwrite_type_(stream, FaslCode_unbound);
}


/*
 *  table code
 */
static void code_value_heap(addr *ret, enum CodeValueType type, CodeValue x)
{
	switch (type) {
		case CodeValueType_Addr:
			*ret = x.pos;
			break;

		case CodeValueType_Index:
			index_heap(ret, x.index);
			break;

		case CodeValueType_Fixnum:
			fixnum_heap(ret, x.value);
			break;

		case CodeValueType_FixnumNull:
			if (x.pos == Nil)
				*ret = Nil;
			else
				fixnum_heap(ret, x.value);
			break;

		case CodeValueType_Character:
			character_heap(ret, x.character);
			break;

		case CodeValueType_Null:
		default:
			*ret = Nil;
			break;
	}
}

static int faslwrite_value_operator_(Execute ptr, addr stream, pointer id, CodeValue x)
{
	enum CodeValueType type;
	enum FaslCode code;
	addr value;

	Check(p_size_code < id, "pointer error");
	GetCodeValueArray(id, &type);
	code_value_heap(&value, type, x);
	code = GetCompileWrite(id);
	/* write */
	Return(faslwrite_type_(stream, code));
	return faslwrite_value_(ptr, stream, value);
}

static int faslwrite_value_code_(Execute ptr, addr stream, addr pos)
{
	struct code_struct *str;
	struct code_value *sys, *bind;
	size_t size, i;

	Check(GetType(pos) != LISPTYPE_CODE, "type error.");
	/* type */
	Return(faslwrite_type_status_(stream, pos, FaslCode_code));
	/* struct */
	str = StructCode(pos);
	sys = str->sys;
	size = str->size;
	Return(faslwrite_size_(stream, size));
	/* code */
	GetArrayCode(pos, Code_Array, &pos);
	for (i = 0; i < size; i++) {
		bind = sys + i;
		Return(faslwrite_value_operator_(ptr, stream, bind->id, bind->value));
	}

	return 0;
}


/*
 *  table value
 */
typedef int (*faslwrite_callvalue)(Execute, addr, addr);
static faslwrite_callvalue FaslWrite_Value[LISPTYPE_COMPILE];

int faslwrite_value_(Execute ptr, addr stream, addr pos)
{
	enum LISPTYPE type;
	faslwrite_callvalue call;

	/* unbound */
	if (pos == Unbound)
		return faslwrite_unbound_(ptr, stream);

	/* object */
	type = GetType(pos);
	Check(LISPTYPE_COMPILE <= type, "type error");
	call = FaslWrite_Value[type];
	if (call == NULL) {
		return fmte_("Cannot compile the value ~S.", pos, NULL);
	}

	return (*call)(ptr, stream, pos);
}


/*
 *  initialize
 */
void init_compile_write(void)
{
	FaslWrite_Value[LISPTYPE_NIL] = faslwrite_value_nil_;
	FaslWrite_Value[LISPTYPE_T] = faslwrite_value_t_;
	FaslWrite_Value[LISPTYPE_TYPE] = faslwrite_value_type_;
	FaslWrite_Value[LISPTYPE_CLOS] = faslwrite_value_clos_;
	FaslWrite_Value[LISPTYPE_CONS] = faslwrite_value_cons_;
	FaslWrite_Value[LISPTYPE_ARRAY] = faslwrite_value_array_;
	FaslWrite_Value[LISPTYPE_VECTOR] = faslwrite_value_vector_;
	FaslWrite_Value[LISPTYPE_CHARACTER] = faslwrite_value_character_;
	FaslWrite_Value[LISPTYPE_STRING] = faslwrite_value_string_;
	FaslWrite_Value[LISPTYPE_HASHTABLE] = faslwrite_value_hashtable_;
	FaslWrite_Value[LISPTYPE_SYMBOL] = faslwrite_value_symbol_;
	FaslWrite_Value[LISPTYPE_FIXNUM] = faslwrite_value_fixnum_;
	FaslWrite_Value[LISPTYPE_BIGNUM] = faslwrite_value_bignum_;
	FaslWrite_Value[LISPTYPE_RATIO] = faslwrite_value_ratio_;
	FaslWrite_Value[LISPTYPE_SINGLE_FLOAT] = faslwrite_value_single_float_;
	FaslWrite_Value[LISPTYPE_DOUBLE_FLOAT] = faslwrite_value_double_float_;
	FaslWrite_Value[LISPTYPE_LONG_FLOAT] = faslwrite_value_long_float_;
	FaslWrite_Value[LISPTYPE_COMPLEX] = faslwrite_value_complex_;
	FaslWrite_Value[LISPTYPE_CODE] = faslwrite_value_code_;
	FaslWrite_Value[LISPTYPE_CALLNAME] = faslwrite_value_callname_;
	FaslWrite_Value[LISPTYPE_INDEX] = faslwrite_value_index_;
	FaslWrite_Value[LISPTYPE_PACKAGE] = faslwrite_value_package_;
	FaslWrite_Value[LISPTYPE_RANDOM_STATE] = faslwrite_value_random_state_;
	FaslWrite_Value[LISPTYPE_PATHNAME] = faslwrite_value_pathname_;
	FaslWrite_Value[LISPTYPE_QUOTE] = faslwrite_value_quote_;
	FaslWrite_Value[LISPTYPE_BITVECTOR] = faslwrite_value_bitvector_;
	FaslWrite_Value[LISPTYPE_LOAD_TIME_VALUE] = faslwrite_value_load_time_value_;
	FaslWrite_Value[LISPTYPE_PAPER] = faslwrite_value_paper_;
}

