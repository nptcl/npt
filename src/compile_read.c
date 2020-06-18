#include <memory.h>
#include <string.h>
#include "code_object.h"
#include "compile_array.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_type.h"
#include "compile_typedef.h"
#include "compile_value.h"
#include "condition.h"
#include "define.h"
#include "execute.h"
#include "stream.h"

/*
 *  read function
 */
static int faslread_magic(addr stream)
{
	char a[8], b[8];

	/* LISPNAME */
	faslread_buffer(stream, a, 8);
	memset(b, 0, 8);
	strncpy(b, LISPNAME, 8);
	if (memcmp(a, b, 8) != 0)
		return 1;

	/* FASL0000 */
	faslread_buffer(stream, a, 8);
	if (memcmp(a, "FASL\0\0\0\0", 8) != 0)
		return 1;

	/* OK */
	return 0;
}

_g int faslread_header(addr input)
{
	byte buffer[64];
	uint16_t v, a, b, c;

	/* 0: magic number */
	if (faslread_magic(input))
		return 1;
	/* 16: endian */
	if (faslread_variable(input, v))
		return 1;
	if (v != 1) {
		Debug("endian error.");
		return 1;
	}
	/* 18: version */
	if (faslread_variable(input, a))
		return 1;
	if (faslread_variable(input, b))
		return 1;
	if (faslread_variable(input, c))
		return 1;
	if (a != LISP_VERSION_A || b != LISP_VERSION_B || c != LISP_VERSION_C)
		return 1;
	/* 24: arch */
	if (faslread_variable(input, v))
		return 1;
#ifdef LISP_64BIT
	if (v != 1) {
		Debug("This fasl file is not 64bit arch.");
		return 1;
	}
#else
	if (v != 0) {
		Debug("This fasl file is not 32bit arch.");
		return 1;
	}
#endif
	/* 26: padding */
	if (faslread_buffer_check(input, buffer, 6))
		return 1;
	/* 32: end */

	return 0;
}

_g int faslread_footer(addr input)
{
	byte buffer[8];

	/* fill */
	faslread_buffer(input, buffer, 8);
	if (memcmp(buffer, "\x00\x00\x00\x00" "\x00\x00\x00\x00", 8) != 0)
		return 1;
	/* zero */
	faslread_buffer(input, buffer, 8);
	if (memcmp(buffer, "\xFF\xFF\xFF\xFF" "\xFF\xFF\xFF\xFF", 8) != 0)
		return 1;

	return 0;
}


/*
 *  system
 */
static int faslread_error(Execute ptr, addr stream, addr *ret)
{
	fmte("fasl read error.", NULL);
	return Result(ret, Nil);
}

static int faslread_unbound(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, Unbound);
}


/*
 *  code
 */
static int faslread_code_operator(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	constindex index;
	addr car, cdr;

	/* car */
	faslread_type(stream, &type);
	Check(type < FaslCode_value, "type error");
	index = GetCompileRead(type);
	GetConstant(index, &car);

	/* cdr */
	Return(faslread_value(ptr, stream, &cdr));

	/* result */
	cons_heap(ret, car, cdr);
	return 0;
}

static int faslread_value_code(Execute ptr, addr stream, addr *ret)
{
	addr vector, pos;
	size_t size, i;
	struct code_struct head, *str;

	/* struct */
	faslread_buffer(stream, &head, sizeoft(head));
	/* code */
	size = head.size;
	vector4_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(faslread_code_operator(ptr, stream, &pos));
		SetArrayA4(vector, i, pos);
	}

	/* update */
	code_heap(&pos, vector);
	str = StructCode(pos);
	str->p_control = head.p_control;
	str->p_args = head.p_args;
	update_code(pos);

	return Result(ret, pos);
}


/*
 *  code input
 */
typedef int (*faslread_calltype)(Execute, addr, addr *);
static faslread_calltype FaslRead_Table[FaslCode_value];

_g int faslread_value(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	faslread_calltype call;

	faslread_type(stream, &type);
	Check(FaslCode_value <= type, "type error");
	call = FaslRead_Table[type];
	Check(call == NULL, "faslread call error.");
	return (*call)(ptr, stream, ret);
}


/*
 *  initialize
 */
_g void init_compile_read(void)
{
	FaslRead_Table[FaslCode_error] = faslread_error;
	FaslRead_Table[FaslCode_unbound] = faslread_unbound;
	FaslRead_Table[FaslCode_code] = faslread_value_code;
	FaslRead_Table[FaslCode_nil] = faslread_value_nil;
	FaslRead_Table[FaslCode_t] = faslread_value_t;
	FaslRead_Table[FaslCode_type] = faslread_value_type;
	FaslRead_Table[FaslCode_cons] = faslread_value_cons;
	FaslRead_Table[FaslCode_array] = faslread_value_array;
	FaslRead_Table[FaslCode_vector2] = faslread_value_vector2;
	FaslRead_Table[FaslCode_vector4] = faslread_value_vector4;
#ifdef LISP_ARCH_64BIT
	FaslRead_Table[FaslCode_vector8] = faslread_value_vector8;
#endif
	FaslRead_Table[FaslCode_character] = faslread_value_character;
	FaslRead_Table[FaslCode_string] = faslread_value_string;
	FaslRead_Table[FaslCode_hashtable] = faslread_value_hashtable;
	FaslRead_Table[FaslCode_symbol] = faslread_value_symbol;
	FaslRead_Table[FaslCode_fixnum] = faslread_value_fixnum;
	FaslRead_Table[FaslCode_bignum] = faslread_value_bignum;
	FaslRead_Table[FaslCode_ratio] = faslread_value_ratio;
	FaslRead_Table[FaslCode_single_float] = faslread_value_single_float;
	FaslRead_Table[FaslCode_double_float] = faslread_value_double_float;
	FaslRead_Table[FaslCode_long_float] = faslread_value_long_float;
	FaslRead_Table[FaslCode_complex] = faslread_value_complex;
	FaslRead_Table[FaslCode_index] = faslread_value_index;
	FaslRead_Table[FaslCode_package] = faslread_value_package;
	FaslRead_Table[FaslCode_random_state] = faslread_value_random_state;
	FaslRead_Table[FaslCode_pathname] = faslread_value_pathname;
	FaslRead_Table[FaslCode_bitvector] = faslread_value_bitvector;
	FaslRead_Table[FaslCode_load_time_value] = faslread_value_load_time_value;
}

