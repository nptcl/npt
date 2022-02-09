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
static int faslread_magic_(addr stream, int *ret)
{
	char a[16], b[16];

	/* 0000FASL */
	Return(faslread_buffer_(stream, a, 8));
	if (memcmp(a, "\0\0\0\0FASL", 8) != 0)
		goto error;

	/* LISPNAME */
	Return(faslread_buffer_(stream, a, 8));
	memset(b, 0, 8);
	strncpy(b, LISPNAME, 8);
	if (memcmp(a, b, 8) != 0)
		goto error;

	/* OK */
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

int faslread_header_(addr input, int *ret)
{
	int check;
	byte buffer[64];
	uint8_t x;
	uint16_t v, a, b, c;

	/* 0: magic number */
	Return(faslread_magic_(input, &check));
	if (check)
		goto error;
	/* 16: endian */
	Return(faslread_variable_(input, v, &check));
	if (check)
		goto error;
	if (v != 1) {
		Debug("endian error.");
		goto error;
	}
	/* 18: version */
	Return(faslread_variable_(input, a, &check));
	if (check)
		goto error;
	Return(faslread_variable_(input, b, &check));
	if (check)
		goto error;
	Return(faslread_variable_(input, c, &check));
	if (check)
		goto error;
	if (a != LISP_VERSION_A || b != LISP_VERSION_B || c != LISP_VERSION_C)
		goto error;
	/* 24: CPU arch */
	Return(faslread_variable_(input, x, &check));
	if (check)
		goto error;
#ifdef LISP_ARCH_64BIT
	if (x != 1) {
		Debug("This fasl file is not 64bit arch.");
		goto error;
	}
#else
	if (x != 0) {
		Debug("This fasl file is not 32bit arch.");
		goto error;
	}
#endif
	/* 25: fixnum size */
	Return(faslread_variable_(input, x, &check));
	if (check)
		goto error;
#ifdef LISP_64BIT
	if (x != 1) {
		Debug("This fasl file is not 64bit fixnum.");
		goto error;
	}
#else
	if (x != 0) {
		Debug("This fasl file is not 32bit fixnum.");
		goto error;
	}
#endif
	/* 26: padding */
	Return(faslread_buffer_check_(input, buffer, 6, &check));
	if (check)
		goto error;
	/* 32: end */
	return Result(ret, 0);

error:
	return Result(ret, 1);
}

int faslread_footer_(addr input, int *ret)
{
	byte buffer[8];

	/* zero */
	Return(faslread_buffer_(input, buffer, 8));
	if (memcmp(buffer, "\x00\x00\x00\x00" "\x00\x00\x00\x00", 8) != 0)
		goto error;

	/* fill */
	Return(faslread_buffer_(input, buffer, 8));
	if (memcmp(buffer, "\xFF\xFF\xFF\xFF" "\xFF\xFF\xFF\xFF", 8) != 0)
		goto error;

	/* ok */
	return Result(ret, 0);

error:
	return Result(ret, 1);
}


/*
 *  system
 */
static int faslread_error_(Execute ptr, addr stream, addr *ret)
{
	return fmte_("fasl read error.", NULL);
}

static int faslread_unbound_(Execute ptr, addr stream, addr *ret)
{
	return Result(ret, Unbound);
}


/*
 *  code
 */
static int faslread_code_operator_(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	constindex index;
	addr car, cdr;

	/* type */
	Return(faslread_type_(stream, &type));
	Check(type < FaslCode_value, "type error");
	index = GetCompileRead(type);
	GetConstant(index, &car);

	/* result */
	Return(faslread_value_(ptr, stream, &cdr));
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_value_code_(Execute ptr, addr stream, addr *ret)
{
	FaslStatus status;
	addr vector, pos;
	size_t size, i;

	Return(faslread_status_(stream, &status));
	Return(faslread_size_(stream, &size));

	/* code */
	vector4_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(faslread_code_operator_(ptr, stream, &pos));
		SetArrayA4(vector, i, pos);
	}

	/* object */
	code_heap(&pos, vector);
	faslread_status_update(pos, status);
	update_code(pos);

	return Result(ret, pos);
}


/*
 *  code input
 */
typedef int (*faslread_calltype)(Execute, addr, addr *);
static faslread_calltype FaslRead_Table[FaslCode_value];

int faslread_value_(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	faslread_calltype call;

	Return(faslread_type_(stream, &type));
	Check(FaslCode_value <= type, "type error");
	call = FaslRead_Table[type];
	Check(call == NULL, "faslread call error.");
	return (*call)(ptr, stream, ret);
}


/*
 *  initialize
 */
void init_compile_read(void)
{
	FaslRead_Table[FaslCode_error] = faslread_error_;
	FaslRead_Table[FaslCode_unbound] = faslread_unbound_;
	FaslRead_Table[FaslCode_clos] = faslread_value_clos_;
	FaslRead_Table[FaslCode_code] = faslread_value_code_;
	FaslRead_Table[FaslCode_nil] = faslread_value_nil_;
	FaslRead_Table[FaslCode_t] = faslread_value_t_;
	FaslRead_Table[FaslCode_type] = faslread_value_type_;
	FaslRead_Table[FaslCode_cons] = faslread_value_cons_;
	FaslRead_Table[FaslCode_array] = faslread_value_array_;
	FaslRead_Table[FaslCode_vector2] = faslread_value_vector2_;
	FaslRead_Table[FaslCode_vector4] = faslread_value_vector4_;
#ifdef LISP_ARCH_64BIT
	FaslRead_Table[FaslCode_vector8] = faslread_value_vector8_;
#endif
	FaslRead_Table[FaslCode_character] = faslread_value_character_;
	FaslRead_Table[FaslCode_character7] = faslread_value_character7_;
	FaslRead_Table[FaslCode_string] = faslread_value_string_;
	FaslRead_Table[FaslCode_string7] = faslread_value_string7_;
	FaslRead_Table[FaslCode_hashtable] = faslread_value_hashtable_;
	FaslRead_Table[FaslCode_gensym] = faslread_value_gensym_;
	FaslRead_Table[FaslCode_symbol] = faslread_value_symbol_;
	FaslRead_Table[FaslCode_fixnum] = faslread_value_fixnum_;
	FaslRead_Table[FaslCode_bignum] = faslread_value_bignum_;
	FaslRead_Table[FaslCode_ratio] = faslread_value_ratio_;
	FaslRead_Table[FaslCode_single_float] = faslread_value_single_float_;
	FaslRead_Table[FaslCode_double_float] = faslread_value_double_float_;
	FaslRead_Table[FaslCode_long_float] = faslread_value_long_float_;
	FaslRead_Table[FaslCode_complex] = faslread_value_complex_;
	FaslRead_Table[FaslCode_callname] = faslread_value_callname_;
	FaslRead_Table[FaslCode_index] = faslread_value_index_;
	FaslRead_Table[FaslCode_package] = faslread_value_package_;
	FaslRead_Table[FaslCode_random_state] = faslread_value_random_state_;
	FaslRead_Table[FaslCode_pathname] = faslread_value_pathname_;
	FaslRead_Table[FaslCode_quote] = faslread_value_quote_;
	FaslRead_Table[FaslCode_bitvector] = faslread_value_bitvector_;
	FaslRead_Table[FaslCode_load] = faslread_value_load_time_value_;
	FaslRead_Table[FaslCode_paper] = faslread_value_paper_;
}

