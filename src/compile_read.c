#include <memory.h>
#include <string.h>
#include "code_object.h"
#include "compile_read.h"
#include "compile_stream.h"
#include "compile_value.h"
#include "compile_type.h"
#include "compile_typedef.h"
#include "condition.h"
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
	/* 20: arch */
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
	/* 22: padding */
	if (faslread_buffer_check(input, buffer, 2))
		return 1;
	/* 24: end */

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
static int faslread_error_code(Execute ptr, addr stream, addr *ret)
{
	fmte("fasl read error.", NULL);
	return Result(ret, Nil);
}

#define FaslRead_single(x, r) { \
	addr __pos; \
	GetConst(CODE_##x, &__pos); \
	conscar_heap((r), __pos); \
}
static int faslread_nop_code(Execute ptr, addr stream, addr *ret)
{
	FaslRead_single(NOP, ret);
	return 0;
}

static int faslread_execute_control_set_code(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_code(ptr, stream, &cdr));
	GetConst(CODE_EXECUTE_CONTROL_SET, &car);
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_execute_control_push_code(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_code(ptr, stream, &cdr));
	GetConst(CODE_EXECUTE_CONTROL_PUSH, &car);
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_execute_switch_set_code(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_code(ptr, stream, &cdr));
	GetConst(CODE_EXECUTE_SWITCH_SET, &car);
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_execute_switch_push_code(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_code(ptr, stream, &cdr));
	GetConst(CODE_EXECUTE_SWITCH_PUSH, &car);
	cons_heap(ret, car, cdr);

	return 0;
}


/*
 *  object
 */
static int faslread_set_code(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_object(ptr, stream, &cdr));
	GetConst(CODE_SET, &car);
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_push_code(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_object(ptr, stream, &cdr));
	GetConst(CODE_PUSH, &car);
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_push_result_code(Execute ptr, addr stream, addr *ret)
{
	FaslRead_single(PUSH_RESULT, ret);
	return 0;
}

static int faslread_push_values_code(Execute ptr, addr stream, addr *ret)
{
	FaslRead_single(PUSH_VALUES, ret);
	return 0;
}

static int faslread_type_result_code(Execute ptr, addr stream, addr *ret)
{
	addr car, cdr;

	Return(faslread_object(ptr, stream, &cdr));
	CheckType(cdr, LISPTYPE_TYPE);
	GetConst(CODE_TYPE_RESULT, &car);
	cons_heap(ret, car, cdr);

	return 0;
}

static int faslread_nil_set_code(Execute ptr, addr stream, addr *ret)
{
	FaslRead_single(NIL_SET, ret);
	return 0;
}

static int faslread_nil_push_code(Execute ptr, addr stream, addr *ret)
{
	FaslRead_single(NIL_PUSH, ret);
	return 0;
}

static int faslread_t_set_code(Execute ptr, addr stream, addr *ret)
{
	FaslRead_single(T_SET, ret);
	return 0;
}

static int faslread_t_push_code(Execute ptr, addr stream, addr *ret)
{
	FaslRead_single(T_PUSH, ret);
	return 0;
}


/*
 *  code input
 */
typedef int (*faslread_calltype)(Execute, addr, addr *);
static faslread_calltype faslread_table[FaslCode_size];

_g int faslread_object(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	faslread_calltype call;

	faslread_type(stream, &type);
	call = faslread_table[type];
	Check(call == NULL, "faslread call error.");
	return (*call)(ptr, stream, ret);
}

_g int faslread_code(Execute ptr, addr stream, addr *ret)
{
	enum FaslCode type;
	addr vector, pos;
	size_t size, i;
	struct code_struct head, *str;

	/* type */
	faslread_type(stream, &type);
	Check(type != FaslCode_code, "type error.");
	/* struct */
	faslread_buffer(stream, &head, sizeoft(head));
	/* code */
	size = head.size;
	vector4_heap(&vector, size);
	for (i = 0; i < size; i++) {
		Return(faslread_object(ptr, stream, &pos));
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
 *  initialize
 */
_g void init_compile_read(void)
{
	faslread_table[FaslCode_error] = faslread_error_code;
	faslread_table[FaslCode_fixnum] = faslread_fixnum_code;
	faslread_table[FaslCode_bignum] = faslread_bignum_code;
	faslread_table[FaslCode_ratio] = faslread_ratio_code;
	faslread_table[FaslCode_type] = faslread_type_code;

	/* system */
	faslread_table[FaslCode_nop] = faslread_nop_code;
	faslread_table[FaslCode_execute_control_set] = faslread_execute_control_set_code;
	faslread_table[FaslCode_execute_control_push] = faslread_execute_control_push_code;
	faslread_table[FaslCode_execute_switch_set] = faslread_execute_switch_set_code;
	faslread_table[FaslCode_execute_switch_push] = faslread_execute_switch_push_code;

	/* object */
	faslread_table[FaslCode_set] = faslread_set_code;
	faslread_table[FaslCode_push] = faslread_push_code;
	faslread_table[FaslCode_push_result] = faslread_push_result_code;
	faslread_table[FaslCode_push_values] = faslread_push_values_code;
	faslread_table[FaslCode_nil_set] = faslread_nil_set_code;
	faslread_table[FaslCode_nil_push] = faslread_nil_push_code;
	faslread_table[FaslCode_t_set] = faslread_t_set_code;
	faslread_table[FaslCode_t_push] = faslread_t_push_code;

	faslread_table[FaslCode_type_result] = faslread_type_result_code;
}

