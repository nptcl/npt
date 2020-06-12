#include <memory.h>
#include <string.h>
#include "code_object.h"
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
static void faslwrite_magic(addr stream)
{
	char a[8];

	memset(a, 0, 8);
	strncpy(a, LISPNAME, 8);
	faslwrite_buffer(stream, a, 8);
	faslwrite_buffer(stream, "FASL\0\0\0\0", 8);
}

static void faslwrite_byte16(addr stream, uint16_t v)
{
	faslwrite_buffer(stream, &v, sizeoft(v));
}

_g void faslwrite_header(addr stream)
{
	char buffer[32];

	/* 0: magic number */
	faslwrite_magic(stream);
	/* 16: endian */
	faslwrite_byte16(stream, 1);
	/* 18: version */
	faslwrite_byte16(stream, LISP_VERSION_A);
	faslwrite_byte16(stream, LISP_VERSION_B);
	faslwrite_byte16(stream, LISP_VERSION_C);
	/* 20: arch */
#ifdef LISP_64BIT
	faslwrite_byte16(stream, 1);
#else
	faslwrite_byte16(stream, 0);
#endif
	/* 22: padding */
	memset(buffer, 0xFF, 32);
	faslwrite_buffer(stream, buffer, 2);
	/* 24: end */
}

_g void faslwrite_footer(addr stream)
{
	char buffer[8];

	memset(buffer, 0x00, 8);
	faslwrite_buffer(stream, buffer, 8);
	memset(buffer, 0xFF, 8);
	faslwrite_buffer(stream, buffer, 8);
}


/*
 *  system
 */
static int faslwrite_nop_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_nop);
	return 0;
}

static int faslwrite_execute_control_set_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_execute_control_set);
	Return(faslwrite_code(ptr, stream, x.pos));
	return 0;
}

static int faslwrite_execute_control_push_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_execute_control_push);
	Return(faslwrite_code(ptr, stream, x.pos));
	return 0;
}

static int faslwrite_execute_switch_set_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_execute_switch_set);
	Return(faslwrite_code(ptr, stream, x.pos));
	return 0;
}

static int faslwrite_execute_switch_push_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_execute_switch_push);
	Return(faslwrite_code(ptr, stream, x.pos));
	return 0;
}


/*
 *  object
 */
static int faslwrite_set_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_set);
	return faslwrite_value(ptr, stream, x.pos);
}

static int faslwrite_push_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_push);
	return faslwrite_value(ptr, stream, x.pos);
}

static int faslwrite_push_result_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_push_result);
	return 0;
}

static int faslwrite_push_values_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_push_values);
	return 0;
}

static int faslwrite_nil_set_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_nil_set);
	return 0;
}

static int faslwrite_nil_push_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_nil_push);
	return 0;
}

static int faslwrite_t_set_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_t_set);
	return 0;
}

static int faslwrite_t_push_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_t_push);
	return 0;
}


/*
 *  let
 */
static int faslwrite_type_result_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_type_result);
	return faslwrite_value_type(ptr, stream, x.pos);
}


/*
 *  setq
 */
static int faslwrite_setq_lexical_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_setq_lexical);
	faslwrite_buffer(stream, &x.index, IdxSize);
	return 0;
}

static int faslwrite_setq_special_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_setq_special);
	return faslwrite_value_symbol(ptr, stream, x.pos);
}

static int faslwrite_setq_global_code(Execute ptr, addr stream, CodeValue x)
{
	faslwrite_type(stream, FaslCode_setq_global);
	return faslwrite_value_symbol(ptr, stream, x.pos);
}


/*
 *  code output
 */
typedef int (*faslwrite_calltype)(Execute, addr, CodeValue);
static faslwrite_calltype faslwrite_table[p_size_code];

_g int faslwrite_code(Execute ptr, addr stream, addr pos)
{
	struct code_struct *str;
	struct code_value *sys, *bind;
	faslwrite_calltype call;
	size_t size, i;

	Check(GetType(pos) != LISPTYPE_CODE, "type error.");
	/* type */
	faslwrite_type(stream, FaslCode_code);
	/* struct */
	str = StructCode(pos);
	faslwrite_buffer(stream, str, sizeoft(struct code_struct));
	/* code */
	GetArrayCode(pos, Code_Array, &pos);
	sys = str->sys;
	size = str->size;
	for (i = 0; i < size; i++) {
		bind = sys + i;
		call = faslwrite_table[bind->id];
		Check(call == NULL, "faslwrite call error.");
		Return((*call)(ptr, stream, bind->value));
	}

	return 0;
}


/*
 *  initialize
 */
_g void init_compile_write(void)
{
	/* system */
	faslwrite_table[p_nop_code] = faslwrite_nop_code;
	faslwrite_table[p_execute_control_set_code] = faslwrite_execute_control_set_code;
	faslwrite_table[p_execute_control_push_code] = faslwrite_execute_control_push_code;
	faslwrite_table[p_execute_switch_set_code] = faslwrite_execute_switch_set_code;
	faslwrite_table[p_execute_switch_push_code] = faslwrite_execute_switch_push_code;

	/* object */
	faslwrite_table[p_set_code] = faslwrite_set_code;
	faslwrite_table[p_push_code] = faslwrite_push_code;
	faslwrite_table[p_push_result_code] = faslwrite_push_result_code;
	faslwrite_table[p_push_values_code] = faslwrite_push_values_code;
	faslwrite_table[p_nil_set_code] = faslwrite_nil_set_code;
	faslwrite_table[p_nil_push_code] = faslwrite_nil_push_code;
	faslwrite_table[p_t_set_code] = faslwrite_t_set_code;
	faslwrite_table[p_t_push_code] = faslwrite_t_push_code;

	/* let */
	faslwrite_table[p_type_result_code] = faslwrite_type_result_code;

	/* setq */
	faslwrite_table[p_setq_lexical_code] = faslwrite_setq_lexical_code;
	faslwrite_table[p_setq_special_code] = faslwrite_setq_special_code;
	faslwrite_table[p_setq_global_code] = faslwrite_setq_global_code;

}

