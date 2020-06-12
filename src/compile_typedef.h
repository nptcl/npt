#ifndef __COMPILE_TYPEDEF__
#define __COMPILE_TYPEDEF__

#include "define.h"

enum FaslCode {
	/* fasl */
	FaslCode_error = 0,
	FaslCode_code,
	FaslCode_nil,
	FaslCode_t,
	FaslCode_type,
	FaslCode_cons,
	FaslCode_vector2,
	FaslCode_vector4,
#ifdef LISP_ARCH_64BIT
	FaslCode_vector8,
#endif
	FaslCode_character,
	FaslCode_string,
	FaslCode_symbol,
	FaslCode_fixnum,
	FaslCode_bignum,
	FaslCode_ratio,
	FaslCode_single_float,
	FaslCode_double_float,
	FaslCode_long_float,
	FaslCode_complex,
	FaslCode_package,
	/* system */
	FaslCode_nop,
	FaslCode_execute_control_set,
	FaslCode_execute_control_push,
	FaslCode_execute_switch_set,
	FaslCode_execute_switch_push,
	/* object */
	FaslCode_set,
	FaslCode_push,
	FaslCode_push_result,
	FaslCode_push_values,
	FaslCode_nil_set,
	FaslCode_nil_push,
	FaslCode_t_set,
	FaslCode_t_push,
	FaslCode_type_result,
	/* setq */
	FaslCode_setq_lexical,
	FaslCode_setq_special,
	FaslCode_setq_global,
	/* size */
	FaslCode_size
};

#endif

