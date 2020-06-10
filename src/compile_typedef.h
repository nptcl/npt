#ifndef __COMPILE_TYPEDEF__
#define __COMPILE_TYPEDEF__

enum FaslCode {
	/* fasl */
	FaslCode_error = 0,
	FaslCode_code,
	FaslCode_fixnum,
	FaslCode_bignum,
	FaslCode_ratio,
	FaslCode_type,
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
	/* size */
	FaslCode_size
};

#endif

