#ifndef __COMPILE_TYPEDEF__
#define __COMPILE_TYPEDEF__

#include "constant.h"
#include "define.h"
#include "pointer.h"

enum FaslCode {
	/* fasl */
	FaslCode_error = 0,
	FaslCode_unbound,
	FaslCode_code,
	FaslCode_nil,
	FaslCode_t,
	FaslCode_type,
	FaslCode_clos,
	FaslCode_cons,
	FaslCode_array,
	FaslCode_vector2,
	FaslCode_vector4,
#ifdef LISP_ARCH_64BIT
	FaslCode_vector8,
#endif
	FaslCode_character,
	FaslCode_string,
	FaslCode_hashtable,
	FaslCode_symbol,
	FaslCode_fixnum,
	FaslCode_bignum,
	FaslCode_ratio,
	FaslCode_single_float,
	FaslCode_double_float,
	FaslCode_long_float,
	FaslCode_complex,
	FaslCode_callname,
	FaslCode_index,
	FaslCode_package,
	FaslCode_random_state,
	FaslCode_pathname,
	FaslCode_bitvector,
	FaslCode_load_time_value,

	FaslCode_value, /* end of value */

	FaslCode_nop,
	FaslCode_execute_control_set,
	FaslCode_execute_control_push,
	FaslCode_execute_control_save,

	FaslCode_set,
	FaslCode_push,
	FaslCode_push_result,
	FaslCode_push_values,
	FaslCode_nil_set,
	FaslCode_nil_push,
	FaslCode_t_set,
	FaslCode_t_push,

	FaslCode_lexical,
	FaslCode_lexical_set,
	FaslCode_lexical_push,
	FaslCode_lexical_rem,
	FaslCode_special_set,
	FaslCode_special_push,
	FaslCode_special_rem,

	FaslCode_declaim_special,
	FaslCode_declaim_type_value,
	FaslCode_declaim_type_function,
	FaslCode_declaim_inline,
	FaslCode_declaim_notinline,
	FaslCode_declaim_compilation,
	FaslCode_declaim_debug,
	FaslCode_declaim_safety,
	FaslCode_declaim_space,
	FaslCode_declaim_speed,
	FaslCode_declaim_declaration,

	FaslCode_type_result,
	FaslCode_type_lexical,
	FaslCode_type_special,
	FaslCode_type_global,
	FaslCode_type_function,
	FaslCode_type_setf,
	FaslCode_let_lexical,
	FaslCode_let_special,
	FaslCode_leta_special,

	FaslCode_setq_lexical,
	FaslCode_setq_special,
	FaslCode_setq_global,

	FaslCode_function_set,
	FaslCode_function_push,
	FaslCode_setf_set,
	FaslCode_setf_push,

	FaslCode_defmacro,
	FaslCode_deftype,
	FaslCode_define_compiler_macro,
	FaslCode_define_symbol_macro,
	FaslCode_defun,

	FaslCode_call_result,
	FaslCode_call_type,
	FaslCode_call_function,
	FaslCode_call_setf,
	FaslCode_call_lexical,

	FaslCode_values_nil,
	FaslCode_values_set,
	FaslCode_the_set,
	FaslCode_the_push,

	FaslCode_if_unbound,
	FaslCode_if_nil,
	FaslCode_if_t,
	FaslCode_goto,
	FaslCode_go,
	FaslCode_return_from,
	FaslCode_catch,
	FaslCode_throw_operator,
	FaslCode_taginfo,
	FaslCode_blockinfo,
	FaslCode_unwind_protect,

	FaslCode_handler_bind,
	FaslCode_handler_case,
	FaslCode_restart_bind,
	FaslCode_restart_case,

	FaslCode_funcall,
	FaslCode_nth_value,
	FaslCode_progv,

	FaslCode_pop,
	FaslCode_pop_unbound,
	FaslCode_getf,
	FaslCode_rest,
	FaslCode_allow_other_keys,
	FaslCode_rest_null,
	FaslCode_whole,

	FaslCode_lambda,
	FaslCode_lambda_name,
	FaslCode_lambda_type,
	FaslCode_lambda_doc,
	FaslCode_lambda_form,
	FaslCode_lambda_defun,
	FaslCode_lambda_closure,
	FaslCode_lambda_lexical,
	FaslCode_macro,
	FaslCode_macro_special,
	FaslCode_macro_env,
	FaslCode_macro_whole,

	FaslCode_bind1_type,
	FaslCode_bind1_special,
	FaslCode_bind1_lexical,
	FaslCode_bind2_type,
	FaslCode_bind2_special,
	FaslCode_bind2_lexical,

	FaslCode_load_time_value_bind,
	FaslCode_load_time_value_init,
	FaslCode_load_time_value_set,
	FaslCode_load_time_value_push,

	FaslCode_step,

	FaslCode_optcode_result_type,
	FaslCode_optcode_car0_set,
	FaslCode_optcode_car0_push,
	FaslCode_optcode_car1_set,
	FaslCode_optcode_car1_push,
	FaslCode_optcode_cdr0_set,
	FaslCode_optcode_cdr0_push,
	FaslCode_optcode_cdr1_set,
	FaslCode_optcode_cdr1_push,
	FaslCode_optcode_cons,

	/* size */
	FaslCode_size,

	/* end */
	FaslCode_end = 0xFF
};

__extern enum FaslCode CompileWrite[p_size_code];
__extern constindex CompileRead[FaslCode_size];

#ifdef LISP_DEBUG
#define GetCompileWrite(x)	get_compile_write(x)
#define GetCompileRead(x)	get_compile_read(x)
#else
#define GetCompileWrite(x)	CompileWrite[x]
#define GetCompileRead(x)	CompileRead[x]
#endif

_g enum FaslCode get_compile_write(pointer id);
_g constindex get_compile_read(enum FaslCode id);
_g void init_compile_typedef(void);

#endif

