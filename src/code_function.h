#ifndef __CODE_FUNCTION_HEADER__
#define __CODE_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

_g int nop_code(Execute ptr, CodeValue x);
_g int execute_control_set_code(Execute ptr, CodeValue x);
_g int execute_control_push_code(Execute ptr, CodeValue x);
_g int execute_switch_set_code(Execute ptr, CodeValue x);
_g int execute_switch_push_code(Execute ptr, CodeValue x);

_g int set_code(Execute ptr, CodeValue x);
_g int push_code(Execute ptr, CodeValue x);
_g int push_result_code(Execute ptr, CodeValue x);
_g int push_values_code(Execute ptr, CodeValue x);
_g int nil_set_code(Execute ptr, CodeValue x);
_g int nil_push_code(Execute ptr, CodeValue x);
_g int t_set_code(Execute ptr, CodeValue x);
_g int t_push_code(Execute ptr, CodeValue x);

_g int lexical_code(Execute ptr, CodeValue x);
_g int lexical_set_code(Execute ptr, CodeValue x);
_g int lexical_push_code(Execute ptr, CodeValue x);
_g int lexical_rem_code(Execute ptr, CodeValue x);
_g int special_set_code(Execute ptr, CodeValue x);
_g int special_push_code(Execute ptr, CodeValue x);
_g int special_rem_code(Execute ptr, CodeValue x);

_g int declaim_special_code(Execute ptr, CodeValue x);
_g int declaim_type_value_code(Execute ptr, CodeValue x);
_g int declaim_type_function_code(Execute ptr, CodeValue x);
_g int declaim_inline_code(Execute ptr, CodeValue x);
_g int declaim_notinline_code(Execute ptr, CodeValue x);
_g int declaim_compilation_code(Execute ptr, CodeValue x);
_g int declaim_debug_code(Execute ptr, CodeValue x);
_g int declaim_safety_code(Execute ptr, CodeValue x);
_g int declaim_space_code(Execute ptr, CodeValue x);
_g int declaim_speed_code(Execute ptr, CodeValue x);
_g int declaim_declaration_code(Execute ptr, CodeValue x);

_g int type_result_code(Execute ptr, CodeValue x);
_g int type_lexical_code(Execute ptr, CodeValue x);
_g int type_special_code(Execute ptr, CodeValue x);
_g int type_global_code(Execute ptr, CodeValue x);
_g int type_function_code(Execute ptr, CodeValue x);
_g int type_setf_code(Execute ptr, CodeValue x);
_g int let_lexical_code(Execute ptr, CodeValue x);
_g int let_special_code(Execute ptr, CodeValue x);
_g int leta_special_code(Execute ptr, CodeValue x);

_g int setq_lexical_code(Execute ptr, CodeValue x);
_g int setq_special_code(Execute ptr, CodeValue x);
_g int setq_global_code(Execute ptr, CodeValue x);

_g int function_set_code(Execute ptr, CodeValue x);
_g int function_push_code(Execute ptr, CodeValue x);
_g int setf_set_code(Execute ptr, CodeValue x);
_g int setf_push_code(Execute ptr, CodeValue x);

_g int defmacro_code(Execute ptr, CodeValue x);
_g int deftype_code(Execute ptr, CodeValue x);
_g int define_compiler_macro_code(Execute ptr, CodeValue x);
_g int define_symbol_macro_code(Execute ptr, CodeValue x);
_g int defun_code(Execute ptr, CodeValue x);

_g int call_code(Execute ptr, CodeValue x);
_g int call_result_code(Execute ptr, CodeValue x);
_g int call_type_code(Execute ptr, CodeValue x);
_g int call_function_code(Execute ptr, CodeValue x);
_g int call_setf_code(Execute ptr, CodeValue x);
_g int call_lexical_code(Execute ptr, CodeValue x);

_g int values_nil_code(Execute ptr, CodeValue x);
_g int values_set_code(Execute ptr, CodeValue x);
_g int the_set_code(Execute ptr, CodeValue x);
_g int the_push_code(Execute ptr, CodeValue x);

_g int if_unbound_code(Execute ptr, CodeValue x);
_g int if_nil_code(Execute ptr, CodeValue x);
_g int if_t_code(Execute ptr, CodeValue x);
_g int goto_code(Execute ptr, CodeValue x);
_g int go_code(Execute ptr, CodeValue x);
_g int return_from_code(Execute ptr, CodeValue x);
_g int catch_code(Execute ptr, CodeValue x);
_g int throw_operator_code(Execute ptr, CodeValue x);
_g int taginfo_code(Execute ptr, CodeValue x);
_g int blockinfo_code(Execute ptr, CodeValue x);
_g int unwind_protect_code(Execute ptr, CodeValue x);

_g int handler_bind_code(Execute ptr, CodeValue x);
_g int handler_case_code(Execute ptr, CodeValue x);
_g int restart_bind_code(Execute ptr, CodeValue x);
_g int restart_case_code(Execute ptr, CodeValue x);

_g int prog1_set_code(Execute ptr, CodeValue x);
_g int prog1_push_code(Execute ptr, CodeValue x);
_g int funcall_code(Execute ptr, CodeValue x);
_g int nth_value_code(Execute ptr, CodeValue x);
_g int progv_code(Execute ptr, CodeValue x);

_g int load_time_value_alloc_code(Execute ptr, CodeValue x);
_g int load_time_value_value_code(Execute ptr, CodeValue x);
_g int load_time_value_set_code(Execute ptr, CodeValue x);
_g int load_time_value_push_code(Execute ptr, CodeValue x);

#endif

