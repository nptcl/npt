#ifndef __CODE_FUNCTION_HEADER__
#define __CODE_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

#define nop_code _n(nop_code)
#define execute_control_set_code _n(execute_control_set_code)
#define execute_control_push_code _n(execute_control_push_code)
#define execute_control_save_code _n(execute_control_save_code)
#define set_code _n(set_code)
#define push_code _n(push_code)
#define push_result_code _n(push_result_code)
#define push_values_code _n(push_values_code)
#define nil_set_code _n(nil_set_code)
#define nil_push_code _n(nil_push_code)
#define t_set_code _n(t_set_code)
#define t_push_code _n(t_push_code)
#define lexical_code _n(lexical_code)
#define lexical_set_code _n(lexical_set_code)
#define lexical_push_code _n(lexical_push_code)
#define lexical_rem_code _n(lexical_rem_code)
#define special_set_code _n(special_set_code)
#define special_push_code _n(special_push_code)
#define special_rem_code _n(special_rem_code)
#define declaim_special_code _n(declaim_special_code)
#define declaim_type_value_code _n(declaim_type_value_code)
#define declaim_type_function_code _n(declaim_type_function_code)
#define declaim_inline_code _n(declaim_inline_code)
#define declaim_notinline_code _n(declaim_notinline_code)
#define declaim_compilation_code _n(declaim_compilation_code)
#define declaim_debug_code _n(declaim_debug_code)
#define declaim_safety_code _n(declaim_safety_code)
#define declaim_space_code _n(declaim_space_code)
#define declaim_speed_code _n(declaim_speed_code)
#define declaim_declaration_code _n(declaim_declaration_code)
#define type_result_code _n(type_result_code)
#define type_lexical_code _n(type_lexical_code)
#define type_special_code _n(type_special_code)
#define type_global_code _n(type_global_code)
#define type_function_code _n(type_function_code)
#define type_setf_code _n(type_setf_code)
#define let_lexical_code _n(let_lexical_code)
#define let_special_code _n(let_special_code)
#define leta_special_code _n(leta_special_code)
#define setq_lexical_code _n(setq_lexical_code)
#define setq_special_code _n(setq_special_code)
#define setq_global_code _n(setq_global_code)
#define function_set_code _n(function_set_code)
#define function_push_code _n(function_push_code)
#define setf_set_code _n(setf_set_code)
#define setf_push_code _n(setf_push_code)
#define defmacro_code _n(defmacro_code)
#define deftype_code _n(deftype_code)
#define define_compiler_macro_code _n(define_compiler_macro_code)
#define define_symbol_macro_code _n(define_symbol_macro_code)
#define defun_code _n(defun_code)
#define call_result_code _n(call_result_code)
#define call_type_code _n(call_type_code)
#define call_function_code _n(call_function_code)
#define call_setf_code _n(call_setf_code)
#define call_lexical_code _n(call_lexical_code)
#define values_nil_code _n(values_nil_code)
#define values_set_code _n(values_set_code)
#define the_set_code _n(the_set_code)
#define the_push_code _n(the_push_code)
#define if_unbound_code _n(if_unbound_code)
#define if_nil_code _n(if_nil_code)
#define if_t_code _n(if_t_code)
#define goto_code _n(goto_code)
#define go_code _n(go_code)
#define return_from_code _n(return_from_code)
#define catch_code _n(catch_code)
#define throw_operator_code _n(throw_operator_code)
#define taginfo_code _n(taginfo_code)
#define blockinfo_code _n(blockinfo_code)
#define unwind_protect_code _n(unwind_protect_code)
#define handler_bind_code _n(handler_bind_code)
#define handler_case_code _n(handler_case_code)
#define restart_bind_code _n(restart_bind_code)
#define restart_case_code _n(restart_case_code)
#define funcall_code _n(funcall_code)
#define nth_value_code _n(nth_value_code)
#define progv_code _n(progv_code)
#define load_time_value_bind_code _n(load_time_value_bind_code)
#define load_time_value_init_code _n(load_time_value_init_code)
#define load_time_value_set_code _n(load_time_value_set_code)
#define load_time_value_push_code _n(load_time_value_push_code)
#define step_code _n(step_code)

_g int nop_code(Execute ptr, CodeValue x);
_g int execute_control_set_code(Execute ptr, CodeValue x);
_g int execute_control_push_code(Execute ptr, CodeValue x);
_g int execute_control_save_code(Execute ptr, CodeValue x);

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

_g int funcall_code(Execute ptr, CodeValue x);
_g int nth_value_code(Execute ptr, CodeValue x);
_g int progv_code(Execute ptr, CodeValue x);

_g int load_time_value_bind_code(Execute ptr, CodeValue x);
_g int load_time_value_init_code(Execute ptr, CodeValue x);
_g int load_time_value_set_code(Execute ptr, CodeValue x);
_g int load_time_value_push_code(Execute ptr, CodeValue x);

_g int step_code(Execute ptr, CodeValue x);

#endif

