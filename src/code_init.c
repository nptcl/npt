#include "code_function.h"
#include "code_init.h"
#include "code_lambda.h"
#include "constant.h"
#include "function.h"
#include "symbol.h"

#define defcode(x,y) defcode_constant(CONSTANT_CODE_##x, p_##y)
#define initcode(x,y) { \
	SetPointer_code(p_##x, x); \
	CodeValueArray[p_##x] = (byte)CodeValueType_##y; \
}

byte CodeValueArray[p_size_code];

/*
 *  initialize
 */
void defcode_constant(constindex index, pointer p)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Check(symbol == Nil || symbol == Unbound, "constant error");
	GetFunctionSymbol(symbol, &pos);
	Check(pos != Unbound, "code-function already exists.");
	compiled_system(&pos, symbol);
	setcompiled_code(pos, p);
	SetFunctionSymbol(symbol, pos);
}

void init_code_init(void)
{
	cleartype(CodeValueArray);

	/* system */
	initcode(nop_code, Null);
	initcode(execute_control_set_code, Addr);
	initcode(execute_control_push_code, Addr);
	initcode(execute_control_save_code, Addr);

	/* object */
	initcode(set_code, Addr);
	initcode(push_code, Addr);
	initcode(push_result_code, Null);
	initcode(push_values_code, Null);
	initcode(nil_set_code, Null);
	initcode(nil_push_code, Null);
	initcode(t_set_code, Null);
	initcode(t_push_code, Null);

	/* symbol */
	initcode(lexical_code, Addr);
	initcode(lexical_set_code, Index);
	initcode(lexical_push_code, Index);
	initcode(lexical_rem_code, Index);
	initcode(special_set_code, Addr);
	initcode(special_push_code, Addr);
	initcode(special_rem_code, Addr);

	/* declaim */
	initcode(declaim_special_code, Addr);
	initcode(declaim_type_value_code, Addr);
	initcode(declaim_type_function_code, Addr);
	initcode(declaim_inline_code, Addr);
	initcode(declaim_notinline_code, Addr);
	initcode(declaim_compilation_code, Fixnum);
	initcode(declaim_debug_code, Fixnum);
	initcode(declaim_safety_code, Fixnum);
	initcode(declaim_space_code, Fixnum);
	initcode(declaim_speed_code, Fixnum);
	initcode(declaim_declaration_code, Addr);

	/* let */
	initcode(type_result_code, Addr);
	initcode(type_lexical_code, Addr);
	initcode(type_special_code, Addr);
	initcode(type_global_code, Addr);
	initcode(type_function_code, Addr);
	initcode(type_setf_code, Addr);
	initcode(let_lexical_code, Addr);
	initcode(let_special_code, Addr);
	initcode(leta_special_code, Addr);

	/* setq */
	initcode(setq_lexical_code, Index);
	initcode(setq_special_code, Addr);
	initcode(setq_global_code, Addr);

	/* function */
	initcode(function_set_code, Addr);
	initcode(function_push_code, Addr);
	initcode(setf_set_code, Addr);
	initcode(setf_push_code, Addr);

	/* define */
	initcode(defmacro_code, Addr);
	initcode(deftype_code, Addr);
	initcode(define_compiler_macro_code, Addr);
	initcode(defun_code, Null);

	/* call */
	initcode(call_name_code, Addr);
	initcode(call_result_code, Addr);
	initcode(call_type_code, Addr);
	initcode(call_key_code, Addr);
	initcode(call_function_code, Addr);
	initcode(call_setf_code, Addr);
	initcode(call_lexical_code, Index);

	/* values */
	initcode(values_nil_code, Null);
	initcode(values_set_code, Null);
	initcode(the_set_code, Addr);
	initcode(the_push_code, Addr);

	/* control */
	initcode(if_unbound_code, Index);
	initcode(if_nil_code, Index);
	initcode(if_t_code, Index);
	initcode(goto_code, Index);
	initcode(go_code, Index);
	initcode(return_from_code, Index);
	initcode(catch_code, Addr);
	initcode(throw_operator_code, Addr);
	initcode(taginfo_code, Addr);
	initcode(blockinfo_code, Addr);
	initcode(unwind_protect_code, Addr);

	/* control-switch */
	initcode(handler_bind_code, Null);
	initcode(handler_case_code, Null);
	initcode(restart_bind_code, Null);
	initcode(restart_case_code, Null);

	/* eval */
	initcode(funcall_code, Addr);
	initcode(nth_value_code, Null);
	initcode(progv_code, Null);

	/* lambda */
	initcode(pop_code, Null);
	initcode(pop_unbound_code, Null);
	initcode(getf_code, Addr);
	initcode(rest_code, Null);
	initcode(allow_other_keys_code, Addr);
	initcode(rest_null_code, Null);
	initcode(whole_code, Null);
	initcode(lambda_code, Addr);
	initcode(lambda_name_code, Addr);
	initcode(lambda_type_code, Addr);
	initcode(lambda_doc_code, Addr);
	initcode(lambda_form_code, Addr);
	initcode(lambda_defun_code, Addr);
	initcode(lambda_closure_code, Addr);
	initcode(lambda_lexical_code, Addr);
	initcode(lambda_cache_code, Addr);
	initcode(lambda_cache_set_code, Addr);
	initcode(macro_code, Addr);
	initcode(macro_special_code, Addr);
	initcode(macro_env_code, Null);
	initcode(macro_whole_code, Null);

	/* labels */
	initcode(labels_make_code, Addr);
	initcode(labels_lambda_code, Addr);

	/* multiple-value-bind */
	initcode(bind1_type_code, Addr);
	initcode(bind1_special_code, Addr);
	initcode(bind1_lexical_code, Addr);
	initcode(bind2_type_code, Addr);
	initcode(bind2_special_code, Addr);
	initcode(bind2_lexical_code, Index);

	/* load-time-value */
	initcode(load_alloc_code, Index);
	initcode(load_gensym_code, Addr);
	initcode(load_set_code, Index);
	initcode(reference_set_code, Addr);
	initcode(reference_push_code, Addr);

	/* step */
	initcode(step_code, Addr);
}


/*
 *  build
 */
void build_code_init(void)
{
	/* system */
	defcode(NOP, nop_code);
	defcode(EXECUTE_CONTROL_SET, execute_control_set_code);
	defcode(EXECUTE_CONTROL_PUSH, execute_control_push_code);
	defcode(EXECUTE_CONTROL_SAVE, execute_control_save_code);

	/* object */
	defcode(SET, set_code);
	defcode(PUSH, push_code);
	defcode(PUSH_RESULT, push_result_code);
	defcode(PUSH_VALUES, push_values_code);
	defcode(NIL_SET, nil_set_code);
	defcode(NIL_PUSH, nil_push_code);
	defcode(T_SET, t_set_code);
	defcode(T_PUSH, t_push_code);

	/* symbol */
	defcode(LEXICAL, lexical_code);
	defcode(LEXICAL_SET, lexical_set_code);
	defcode(LEXICAL_PUSH, lexical_push_code);
	defcode(LEXICAL_REM, lexical_rem_code);
	defcode(SPECIAL_SET, special_set_code);
	defcode(SPECIAL_PUSH, special_push_code);
	defcode(SPECIAL_REM, special_rem_code);

	/* declaim */
	defcode(DECLAIM_SPECIAL, declaim_special_code);
	defcode(DECLAIM_TYPE_VALUE, declaim_type_value_code);
	defcode(DECLAIM_TYPE_FUNCTION, declaim_type_function_code);
	defcode(DECLAIM_INLINE, declaim_inline_code);
	defcode(DECLAIM_NOTINLINE, declaim_notinline_code);
	defcode(DECLAIM_COMPILATION, declaim_compilation_code);
	defcode(DECLAIM_DEBUG, declaim_debug_code);
	defcode(DECLAIM_SAFETY, declaim_safety_code);
	defcode(DECLAIM_SPACE, declaim_space_code);
	defcode(DECLAIM_SPEED, declaim_speed_code);
	defcode(DECLAIM_DECLARATION, declaim_declaration_code);

	/* let */
	defcode(TYPE_RESULT, type_result_code);
	defcode(TYPE_LEXICAL, type_lexical_code);
	defcode(TYPE_SPECIAL, type_special_code);
	defcode(TYPE_GLOBAL, type_global_code);
	defcode(TYPE_FUNCTION, type_function_code);
	defcode(TYPE_SETF, type_setf_code);
	defcode(LET_LEXICAL, let_lexical_code);
	defcode(LET_SPECIAL, let_special_code);
	defcode(LETA_SPECIAL, leta_special_code);

	/* setq */
	defcode(SETQ_LEXICAL, setq_lexical_code);
	defcode(SETQ_SPECIAL, setq_special_code);
	defcode(SETQ_GLOBAL, setq_global_code);

	/* function */
	defcode(FUNCTION_SET, function_set_code);
	defcode(FUNCTION_PUSH, function_push_code);
	defcode(SETF_SET, setf_set_code);
	defcode(SETF_PUSH, setf_push_code);

	/* define */
	defcode(DEFMACRO, defmacro_code);
	defcode(DEFTYPE, deftype_code);
	defcode(DEFINE_COMPILER_MACRO, define_compiler_macro_code);
	defcode(DEFUN, defun_code);

	/* call */
	defcode(CALL_NAME, call_name_code);
	defcode(CALL_RESULT, call_result_code);
	defcode(CALL_TYPE, call_type_code);
	defcode(CALL_KEY, call_key_code);
	defcode(CALL_FUNCTION, call_function_code);
	defcode(CALL_SETF, call_setf_code);
	defcode(CALL_LEXICAL, call_lexical_code);

	/* values */
	defcode(VALUES_NIL, values_nil_code);
	defcode(VALUES_SET, values_set_code);
	defcode(THE_SET, the_set_code);
	defcode(THE_PUSH, the_push_code);

	/* control */
	defcode(IF_UNBOUND, if_unbound_code);
	defcode(IF_NIL, if_nil_code);
	defcode(IF_T, if_t_code);
	defcode(GOTO, goto_code);
	defcode(GO, go_code);
	defcode(RETURN_FROM, return_from_code);
	defcode(CATCH, catch_code);
	defcode(THROW, throw_operator_code);
	defcode(TAGINFO, taginfo_code);
	defcode(BLOCKINFO, blockinfo_code);
	defcode(UNWIND_PROTECT, unwind_protect_code);

	/* control-switch */
	defcode(HANDLER_BIND, handler_bind_code);
	defcode(HANDLER_CASE, handler_case_code);
	defcode(RESTART_BIND, restart_bind_code);
	defcode(RESTART_CASE, restart_case_code);

	/* eval */
	defcode(FUNCALL, funcall_code);
	defcode(NTH_VALUE, nth_value_code);
	defcode(PROGV, progv_code);

	/* lambda */
	defcode(POP, pop_code);
	defcode(POP_UNBOUND, pop_unbound_code);
	defcode(GETF, getf_code);
	defcode(REST, rest_code);
	defcode(ALLOW_OTHER_KEYS, allow_other_keys_code);
	defcode(REST_NULL, rest_null_code);
	defcode(WHOLE, whole_code);
	defcode(LAMBDA, lambda_code);
	defcode(LAMBDA_NAME, lambda_name_code);
	defcode(LAMBDA_TYPE, lambda_type_code);
	defcode(LAMBDA_DOC, lambda_doc_code);
	defcode(LAMBDA_FORM, lambda_form_code);
	defcode(LAMBDA_DEFUN, lambda_defun_code);
	defcode(LAMBDA_CLOSURE, lambda_closure_code);
	defcode(LAMBDA_LEXICAL, lambda_lexical_code);
	defcode(LAMBDA_CACHE, lambda_cache_code);
	defcode(LAMBDA_CACHE_SET, lambda_cache_set_code);
	defcode(MACRO, macro_code);
	defcode(MACRO_SPECIAL, macro_special_code);
	defcode(MACRO_ENV, macro_env_code);
	defcode(MACRO_WHOLE, macro_whole_code);

	/* labels */
	defcode(LABELS_MAKE, labels_make_code);
	defcode(LABELS_LAMBDA, labels_lambda_code);

	/* multiple-value-bind */
	defcode(BIND1_TYPE, bind1_type_code);
	defcode(BIND1_SPECIAL, bind1_special_code);
	defcode(BIND1_LEXICAL, bind1_lexical_code);
	defcode(BIND2_TYPE, bind2_type_code);
	defcode(BIND2_SPECIAL, bind2_special_code);
	defcode(BIND2_LEXICAL, bind2_lexical_code);

	/* load-time-value */
	defcode(LOAD_ALLOC, load_alloc_code);
	defcode(LOAD_GENSYM, load_gensym_code);
	defcode(LOAD_SET, load_set_code);
	defcode(REFERENCE_SET, reference_set_code);
	defcode(REFERENCE_PUSH, reference_push_code);

	/* step */
	defcode(STEP, step_code);
}

#undef defcode
#undef initcode

