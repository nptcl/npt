#include "compile_typedef.h"
#include "constant.h"
#include "pointer.h"

_g enum FaslCode CompileWrite[p_size_code];
_g constindex CompileRead[FaslCode_size];

#define defwrite(x) (CompileWrite[p_##x##_code] = FaslCode_##x)
#define defread(x,y) (CompileRead[FaslCode_##x] = CONSTANT_CODE_##y)

static void init_compile_typedef_write(void)
{
	defwrite(nop);
	defwrite(execute_control_set);
	defwrite(execute_control_push);
	defwrite(execute_control_save);
	defwrite(execute_switch_set);
	defwrite(execute_switch_push);

	defwrite(set);
	defwrite(push);
	defwrite(push_result);
	defwrite(push_values);
	defwrite(nil_set);
	defwrite(nil_push);
	defwrite(t_set);
	defwrite(t_push);

	defwrite(lexical);
	defwrite(lexical_set);
	defwrite(lexical_push);
	defwrite(lexical_rem);
	defwrite(special_set);
	defwrite(special_push);
	defwrite(special_rem);

	defwrite(declaim_special);
	defwrite(declaim_type_value);
	defwrite(declaim_type_function);
	defwrite(declaim_inline);
	defwrite(declaim_notinline);
	defwrite(declaim_compilation);
	defwrite(declaim_debug);
	defwrite(declaim_safety);
	defwrite(declaim_space);
	defwrite(declaim_speed);
	defwrite(declaim_declaration);

	defwrite(type_result);
	defwrite(type_lexical);
	defwrite(type_special);
	defwrite(type_global);
	defwrite(type_function);
	defwrite(type_setf);
	defwrite(let_lexical);
	defwrite(let_special);
	defwrite(leta_special);

	defwrite(setq_lexical);
	defwrite(setq_special);
	defwrite(setq_global);

	defwrite(function_set);
	defwrite(function_push);
	defwrite(setf_set);
	defwrite(setf_push);

	defwrite(defmacro);
	defwrite(deftype);
	defwrite(define_compiler_macro);
	defwrite(define_symbol_macro);
	defwrite(defun);

	defwrite(call_result);
	defwrite(call_type);
	defwrite(call_function);
	defwrite(call_setf);
	defwrite(call_lexical);

	defwrite(values_nil);
	defwrite(values_set);
	defwrite(the_set);
	defwrite(the_push);

	defwrite(if_unbound);
	defwrite(if_nil);
	defwrite(if_t);
	defwrite(goto);
	defwrite(go);
	defwrite(return_from);
	defwrite(catch);
	defwrite(throw_operator);
	defwrite(taginfo);
	defwrite(blockinfo);
	defwrite(unwind_protect);

	defwrite(handler_bind);
	defwrite(handler_case);
	defwrite(restart_bind);
	defwrite(restart_case);

	defwrite(funcall);
	defwrite(nth_value);
	defwrite(progv);

	defwrite(pop);
	defwrite(pop_unbound);
	defwrite(getf);
	defwrite(rest);
	defwrite(allow_other_keys);
	defwrite(rest_null);
	defwrite(whole);

	defwrite(lambda);
	defwrite(lambda_name);
	defwrite(lambda_type);
	defwrite(lambda_doc);
	defwrite(lambda_form);
	defwrite(lambda_defun);
	defwrite(lambda_closure);
	defwrite(lambda_lexical);
	defwrite(macro);
	defwrite(macro_special);
	defwrite(macro_env);
	defwrite(macro_whole);

	defwrite(bind1_type);
	defwrite(bind1_special);
	defwrite(bind1_lexical);
	defwrite(bind2_type);
	defwrite(bind2_special);
	defwrite(bind2_lexical);

	defwrite(load_time_value_bind);
	defwrite(load_time_value_init);
	defwrite(load_time_value_set);
	defwrite(load_time_value_push);

	defwrite(optcode_result_type);
	defwrite(optcode_car0_set);
	defwrite(optcode_car0_push);
	defwrite(optcode_car1_set);
	defwrite(optcode_car1_push);
	defwrite(optcode_cdr0_set);
	defwrite(optcode_cdr0_push);
	defwrite(optcode_cdr1_set);
	defwrite(optcode_cdr1_push);
	defwrite(optcode_cons);
}

static void init_compile_typedef_read(void)
{
	defread(nop, NOP);
	defread(execute_control_set, EXECUTE_CONTROL_SET);
	defread(execute_control_push, EXECUTE_CONTROL_PUSH);
	defread(execute_control_save, EXECUTE_CONTROL_SAVE);
	defread(execute_switch_set, EXECUTE_SWITCH_SET);
	defread(execute_switch_push, EXECUTE_SWITCH_PUSH);

	defread(set, SET);
	defread(push, PUSH);
	defread(push_result, PUSH_RESULT);
	defread(push_values, PUSH_VALUES);
	defread(nil_set, NIL_SET);
	defread(nil_push, NIL_PUSH);
	defread(t_set, T_SET);
	defread(t_push, T_PUSH);

	defread(lexical, LEXICAL);
	defread(lexical_set, LEXICAL_SET);
	defread(lexical_push, LEXICAL_PUSH);
	defread(lexical_rem, LEXICAL_REM);
	defread(special_set, SPECIAL_SET);
	defread(special_push, SPECIAL_PUSH);
	defread(special_rem, SPECIAL_REM);

	defread(declaim_special, DECLAIM_SPECIAL);
	defread(declaim_type_value, DECLAIM_TYPE_VALUE);
	defread(declaim_type_function, DECLAIM_TYPE_FUNCTION);
	defread(declaim_inline, DECLAIM_INLINE);
	defread(declaim_notinline, DECLAIM_NOTINLINE);
	defread(declaim_compilation, DECLAIM_COMPILATION);
	defread(declaim_debug, DECLAIM_DEBUG);
	defread(declaim_safety, DECLAIM_SAFETY);
	defread(declaim_space, DECLAIM_SPACE);
	defread(declaim_speed, DECLAIM_SPEED);
	defread(declaim_declaration, DECLAIM_DECLARATION);

	defread(type_result, TYPE_RESULT);
	defread(type_lexical, TYPE_LEXICAL);
	defread(type_special, TYPE_SPECIAL);
	defread(type_global, TYPE_GLOBAL);
	defread(type_function, TYPE_FUNCTION);
	defread(type_setf, TYPE_SETF);
	defread(let_lexical, LET_LEXICAL);
	defread(let_special, LET_SPECIAL);
	defread(leta_special, LETA_SPECIAL);

	defread(setq_lexical, SETQ_LEXICAL);
	defread(setq_special, SETQ_SPECIAL);
	defread(setq_global, SETQ_GLOBAL);

	defread(function_set, FUNCTION_SET);
	defread(function_push, FUNCTION_PUSH);
	defread(setf_set, SETF_SET);
	defread(setf_push, SETF_PUSH);

	defread(defmacro, DEFMACRO);
	defread(deftype, DEFTYPE);
	defread(define_compiler_macro, DEFINE_COMPILER_MACRO);
	defread(define_symbol_macro, DEFINE_SYMBOL_MACRO);
	defread(defun, DEFUN);

	defread(call_result, CALL_RESULT);
	defread(call_type, CALL_TYPE);
	defread(call_function, CALL_FUNCTION);
	defread(call_setf, CALL_SETF);
	defread(call_lexical, CALL_LEXICAL);

	defread(values_nil, VALUES_NIL);
	defread(values_set, VALUES_SET);
	defread(the_set, THE_SET);
	defread(the_push, THE_PUSH);

	defread(if_unbound, IF_UNBOUND);
	defread(if_nil, IF_NIL);
	defread(if_t, IF_T);
	defread(goto, GOTO);
	defread(go, GO);
	defread(return_from, RETURN_FROM);
	defread(catch, CATCH);
	defread(throw_operator, THROW);
	defread(taginfo, TAGINFO);
	defread(blockinfo, BLOCKINFO);
	defread(unwind_protect, UNWIND_PROTECT);

	defread(handler_bind, HANDLER_BIND);
	defread(handler_case, HANDLER_CASE);
	defread(restart_bind, RESTART_BIND);
	defread(restart_case, RESTART_CASE);

	defread(funcall, FUNCALL);
	defread(nth_value, NTH_VALUE);
	defread(progv, PROGV);

	defread(pop, POP);
	defread(pop_unbound, POP_UNBOUND);
	defread(getf, GETF);
	defread(rest, REST);
	defread(allow_other_keys, ALLOW_OTHER_KEYS);
	defread(rest_null, REST_NULL);
	defread(whole, WHOLE);

	defread(lambda, LAMBDA);
	defread(lambda_name, LAMBDA_NAME);
	defread(lambda_type, LAMBDA_TYPE);
	defread(lambda_doc, LAMBDA_DOC);
	defread(lambda_form, LAMBDA_FORM);
	defread(lambda_defun, LAMBDA_DEFUN);
	defread(lambda_closure, LAMBDA_CLOSURE);
	defread(lambda_lexical, LAMBDA_LEXICAL);
	defread(macro, MACRO);
	defread(macro_special, MACRO_SPECIAL);
	defread(macro_env, MACRO_ENV);
	defread(macro_whole, MACRO_WHOLE);

	defread(bind1_type, BIND1_TYPE);
	defread(bind1_special, BIND1_SPECIAL);
	defread(bind1_lexical, BIND1_LEXICAL);
	defread(bind2_type, BIND2_TYPE);
	defread(bind2_special, BIND2_SPECIAL);
	defread(bind2_lexical, BIND2_LEXICAL);

	defread(load_time_value_bind, LOAD_TIME_VALUE_BIND);
	defread(load_time_value_init, LOAD_TIME_VALUE_INIT);
	defread(load_time_value_set, LOAD_TIME_VALUE_SET);
	defread(load_time_value_push, LOAD_TIME_VALUE_PUSH);

	defread(optcode_result_type, OPTCODE_RESULT_TYPE);
	defread(optcode_car0_set, OPTCODE_CAR0_SET);
	defread(optcode_car0_push, OPTCODE_CAR0_PUSH);
	defread(optcode_car1_set, OPTCODE_CAR1_SET);
	defread(optcode_car1_push, OPTCODE_CAR1_PUSH);
	defread(optcode_cdr0_set, OPTCODE_CDR0_SET);
	defread(optcode_cdr0_push, OPTCODE_CDR0_PUSH);
	defread(optcode_cdr1_set, OPTCODE_CDR1_SET);
	defread(optcode_cdr1_push, OPTCODE_CDR1_PUSH);
	defread(optcode_cons, OPTCODE_CONS);
}
#undef defwrite
#undef defread

_g enum FaslCode get_compile_write(pointer id)
{
	Check(p_size_code <= id, "p_size_code error");
	return CompileWrite[id];
}

_g constindex get_compile_read(enum FaslCode id)
{
	return CompileRead[id];
}

_g void init_compile_typedef(void)
{
	init_compile_typedef_write();
	init_compile_typedef_read();
}

