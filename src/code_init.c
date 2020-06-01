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

_g byte CodeValueArray[p_size_code];

/*
 *  initialize
 */
_g void defcode_constant(constindex index, pointer p)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Check(symbol == Nil || symbol == Unbound, "constant error");
	GetFunctionSymbol(symbol, &pos);
	Check(pos != Unbound, "code-function already exists.");
	compiled_heap(&pos, symbol);
	setcompiled_code(pos, p);
	SetFunctionSymbol(symbol, pos);
}

_g void init_code_init(void)
{
	cleartype(CodeValueArray);

	/* system */
	initcode(nop_code, Null);
	initcode(execute_simple_set_code, Addr);
	initcode(execute_normal_set_code, Addr);
	initcode(execute_control_set_code, Addr);
	initcode(execute_switch_set_code, Addr);
	initcode(execute_simple_push_code, Addr);
	initcode(execute_normal_push_code, Addr);
	initcode(execute_control_push_code, Addr);
	initcode(execute_switch_push_code, Addr);

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
	initcode(let_set_code, Addr);
	initcode(let_push_code, Addr);
	initcode(leta_set_code, Addr);
	initcode(leta_push_code, Addr);

	/* setq */
	initcode(setq_set_code, Addr);
	initcode(setq_push_code, Addr);

	/* function */
	initcode(function_set_code, Addr);
	initcode(function_push_code, Addr);
	initcode(setf_set_code, Addr);
	initcode(setf_push_code, Addr);

	/* define */
	initcode(defmacro_code, Addr);
	initcode(deftype_code, Addr);
	initcode(define_compiler_macro_code, Addr);
	initcode(define_symbol_macro_code, Addr);
	initcode(defun_code, Null);

	/* call */
	initcode(call_code, Addr);
	initcode(call_result_code, Addr);
	initcode(call_type_code, Addr);
	initcode(call_function_code, Addr);
	initcode(call_setf_code, Addr);
	initcode(call_lexical_code, Index);

	/* values */
	initcode(values_nil_code, Null);
	initcode(values_set_code, Null);
	initcode(the_set_code, Addr);
	initcode(the_push_code, Addr);

	/* control */
	initcode(if_code, Addr);
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
	initcode(prog1_set_code, Addr);
	initcode(prog1_push_code, Addr);
	initcode(funcall_code, Addr);
	initcode(nth_value_code, Null);
	initcode(progv_code, Null);

	/* lambda */
	initcode(lambda_set_code, Addr);
	initcode(lambda_push_code, Addr);
	initcode(lambda_execute_code, Addr);
	initcode(macro_set_code, Addr);
	initcode(macro_push_code, Addr);
	initcode(macro_execute_code, Addr);
	initcode(bind_set_code, Addr);
	initcode(bind_push_code, Addr);
	initcode(flet_set_code, Addr);
	initcode(flet_push_code, Addr);
	initcode(labels_set_code, Addr);
	initcode(labels_push_code, Addr);
	initcode(locally_declare_code, Addr);
	initcode(bind_values_set_code, Addr);
	initcode(bind_values_push_code, Addr);

	/* load-time-value */
	initcode(load_time_value_alloc_code, Index);
	initcode(load_time_value_value_code, Addr);
	initcode(load_time_value_set_code, Index);
	initcode(load_time_value_push_code, Index);
}


/*
 *  build
 */
_g void build_code_init(void)
{
	/* system */
	defcode(NOP, nop_code);
	defcode(EXECUTE_SIMPLE_SET, execute_simple_set_code);
	defcode(EXECUTE_NORMAL_SET, execute_normal_set_code);
	defcode(EXECUTE_CONTROL_SET, execute_control_set_code);
	defcode(EXECUTE_SWITCH_SET, execute_switch_set_code);
	defcode(EXECUTE_SIMPLE_PUSH, execute_simple_push_code);
	defcode(EXECUTE_NORMAL_PUSH, execute_normal_push_code);
	defcode(EXECUTE_CONTROL_PUSH, execute_control_push_code);
	defcode(EXECUTE_SWITCH_PUSH, execute_switch_push_code);

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
	defcode(LET_SET, let_set_code);
	defcode(LET_PUSH, let_push_code);
	defcode(LETA_SET, leta_set_code);
	defcode(LETA_PUSH, leta_push_code);

	/* setq */
	defcode(SETQ_SET, setq_set_code);
	defcode(SETQ_PUSH, setq_push_code);

	/* function */
	defcode(FUNCTION_SET, function_set_code);
	defcode(FUNCTION_PUSH, function_push_code);
	defcode(SETF_SET, setf_set_code);
	defcode(SETF_PUSH, setf_push_code);

	/* define */
	defcode(DEFMACRO, defmacro_code);
	defcode(DEFTYPE, deftype_code);
	defcode(DEFINE_COMPILER_MACRO, define_compiler_macro_code);
	defcode(DEFINE_SYMBOL_MACRO, define_symbol_macro_code);
	defcode(DEFUN, defun_code);

	/* call */
	defcode(CALL, call_code);
	defcode(CALL_RESULT, call_result_code);
	defcode(CALL_TYPE, call_type_code);
	defcode(CALL_FUNCTION, call_function_code);
	defcode(CALL_SETF, call_setf_code);
	defcode(CALL_LEXICAL, call_lexical_code);

	/* values */
	defcode(VALUES_NIL, values_nil_code);
	defcode(VALUES_SET, values_set_code);
	defcode(THE_SET, the_set_code);
	defcode(THE_PUSH, the_push_code);

	/* control */
	defcode(IF, if_code);
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
	defcode(PROG1_SET, prog1_set_code);
	defcode(PROG1_PUSH, prog1_push_code);
	defcode(FUNCALL, funcall_code);
	defcode(NTH_VALUE, nth_value_code);
	defcode(PROGV, progv_code);

	/* lambda */
	defcode(LAMBDA_SET, lambda_set_code);
	defcode(LAMBDA_PUSH, lambda_push_code);
	defcode(LAMBDA_EXECUTE, lambda_execute_code);
	defcode(MACRO_SET, macro_set_code);
	defcode(MACRO_PUSH, macro_push_code);
	defcode(MACRO_EXECUTE, macro_execute_code);
	defcode(BIND_SET, bind_set_code);
	defcode(BIND_PUSH, bind_push_code);
	defcode(FLET_SET, flet_set_code);
	defcode(FLET_PUSH, flet_push_code);
	defcode(LABELS_SET, labels_set_code);
	defcode(LABELS_PUSH, labels_push_code);
	defcode(LOCALLY_DECLARE, locally_declare_code);
	defcode(BIND_VALUES_SET, bind_values_set_code);
	defcode(BIND_VALUES_PUSH, bind_values_push_code);

	/* load-time-value */
	defcode(LOAD_TIME_VALUE_ALLOC, load_time_value_alloc_code);
	defcode(LOAD_TIME_VALUE_VALUE, load_time_value_value_code);
	defcode(LOAD_TIME_VALUE_SET, load_time_value_set_code);
	defcode(LOAD_TIME_VALUE_PUSH, load_time_value_push_code);
}

#undef defcode
#undef initcode

