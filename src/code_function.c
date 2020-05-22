#include "call_eval.h"
#include "callname.h"
#include "code_lambda.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "declare.h"
#include "execute.h"
#include "execute_object.h"
#include "eval.h"
#include "eval_table.h"
#include "function.h"
#include "gc.h"
#include "info.h"
#include "integer.h"
#include "lambda.h"
#include "restart_value.h"
#include "symbol.h"
#include "type_deftype.h"

/*****************************************************************************
 *  code function
 *****************************************************************************/
static int nop_code(Execute ptr, addr pos)
{
	return 0;
}

static int execute_simple_set_code(Execute ptr, addr pos)
{
	return runcode_simple(ptr, pos);
}

static int execute_normal_set_code(Execute ptr, addr pos)
{
	return runcode_normal(ptr, pos);
}

static int execute_control_set_code(Execute ptr, addr pos)
{
	return runcode_control(ptr, pos);
}

static int execute_switch_set_code(Execute ptr, addr pos)
{
	return runcode_switch(ptr, pos);
}

static int execute_simple_push_code(Execute ptr, addr pos)
{
	Return(runcode_simple(ptr, pos));
	getresult_control(ptr, &pos);
	pushargs_control(ptr, pos);
	return 0;
}

static int execute_normal_push_code(Execute ptr, addr pos)
{
	Return(runcode_normal(ptr, pos));
	getresult_control(ptr, &pos);
	pushargs_control(ptr, pos);
	return 0;
}

static int execute_control_push_code(Execute ptr, addr pos)
{
	Return(runcode_control(ptr, pos));
	getresult_control(ptr, &pos);
	pushargs_control(ptr, pos);
	return 0;
}

static int execute_switch_push_code(Execute ptr, addr pos)
{
	Return(runcode_switch(ptr, pos));
	getresult_control(ptr, &pos);
	pushargs_control(ptr, pos);
	return 0;
}


/*
 *  object
 */
static int set_code(Execute ptr, addr pos)
{
	setresult_control(ptr, pos);
	return 0;
}

static int push_code(Execute ptr, addr pos)
{
	pushargs_control(ptr, pos);
	return 0;
}

static int push_result_code(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	pushargs_control(ptr, pos);
	return 0;
}

static int push_values_code(Execute ptr, addr pos)
{
	pushargs_allvalues(ptr);
	return 0;
}

static int nil_set_code(Execute ptr, addr pos)
{
	setresult_control(ptr, Nil);
	return 0;
}

static int nil_push_code(Execute ptr, addr pos)
{
	pushargs_control(ptr, Nil);
	return 0;
}

static int t_set_code(Execute ptr, addr pos)
{
	setresult_control(ptr, T);
	return 0;
}

static int t_push_code(Execute ptr, addr pos)
{
	pushargs_control(ptr, T);
	return 0;
}


/*
 *  symbol
 */
static int lexical_code(Execute ptr, addr list)
{
	addr pos;
	size_t index;

	/* allocate */
	GetCons(list, &pos, &list);
	GetIndex(pos, &index);
	lexical_control(ptr, index);

	/* closure */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_INDEX);
		GetIndex(pos, &index);
		reference_lexical_control(ptr, index);
	}

	return 0;
}

static int lexical_set_code(Execute ptr, addr pos)
{
	size_t index;

	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);
	setresult_control(ptr, pos);

	return 0;
}

static int lexical_push_code(Execute ptr, addr pos)
{
	size_t index;

	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

static int lexical_rem_code(Execute ptr, addr pos)
{
#ifdef LISP_DEBUG
	size_t index;

	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);
#endif

	return 0;
}

static int special_set_code(Execute ptr, addr pos)
{
	Return(symbol_special_restart(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
}

static int special_push_code(Execute ptr, addr pos)
{
	Return(symbol_special_restart(ptr, pos, &pos));
	pushargs_control(ptr, pos);
	return 0;
}

static int special_rem_code(Execute ptr, addr pos)
{
	return symbol_special_restart(ptr, pos, &pos);
}


/*
 *  declaim
 */
static int declaim_special_code(Execute ptr, addr right)
{
	setspecial_symbol(right);
	return 0;
}

static int declaim_type_value_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	settype_value_symbol(symbol, type);
	return 0;
}

static int declaim_type_function_code(Execute ptr, addr right)
{
	addr key, symbol, type;

	List_bind(right, &key, &type, NULL);
	GetCallName(key, &symbol);
	if (symbolp_callname(key))
		settype_function_symbol(symbol, type);
	else
		settype_setf_symbol(symbol, type);

	return 0;
}

static int declaim_inline_code(Execute ptr, addr right)
{
	addr symbol;

	GetCallName(right, &symbol);
	if (symbolp_callname(right))
		setinline_function_symbol(symbol);
	else
		setinline_setf_symbol(symbol);

	return 0;
}

static int declaim_notinline_code(Execute ptr, addr right)
{
	addr symbol;

	GetCallName(right, &symbol);
	if (symbolp_callname(right))
		setnotinline_function_symbol(symbol);
	else
		setnotinline_setf_symbol(symbol);

	return 0;
}

static int declaim_compilation_code(Execute ptr, addr right)
{
	apply_compilation_speed_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_debug_code(Execute ptr, addr right)
{
	apply_debug_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_safety_code(Execute ptr, addr right)
{
	apply_safety_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_space_code(Execute ptr, addr right)
{
	apply_space_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_speed_code(Execute ptr, addr right)
{
	apply_speed_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_declaration_code(Execute ptr, addr right)
{
	push_declaration_declaim(right);
	return 0;
}


/*
 *  let
 */
static int let_args_code(Execute ptr, addr list)
{
	addr args, pos, value, symbol, type;
	size_t index;

	/* value */
	args = list;
	while (args != Nil) {
		GetCons(args, &pos, &args);
		GetCons(args, &value, &args);
		Return(runcode_simple(ptr, value));
		getresult_control(ptr, &value);
		/* type check */
		if (getcheck_tablevalue(pos)) {
			gettype_tablevalue(pos, &type);
			Return(typep_error(ptr, value, type));
		}
		/* setlet */
		index = getlet_tablevalue(pos);
		set_lexical_control(ptr, index, value);
	}

	/* bind */
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCdr(list, &list);
		/* getlet */
		index = getlet_tablevalue(pos);
		get_lexical_control(ptr, index, &value);
		/* bind */
		if (getspecialp_tablevalue(pos)) {
			getname_tablevalue(pos, &symbol);
			pushspecial_control(ptr, symbol, value);
		}
		else {
			index = getlexical_tablevalue(pos);
			set_lexical_control(ptr, index, value);
		}
	}

	return 0;
}

static int let_set_code(Execute ptr, addr list)
{
	addr control, args, body, free, allocate;

	List_bind(list, &args, &body, &free, &allocate, NULL);
	if (allocate != Nil)
		push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(let_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	if (allocate != Nil)
		return free_control_(ptr, control);

	return 0;
}

static int let_push_code(Execute ptr, addr list)
{
	Return(let_set_code(ptr, list));
	getresult_control(ptr, &list);
	pushargs_control(ptr, list);

	return 0;
}

static int leta_args_code(Execute ptr, addr list)
{
	addr pos, value, symbol, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(list, &value, &list);
		getname_tablevalue(pos, &symbol);
		Return(runcode_simple(ptr, value));
		getresult_control(ptr, &value);
		/* type check */
		if (getcheck_tablevalue(pos)) {
			gettype_tablevalue(pos, &type);
			Return(typep_error(ptr, value, type));
		}
		/* bind */
		if (getspecialp_tablevalue(pos))
			pushspecial_control(ptr, symbol, value);
		else
			setvalue_tablevalue(ptr, pos, value);
	}

	return 0;
}

static int leta_set_code(Execute ptr, addr list)
{
	addr control, args, body, free, allocate;

	List_bind(list, &args, &body, &free, &allocate, NULL);
	if (allocate != Nil)
		push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(leta_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	if (allocate != Nil)
		return free_control_(ptr, control);

	return 0;
}

static int leta_push_code(Execute ptr, addr list)
{
	Return(leta_set_code(ptr, list));
	getresult_control(ptr, &list);
	pushargs_control(ptr, list);

	return 0;
}


/*
 *  setq
 */
static int check_readonly_variable_(addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set value to the constant variable ~S.", symbol, NULL);
	return 0;
}

static int setq_set_code(Execute ptr, addr list)
{
	addr pos, value, symbol, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(list, &value, &list);
		/* readonly */
		getname_tablevalue(pos, &symbol);
		Return(check_readonly_variable_(symbol));
		/* value */
		Return(runcode_simple(ptr, value));
		getresult_control(ptr, &value);
		/* bind */
		if (getcheck_tablevalue(pos)) {
			gettype_tablevalue(pos, &type);
			Return(typep_error(ptr, value, type));
		}
		if (getspecialp_tablevalue(pos)) {
			setspecial_local(ptr, symbol, value);
		}
		else if (getglobalp_tablevalue(pos)) {
			SetValueSymbol(symbol, value);
		}
		else {
			setvalue_tablevalue(ptr, pos, value);
		}
	}

	return 0;
}

static int setq_push_code(Execute ptr, addr list)
{
	Return(setq_set_code(ptr, list));
	getresult_control(ptr, &list);
	pushargs_control(ptr, list);

	return 0;
}


/*
 *  function
 */
static int function_set_code(Execute ptr, addr right)
{
	Return(function_global_restart(ptr, right, &right));
	setresult_control(ptr, right);
	return 0;
}

static int function_push_code(Execute ptr, addr right)
{
	Return(function_global_restart(ptr, right, &right));
	pushargs_control(ptr, right);
	return 0;
}

static int setf_set_code(Execute ptr, addr right)
{
	Return(setf_global_restart(ptr, right, &right));
	setresult_control(ptr, right);
	return 0;
}

static int setf_push_code(Execute ptr, addr right)
{
	Return(setf_global_restart(ptr, right, &right));
	pushargs_control(ptr, right);
	return 0;
}

static int defmacro_code(Execute ptr, addr right)
{
	addr symbol, value;

	List_bind(right, &symbol, &value, NULL);
	setmacro_symbol(symbol, value);
	setresult_control(ptr, symbol);

	return 0;
}

static int deftype_code(Execute ptr, addr right)
{
	addr pos, symbol, doc;

	List_bind(right, &symbol, &doc, NULL);
	getresult_control(ptr, &pos);
	setdocumentation_function(pos, doc);
	setdeftype(symbol, pos);
	setresult_control(ptr, symbol);

	return 0;
}

static int define_compiler_macro_code(Execute ptr, addr right)
{
	addr pos, name, doc;

	List_bind(right, &name, &doc, NULL);
	getresult_control(ptr, &pos);
	setdocumentation_function(pos, doc);
	set_define_compiler_macro(name, pos);
	name_callname_heap(name, &name);
	setresult_control(ptr, name);

	return 0;
}

static int define_symbol_macro_code(Execute ptr, addr right)
{
	addr symbol, eval, form;

	List_bind(right, &symbol, &eval, &form, NULL);
	Check(! symbolp(symbol), "type error");
	setsymbol_macro_symbol(symbol, eval, form);
	setresult_control(ptr, symbol);

	return 0;
}

static int defun_code(Execute ptr, addr right)
{
	addr pos, call, symbol;

	getresult_control(ptr, &pos);
	GetNameFunction(pos, &call);
	GetCallName(call, &symbol);

	if (symbolp_callname(call)) {
		SetFunctionSymbol(symbol, pos);
		setresult_control(ptr, symbol);
	}
	else {
		setsetf_symbol(symbol, pos);
		GetConst(COMMON_SETF, &pos);
		list_heap(&pos, pos, symbol, NULL);
		setresult_control(ptr, pos);
	}

	return 0;
}

static int call_code(Execute ptr, addr pos)
{
	return execute_control(ptr, pos);
}

static int call_result_code(Execute ptr, addr pos)
{
	getresult_control(ptr, &pos);
	return execute_control(ptr, pos);
}

static int call_type_code(Execute ptr, addr pos)
{
	addr value;
	getargs_tail_control(ptr, &value);
	return typep_error(ptr, value, pos);
}

static int call_function_code(Execute ptr, addr pos)
{
	addr value;

	GetFunctionSymbol(pos, &value);
	if (value == Unbound) {
		Return(function_global_restart(ptr, pos, &value));
	}

	return execute_control(ptr, value);
}

static int call_setf_code(Execute ptr, addr pos)
{
	addr value;

	getsetf_symbol(pos, &value);
	if (value == Unbound) {
		Return(setf_global_restart(ptr, pos, &value));
	}

	return execute_control(ptr, value);
}

static int call_lexical_code(Execute ptr, addr pos)
{
	addr value;
	size_t index;

	GetIndex(pos, &index);
	getlow_lexical_control(ptr, index, &value);

	return execute_control(ptr, value);
}

static int values_nil_code(Execute ptr, addr right)
{
	setvalues_nil_control(ptr);
	return 0;
}

static int values_set_code(Execute ptr, addr right)
{
	getargs_list_control_unsafe(ptr, 0, &right);
	setvalues_list_control(ptr, right);
	return 0;
}

static int the_set_code(Execute ptr, addr type)
{
	addr value;
	getresult_control(ptr, &value);
	return typep_error(ptr, value, type);
}

static int the_push_code(Execute ptr, addr type)
{
	addr value;
	getargs_control(ptr, 0, &value);
	return typep_error(ptr, value, type);
}

static int if_code(Execute ptr, addr list)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos == Nil) {
		GetCdr(list, &list);
	}
	GetCar(list, &pos);
	return runcode_simple(ptr, pos);
}

static int goto_code(Execute ptr, addr right)
{
	return goto_control_(ptr, RefIndex(right));
}

static int go_code(Execute ptr, addr pos)
{
	size_t index;

	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);
	return go_control_(ptr, pos);
}

static int return_from_code(Execute ptr, addr pos)
{
	size_t index;

	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);
	return return_from_control_(ptr, pos);
}

static int catch_code(Execute ptr, addr right)
{
	getresult_control(ptr, &right);
	catch_control(ptr, right);
	return 0;
}

static int throw_operator_code(Execute ptr, addr right)
{
	getargs_control(ptr, 0, &right);
	return throw_control_(ptr, right);
}

static void push_handler_code(Execute ptr, int escape)
{
	addr args, symbol, lambda;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &symbol, &args);
		GetCons(args, &lambda, &args);
		pushhandler_common(ptr, symbol, lambda, escape);
	}
	reverse_handler_control(ptr);
}

static int handler_bind_code(Execute ptr, addr right)
{
	push_handler_code(ptr, 0);
	return 0;
}

static int handler_case_code(Execute ptr, addr right)
{
	push_handler_code(ptr, 1);
	return 0;
}

static void push_restart_code(Execute ptr, int escape)
{
	addr args, list;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &list, &args);
		pushbind_restart_control(ptr, list, escape);
	}
	reverse_restart_control(ptr);
}

static int restart_bind_code(Execute ptr, addr right)
{
	push_restart_code(ptr, 0);
	return 0;
}

static int restart_case_code(Execute ptr, addr right)
{
	push_restart_code(ptr, 1);
	return 0;
}

static int prog1_set_code(Execute ptr, addr list)
{
	addr protect, cleanup, control, values;
	size_t size;

	GetCons(list, &protect, &list);
	GetCar(list, &cleanup);
	push_new_control(ptr, &control);
	Return(runcode_simple(ptr, protect));
	save_values_control(ptr, &values, &size);
	Return(runcode_simple(ptr, cleanup));
	restore_values_control(ptr, values, size);

	return free_control_(ptr, control);
}

static int prog1_push_code(Execute ptr, addr list)
{
	Return(prog1_set_code(ptr, list));
	getresult_control(ptr, &list);
	pushargs_control(ptr, list);

	return 0;
}

static int funcall_code(Execute ptr, addr right)
{
	getargs_list_control_unsafe(ptr, 0, &right);
	return call_control(ptr, right);
}

static int nth_value_code(Execute ptr, addr right)
{
	addr nth;
	size_t index;

	getargs_control(ptr, 0, &nth);
	if (! integerp(nth))
		return fmte_("NTH-VALUE argument ~S must be integer type.", nth, NULL);
	if (! zerop_or_plusp_integer(nth))
		return fmte_("NTH-VALUE argument ~S must be greater than equal to 0.", nth, NULL);
	if (GetIndex_integer(nth, &index)) {
		setresult_control(ptr, Nil);
	}
	else {
		getvalues_control(ptr, index, &right);
		setresult_control(ptr, (right == Unbound)? Nil: right);
	}

	return 0;
}

static int progv_code(Execute ptr, addr right)
{
	addr symbols, values, symbol, value;

	getargs_control(ptr, 0, &symbols);
	getargs_control(ptr, 1, &values);
	while (symbols != Nil) {
		if (! consp(symbols))
			return fmte_("PROGV form ~S must be a cons.", symbols, NULL);
		GetCons(symbols, &symbol, &symbols);
		if (! symbolp(symbol))
			return fmte_("PROGV argument ~S must be a symbol.", symbol, NULL);
		if (values == Nil) {
			pushspecial_control(ptr, symbol, Unbound);
		}
		else if (consp(values)) {
			GetCons(values, &value, &values);
			pushspecial_control(ptr, symbol, value);
		}
		else {
			return fmte_("PROGV form ~S must be a cons.", values, NULL);
		}
	}

	return 0;
}

static int taginfo_code(Execute ptr, addr list)
{
	set_taginfo_control(ptr, list);
	return 0;
}

static int blockinfo_code(Execute ptr, addr pos)
{
	set_blockinfo_control(ptr, pos);
	return 0;
}

static int unwind_protect_code(Execute ptr, addr pos)
{
	set_protect_control(ptr, pos);
	return 0;
}


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
	setsystem_function(pos);
	SetFunctionSymbol(symbol, pos);
}
#define defcode(x,y) defcode_constant(CONSTANT_CODE_##x, p_##y)
#define initcode(x) SetPointer_code(p_##x, x)

_g void init_code_function(void)
{
	/* system */
	initcode(nop_code);
	initcode(execute_simple_set_code);
	initcode(execute_normal_set_code);
	initcode(execute_control_set_code);
	initcode(execute_switch_set_code);
	initcode(execute_simple_push_code);
	initcode(execute_normal_push_code);
	initcode(execute_control_push_code);
	initcode(execute_switch_push_code);

	/* object */
	initcode(set_code);
	initcode(push_code);
	initcode(push_result_code);
	initcode(push_values_code);
	initcode(nil_set_code);
	initcode(nil_push_code);
	initcode(t_set_code);
	initcode(t_push_code);

	/* symbol */
	initcode(lexical_code);
	initcode(lexical_set_code);
	initcode(lexical_push_code);
	initcode(lexical_rem_code);
	initcode(special_set_code);
	initcode(special_push_code);
	initcode(special_rem_code);

	/* declaim */
	initcode(declaim_special_code);
	initcode(declaim_type_value_code);
	initcode(declaim_type_function_code);
	initcode(declaim_inline_code);
	initcode(declaim_notinline_code);
	initcode(declaim_compilation_code);
	initcode(declaim_debug_code);
	initcode(declaim_safety_code);
	initcode(declaim_space_code);
	initcode(declaim_speed_code);
	initcode(declaim_declaration_code);

	/* let */
	initcode(let_set_code);
	initcode(let_push_code);
	initcode(leta_set_code);
	initcode(leta_push_code);

	/* setq */
	initcode(setq_set_code);
	initcode(setq_push_code);

	/* function */
	initcode(function_set_code);
	initcode(function_push_code);
	initcode(setf_set_code);
	initcode(setf_push_code);

	/* lambda */
	initcode(lambda_set_code);
	initcode(lambda_push_code);
	initcode(lambda_execute_code);

	initcode(macro_set_code);
	initcode(macro_push_code);
	initcode(macro_execute_code);

	initcode(bind_set_code);
	initcode(bind_push_code);

	initcode(defun_code);
	initcode(defmacro_code);
	initcode(deftype_code);
	initcode(define_compiler_macro_code);
	initcode(define_symbol_macro_code);
	initcode(flet_set_code);
	initcode(flet_push_code);
	initcode(labels_set_code);
	initcode(labels_push_code);

	initcode(call_code);
	initcode(call_result_code);
	initcode(call_type_code);
	initcode(call_function_code);
	initcode(call_setf_code);
	initcode(call_lexical_code);

	initcode(values_nil_code);
	initcode(values_set_code);
	initcode(the_set_code);
	initcode(the_push_code);
	initcode(locally_declare_code);
	initcode(if_code);
	initcode(goto_code);
	initcode(go_code);
	initcode(return_from_code);
	initcode(catch_code);
	initcode(throw_operator_code);
	initcode(handler_bind_code);
	initcode(handler_case_code);
	initcode(restart_bind_code);
	initcode(restart_case_code);

	initcode(bind_values_set_code);
	initcode(bind_values_push_code);
	initcode(prog1_set_code);
	initcode(prog1_push_code);
	initcode(funcall_code);
	initcode(nth_value_code);
	initcode(progv_code);

	initcode(taginfo_code);
	initcode(blockinfo_code);
	initcode(unwind_protect_code);
}

_g void build_code_function(void)
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

	/* lambda */
	defcode(LAMBDA_SET, lambda_set_code);
	defcode(LAMBDA_PUSH, lambda_push_code);
	defcode(LAMBDA_EXECUTE, lambda_execute_code);

	defcode(MACRO_SET, macro_set_code);
	defcode(MACRO_PUSH, macro_push_code);
	defcode(MACRO_EXECUTE, macro_execute_code);

	defcode(BIND_SET, bind_set_code);
	defcode(BIND_PUSH, bind_push_code);

	defcode(DEFUN, defun_code);
	defcode(DEFMACRO, defmacro_code);
	defcode(DEFTYPE, deftype_code);
	defcode(DEFINE_COMPILER_MACRO, define_compiler_macro_code);
	defcode(DEFINE_SYMBOL_MACRO, define_symbol_macro_code);
	defcode(FLET_SET, flet_set_code);
	defcode(FLET_PUSH, flet_push_code);
	defcode(LABELS_SET, labels_set_code);
	defcode(LABELS_PUSH, labels_push_code);
	defcode(CALL, call_code);
	defcode(CALL_RESULT, call_result_code);
	defcode(CALL_TYPE, call_type_code);
	defcode(CALL_FUNCTION, call_function_code);
	defcode(CALL_SETF, call_setf_code);
	defcode(CALL_LEXICAL, call_lexical_code);

	defcode(VALUES_NIL, values_nil_code);
	defcode(VALUES_SET, values_set_code);
	defcode(THE_SET, the_set_code);
	defcode(THE_PUSH, the_push_code);
	defcode(LOCALLY_DECLARE, locally_declare_code);
	defcode(IF, if_code);
	defcode(GOTO, goto_code);
	defcode(GO, go_code);
	defcode(RETURN_FROM, return_from_code);
	defcode(CATCH, catch_code);
	defcode(THROW, throw_operator_code);
	defcode(HANDLER_BIND, handler_bind_code);
	defcode(HANDLER_CASE, handler_case_code);
	defcode(RESTART_BIND, restart_bind_code);
	defcode(RESTART_CASE, restart_case_code);

	defcode(BIND_VALUES_SET, bind_values_set_code);
	defcode(BIND_VALUES_PUSH, bind_values_push_code);
	defcode(PROG1_SET, prog1_set_code);
	defcode(PROG1_PUSH, prog1_push_code);
	defcode(FUNCALL, funcall_code);
	defcode(NTH_VALUE, nth_value_code);
	defcode(PROGV, progv_code);

	defcode(TAGINFO, taginfo_code);
	defcode(BLOCKINFO, blockinfo_code);
	defcode(UNWIND_PROTECT, unwind_protect_code);
}

#undef defcode
#undef initcode

