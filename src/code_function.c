#include "call_eval.h"
#include "callname.h"
#include "code_lambda.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "declare.h"
#include "document.h"
#include "eval_table.h"
#include "execute.h"
#include "execute_object.h"
#include "execute_values.h"
#include "function.h"
#include "integer.h"
#include "load_time_value.h"
#include "restart_value.h"
#include "step_prompt.h"
#include "symbol.h"
#include "type_deftype.h"

/*
 *  system
 */
_g int nop_code(Execute ptr, CodeValue x)
{
	return 0;
}

_g int execute_control_set_code(Execute ptr, CodeValue x)
{
	return runcode_control(ptr, x.pos);
}

_g int execute_control_push_code(Execute ptr, CodeValue x)
{
	Return(runcode_control(ptr, x.pos));
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}

_g int execute_control_save_code(Execute ptr, CodeValue x)
{
	addr control, values;
	size_t size;

	push_new_control(ptr, &control);
	save_values_control(ptr, &values, &size);
	Return(runcode_control(ptr, x.pos));
	restore_values_control(ptr, values, size);
	return free_control_(ptr, control);
}

_g int execute_switch_set_code(Execute ptr, CodeValue x)
{
	return runcode_switch(ptr, x.pos);
}

_g int execute_switch_push_code(Execute ptr, CodeValue x)
{
	Return(runcode_switch(ptr, x.pos));
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}


/*
 *  object
 */
_g int set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, x.pos);
	return 0;
}

_g int push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, x.pos);
	return 0;
}

_g int push_result_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}

_g int push_values_code(Execute ptr, CodeValue x)
{
	pushargs_allvalues(ptr);
	return 0;
}

_g int nil_set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, Nil);
	return 0;
}

_g int nil_push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, Nil);
	return 0;
}

_g int t_set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, T);
	return 0;
}

_g int t_push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, T);
	return 0;
}


/*
 *  symbol
 */
_g int lexical_code(Execute ptr, CodeValue x)
{
	addr list, pos;
	size_t index;

	/* allocate */
	GetCons(x.pos, &pos, &list);
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

_g int lexical_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	get_lexical_control(ptr, x.index, &pos);
	setresult_control(ptr, pos);

	return 0;
}

_g int lexical_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	get_lexical_control(ptr, x.index, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

_g int lexical_rem_code(Execute ptr, CodeValue x)
{
#ifdef LISP_DEBUG
	get_lexical_control(ptr, x.index, &x.pos);
#endif
	return 0;
}

_g int special_set_code(Execute ptr, CodeValue x)
{
	Return(symbol_special_restart(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

_g int special_push_code(Execute ptr, CodeValue x)
{
	Return(symbol_special_restart(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}

_g int special_rem_code(Execute ptr, CodeValue x)
{
	return symbol_special_restart(ptr, x.pos, &x.pos);
}


/*
 *  declaim
 */
_g int declaim_special_code(Execute ptr, CodeValue x)
{
	return setspecial_symbol_(x.pos);
}

_g int declaim_type_value_code(Execute ptr, CodeValue x)
{
	addr symbol, type;

	List_bind(x.pos, &symbol, &type, NULL);
	return settype_value_symbol_(symbol, type);
}

_g int declaim_type_function_code(Execute ptr, CodeValue x)
{
	addr key, symbol, type;

	List_bind(x.pos, &key, &type, NULL);
	GetCallName(key, &symbol);
	if (symbolp_callname(key))
		return settype_function_symbol_(symbol, type);
	else
		return settype_setf_symbol_(symbol, type);
}

_g int declaim_inline_code(Execute ptr, CodeValue x)
{
	addr symbol;

	GetCallName(x.pos, &symbol);
	if (symbolp_callname(x.pos))
		setinline_function_symbol(symbol);
	else
		setinline_setf_symbol(symbol);

	return 0;
}

_g int declaim_notinline_code(Execute ptr, CodeValue x)
{
	addr symbol;

	GetCallName(x.pos, &symbol);
	if (symbolp_callname(x.pos))
		setnotinline_function_symbol(symbol);
	else
		setnotinline_setf_symbol(symbol);

	return 0;
}

_g int declaim_compilation_code(Execute ptr, CodeValue x)
{
	apply_compilation_speed_declaim((OptimizeType)x.value);
	return 0;
}

_g int declaim_debug_code(Execute ptr, CodeValue x)
{
	apply_debug_declaim((OptimizeType)x.value);
	return 0;
}

_g int declaim_safety_code(Execute ptr, CodeValue x)
{
	apply_safety_declaim((OptimizeType)x.value);
	return 0;
}

_g int declaim_space_code(Execute ptr, CodeValue x)
{
	apply_space_declaim((OptimizeType)x.value);
	return 0;
}

_g int declaim_speed_code(Execute ptr, CodeValue x)
{
	apply_speed_declaim((OptimizeType)x.value);
	return 0;
}

_g int declaim_declaration_code(Execute ptr, CodeValue x)
{
	push_declaration_declaim(x.pos);
	return 0;
}


/*
 *  let
 */
_g int type_result_code(Execute ptr, CodeValue x)
{
	/* the-set-code */
	addr value;
	getresult_control(ptr, &value);
	return typep_error(ptr, value, x.pos);
}

_g int type_lexical_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	size_t index;

	List_bind(x.pos, &pos, &type, NULL);
	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);

	return typep_error(ptr, pos, type);
}

_g int type_special_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	getspecial_local(ptr, pos, &pos);
	return (pos == Unbound)? 0: typep_error(ptr, pos, type);
}

_g int type_global_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	GetValueSymbol(pos, &pos);
	return (pos == Unbound)? 0: typep_error(ptr, pos, type);
}

_g int type_function_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	GetFunctionSymbol(pos, &pos);
	return (pos == Unbound)? 0: typep_error(ptr, pos, type);
}

_g int type_setf_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	getsetf_symbol(pos, &pos);
	return (pos == Unbound)? 0: typep_error(ptr, pos, type);
}

_g int let_lexical_code(Execute ptr, CodeValue x)
{
	addr src, dst, value;
	size_t srci, dsti;

	List_bind(x.pos, &src, &dst, NULL);
	GetIndex(src, &srci);
	GetIndex(dst, &dsti);
	/* src -> dst */
	get_lexical_control(ptr, srci, &value);
	set_lexical_control(ptr, dsti, value);

	return 0;
}

_g int let_special_code(Execute ptr, CodeValue x)
{
	addr src, dst, value;
	size_t index;

	List_bind(x.pos, &src, &dst, NULL);
	GetIndex(src, &index);
	/* src -> dst */
	get_lexical_control(ptr, index, &value);
	pushspecial_control(ptr, dst, value);

	return 0;
}

_g int leta_special_code(Execute ptr, CodeValue x)
{
	addr value;
	getresult_control(ptr, &value);
	pushspecial_control(ptr, x.pos, value);
	return 0;
}


/*
 *  setq
 */
#define check_readonly_variable_(x) { \
	if (GetStatusReadOnly(x)) { \
		return fmte_("Cannot set value to the constant variable ~S.", x, NULL); \
	} \
}

_g int setq_lexical_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	set_lexical_control(ptr, x.index, value);

	return 0;
}

_g int setq_special_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	check_readonly_variable_(x.pos);
	setspecial_local(ptr, x.pos, value);

	return 0;
}

_g int setq_global_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	check_readonly_variable_(x.pos);
	SetValueSymbol(x.pos, value);

	return 0;
}


/*
 *  function
 */
_g int function_set_code(Execute ptr, CodeValue x)
{
	Return(function_global_restart(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

_g int function_push_code(Execute ptr, CodeValue x)
{
	Return(function_global_restart(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}

_g int setf_set_code(Execute ptr, CodeValue x)
{
	Return(setf_global_restart(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

_g int setf_push_code(Execute ptr, CodeValue x)
{
	Return(setf_global_restart(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}


/*
 *  define
 */
_g int defmacro_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	Return(setmacro_symbol_(x.pos, value));
	setresult_control(ptr, x.pos);

	return 0;
}

_g int deftype_code(Execute ptr, CodeValue x)
{
	addr pos, symbol, doc;

	List_bind(x.pos, &symbol, &doc, NULL);
	getresult_control(ptr, &pos);
	Return(setdeftype_(symbol, pos));
	setdocumentation_function(pos, doc);
	setresult_control(ptr, symbol);

	return 0;
}

_g int define_compiler_macro_code(Execute ptr, CodeValue x)
{
	addr pos, name, doc;

	List_bind(x.pos, &name, &doc, NULL);
	getresult_control(ptr, &pos);
	setdocumentation_function(pos, doc);
	Return(set_define_compiler_macro(name, pos));
	name_callname_heap(name, &name);
	setresult_control(ptr, name);

	return 0;
}

_g int define_symbol_macro_code(Execute ptr, CodeValue x)
{
	addr symbol, eval, form;

	List_bind(x.pos, &symbol, &eval, &form, NULL);
	Check(! symbolp(symbol), "type error");
	Return(setsymbol_macro_symbol_(symbol, eval, form));
	setresult_control(ptr, symbol);

	return 0;
}

_g int defun_code(Execute ptr, CodeValue x)
{
	addr pos, call, symbol;

	getresult_control(ptr, &pos);
	GetNameFunction(pos, &call);
	GetCallName(call, &symbol);

	if (symbolp_callname(call)) {
		Return(setfunction_symbol_(symbol, pos));
		setresult_control(ptr, symbol);
	}
	else {
		Return(setsetf_symbol_(symbol, pos));
		GetConst(COMMON_SETF, &pos);
		list_heap(&pos, pos, symbol, NULL);
		setresult_control(ptr, pos);
	}

	return 0;
}


/*
 *  call
 */
_g int call_result_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	return execute_control(ptr, x.pos);
}

_g int call_type_code(Execute ptr, CodeValue x)
{
	addr value;
	getargs_tail_control(ptr, &value);
	return typep_error(ptr, value, x.pos);
}

_g int call_function_code(Execute ptr, CodeValue x)
{
	addr value;

	GetFunctionSymbol(x.pos, &value);
	if (value == Unbound) {
		Return(function_global_restart(ptr, x.pos, &value));
	}

	return execute_control(ptr, value);
}

_g int call_setf_code(Execute ptr, CodeValue x)
{
	addr value;

	getsetf_symbol(x.pos, &value);
	if (value == Unbound) {
		Return(setf_global_restart(ptr, x.pos, &value));
	}

	return execute_control(ptr, value);
}

_g int call_lexical_code(Execute ptr, CodeValue x)
{
	addr value;
	getlow_lexical_control(ptr, x.index, &value);
	return execute_control(ptr, value);
}


/*
 *  values
 */
_g int values_nil_code(Execute ptr, CodeValue x)
{
	setvalues_nil_control(ptr);
	return 0;
}

_g int values_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getargs_list_control_unsafe(ptr, 0, &pos);
	setvalues_list_control(ptr, pos);

	return 0;
}

_g int the_set_code(Execute ptr, CodeValue x)
{
	addr value;
	getresult_control(ptr, &value);
	return typep_error(ptr, value, x.pos);
}

_g int the_push_code(Execute ptr, CodeValue x)
{
	addr value;
	getargs_control(ptr, 0, &value);
	return typep_error(ptr, value, x.pos);
}


/*
 *  control
 */
_g int if_unbound_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos == Unbound)
		return goto_control_(ptr, x.index);

	return 0;
}

_g int if_nil_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos == Nil)
		return goto_control_(ptr, x.index);

	return 0;
}

_g int if_t_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos != Nil)
		return goto_control_(ptr, x.index);

	return 0;
}

_g int goto_code(Execute ptr, CodeValue x)
{
	return goto_control_(ptr, x.index);
}

_g int go_code(Execute ptr, CodeValue x)
{
	addr pos;
	get_lexical_control(ptr, x.index, &pos);
	return go_control_(ptr, pos);
}

_g int return_from_code(Execute ptr, CodeValue x)
{
	addr pos;
	get_lexical_control(ptr, x.index, &pos);
	return return_from_control_(ptr, pos);
}

_g int catch_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	catch_control(ptr, x.pos);
	return 0;
}

_g int throw_operator_code(Execute ptr, CodeValue x)
{
	getargs_control(ptr, 0, &x.pos);
	return throw_control_(ptr, x.pos);
}

_g int taginfo_code(Execute ptr, CodeValue x)
{
	set_taginfo_control(ptr, x.pos);
	return 0;
}

_g int blockinfo_code(Execute ptr, CodeValue x)
{
	set_blockinfo_control(ptr, x.pos);
	return 0;
}

_g int unwind_protect_code(Execute ptr, CodeValue x)
{
	set_protect_control(ptr, x.pos);
	return 0;
}


/*
 *  control-switch
 */
static int push_handler_code_(Execute ptr, int escape)
{
	addr args, symbol, lambda;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &symbol, &args);
		GetCons(args, &lambda, &args);
		Return(pushhandler_common_(ptr, symbol, lambda, escape));
	}
	reverse_handler_control(ptr);

	return 0;
}

_g int handler_bind_code(Execute ptr, CodeValue x)
{
	return push_handler_code_(ptr, 0);
}

_g int handler_case_code(Execute ptr, CodeValue x)
{
	return push_handler_code_(ptr, 1);
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

_g int restart_bind_code(Execute ptr, CodeValue x)
{
	push_restart_code(ptr, 0);
	return 0;
}

_g int restart_case_code(Execute ptr, CodeValue x)
{
	push_restart_code(ptr, 1);
	return 0;
}


/*
 *  eval
 */
_g int funcall_code(Execute ptr, CodeValue x)
{
	getargs_list_control_unsafe(ptr, 0, &x.pos);
	return call_control(ptr, x.pos);
}

_g int nth_value_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos;
	size_t index;

	getargs_control(ptr, 0, &pos);
	if (! integerp(pos))
		return fmte_("NTH-VALUE argument ~S must be integer type.", pos, NULL);
	Return(zerop_or_plusp_integer_(pos, &check));
	if (! check)
		return fmte_("NTH-VALUE argument ~S must be greater than equal to 0.", pos, NULL);
	if (GetIndex_integer(pos, &index)) {
		setresult_control(ptr, Nil);
	}
	else {
		getvalues_control(ptr, index, &pos);
		setresult_control(ptr, (pos == Unbound)? Nil: pos);
	}

	return 0;
}

_g int progv_code(Execute ptr, CodeValue x)
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


/*
 *  load-time-value
 */
_g int load_time_value_bind_code(Execute ptr, CodeValue x)
{
	execute_load_time_value_bind(ptr, x.pos);
	return 0;
}

_g int load_time_value_init_code(Execute ptr, CodeValue x)
{
	execute_load_time_value_init(ptr, x.pos);
	return 0;
}

_g int load_time_value_set_code(Execute ptr, CodeValue x)
{
	execute_load_time_value_get(ptr, x.pos, &x.pos);
	setresult_control(ptr, x.pos);
	return 0;
}

_g int load_time_value_push_code(Execute ptr, CodeValue x)
{
	execute_load_time_value_get(ptr, x.pos, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}


/*
 *  step
 */
_g int step_code(Execute ptr, CodeValue x)
{
	addr expr, value;
	List_bind(x.pos, &expr, &value, NULL);
	return execute_step_code(ptr, expr, value);
}

