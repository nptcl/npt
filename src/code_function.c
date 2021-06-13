#include "call_eval.h"
#include "callname.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "code_function.h"
#include "code_lambda.h"
#include "code_values.h"
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
#include "load_code.h"
#include "load_object.h"
#include "restart_value.h"
#include "step_prompt.h"
#include "strtype.h"
#include "symbol.h"
#include "type_deftype.h"

/*
 *  system
 */
int nop_code(Execute ptr, CodeValue x)
{
	return 0;
}

int execute_control_set_code(Execute ptr, CodeValue x)
{
	return runcode_control_(ptr, x.pos);
}

int execute_control_push_code(Execute ptr, CodeValue x)
{
	Return(runcode_control_(ptr, x.pos));
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}

int execute_control_save_code(Execute ptr, CodeValue x)
{
	addr control, values;
	size_t size;

	push_control(ptr, &control);
	save_values_control(ptr, &values, &size);
	if (runcode_control_(ptr, x.pos) == 0)
		restore_values_control(ptr, values, size);
	return pop_control_(ptr, control);
}


/*
 *  object
 */
int set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, x.pos);
	return 0;
}

int push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, x.pos);
	return 0;
}

int push_result_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	pushargs_control(ptr, x.pos);
	return 0;
}

int push_values_code(Execute ptr, CodeValue x)
{
	pushargs_allvalues(ptr);
	return 0;
}

int nil_set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, Nil);
	return 0;
}

int nil_push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, Nil);
	return 0;
}

int t_set_code(Execute ptr, CodeValue x)
{
	setresult_control(ptr, T);
	return 0;
}

int t_push_code(Execute ptr, CodeValue x)
{
	pushargs_control(ptr, T);
	return 0;
}


/*
 *  symbol
 */
int lexical_code(Execute ptr, CodeValue x)
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

int lexical_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	get_lexical_control(ptr, x.index, &pos);
	setresult_control(ptr, pos);

	return 0;
}

int lexical_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	get_lexical_control(ptr, x.index, &pos);
	pushargs_control(ptr, pos);

	return 0;
}

int lexical_rem_code(Execute ptr, CodeValue x)
{
#ifdef LISP_DEBUG
	get_lexical_control(ptr, x.index, &x.pos);
#endif
	return 0;
}

int special_set_code(Execute ptr, CodeValue x)
{
	Return(symbol_special_restart(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

int special_push_code(Execute ptr, CodeValue x)
{
	Return(symbol_special_restart(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}

int special_rem_code(Execute ptr, CodeValue x)
{
	return symbol_special_restart(ptr, x.pos, &x.pos);
}


/*
 *  declaim
 */
int declaim_special_code(Execute ptr, CodeValue x)
{
	return setspecial_symbol_(x.pos);
}

int declaim_type_value_code(Execute ptr, CodeValue x)
{
	addr symbol, type;

	List_bind(x.pos, &symbol, &type, NULL);
	return settype_value_symbol_(symbol, type);
}

int declaim_type_function_code(Execute ptr, CodeValue x)
{
	addr key, symbol, type;

	List_bind(x.pos, &key, &type, NULL);
	GetCallName(key, &symbol);
	if (symbolp_callname(key))
		return settype_function_symbol_(symbol, type);
	else
		return settype_setf_symbol_(symbol, type);
}

int declaim_inline_code(Execute ptr, CodeValue x)
{
	addr symbol;

	GetCallName(x.pos, &symbol);
	if (symbolp_callname(x.pos))
		setinline_function_symbol(symbol);
	else
		setinline_setf_symbol(symbol);

	return 0;
}

int declaim_notinline_code(Execute ptr, CodeValue x)
{
	addr symbol;

	GetCallName(x.pos, &symbol);
	if (symbolp_callname(x.pos))
		setnotinline_function_symbol(symbol);
	else
		setnotinline_setf_symbol(symbol);

	return 0;
}

int declaim_compilation_code(Execute ptr, CodeValue x)
{
	apply_compilation_speed_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_debug_code(Execute ptr, CodeValue x)
{
	apply_debug_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_safety_code(Execute ptr, CodeValue x)
{
	apply_safety_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_space_code(Execute ptr, CodeValue x)
{
	apply_space_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_speed_code(Execute ptr, CodeValue x)
{
	apply_speed_declaim((OptimizeType)x.value);
	return 0;
}

int declaim_declaration_code(Execute ptr, CodeValue x)
{
	push_declaration_declaim(x.pos);
	return 0;
}


/*
 *  let
 */
int type_result_code(Execute ptr, CodeValue x)
{
	/* the-set-code */
	addr value;
	getresult_control(ptr, &value);
	return call_typep_error_(ptr, value, x.pos);
}

int type_lexical_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	size_t index;

	List_bind(x.pos, &pos, &type, NULL);
	GetIndex(pos, &index);
	get_lexical_control(ptr, index, &pos);

	return call_typep_error_(ptr, pos, type);
}

int type_special_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	getspecial_local(ptr, pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int type_global_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	GetValueSymbol(pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int type_function_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	GetFunctionSymbol(pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int type_setf_code(Execute ptr, CodeValue x)
{
	addr pos, type;
	List_bind(x.pos, &pos, &type, NULL);
	getsetf_symbol(pos, &pos);
	return call_typep_unbound_error_(ptr, pos, type);
}

int let_lexical_code(Execute ptr, CodeValue x)
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

int let_special_code(Execute ptr, CodeValue x)
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

int leta_special_code(Execute ptr, CodeValue x)
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

int setq_lexical_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	set_lexical_control(ptr, x.index, value);

	return 0;
}

int setq_special_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	check_readonly_variable_(x.pos);
	setspecial_local(ptr, x.pos, value);

	return 0;
}

int setq_global_code(Execute ptr, CodeValue x)
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
int function_set_code(Execute ptr, CodeValue x)
{
	Return(function_global_restart(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

int function_push_code(Execute ptr, CodeValue x)
{
	Return(function_global_restart(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}

int setf_set_code(Execute ptr, CodeValue x)
{
	Return(setf_global_restart(ptr, x.pos, &x.pos));
	setresult_control(ptr, x.pos);
	return 0;
}

int setf_push_code(Execute ptr, CodeValue x)
{
	Return(setf_global_restart(ptr, x.pos, &x.pos));
	pushargs_control(ptr, x.pos);
	return 0;
}


/*
 *  define
 */
int defmacro_code(Execute ptr, CodeValue x)
{
	addr value;

	getresult_control(ptr, &value);
	Return(setmacro_symbol_(x.pos, value));
	setresult_control(ptr, x.pos);

	return 0;
}

int deftype_code(Execute ptr, CodeValue x)
{
	addr pos, symbol, doc;

	List_bind(x.pos, &symbol, &doc, NULL);
	getresult_control(ptr, &pos);
	Return(setdeftype_(symbol, pos));
	Return(set_documentation_function_object_(pos, doc));
	setresult_control(ptr, symbol);

	return 0;
}

int define_compiler_macro_code(Execute ptr, CodeValue x)
{
	addr pos, name, doc;

	List_bind(x.pos, &name, &doc, NULL);
	getresult_control(ptr, &pos);
	Return(set_documentation_function_object_(pos, doc));
	Return(set_define_compiler_macro(name, pos));
	name_callname_heap(name, &name);
	setresult_control(ptr, name);

	return 0;
}

int defun_code(Execute ptr, CodeValue x)
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
int call_name_code(Execute ptr, CodeValue x)
{
	SetControl(ptr->control, Control_Call, x.pos);
	return 0;
}

int call_result_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	return execute_control(ptr, x.pos);
}

int call_type_code(Execute ptr, CodeValue x)
{
	addr value;
	getargs_tail_control(ptr, &value);
	return call_typep_error_(ptr, value, x.pos);
}

int call_key_code(Execute ptr, CodeValue x)
{
	addr list, key, value, pos, type;

	list = x.pos;
	getresult_control(ptr, &value);
	getargs_tail_control(ptr, &key);

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (pos == key) {
			return call_typep_asterisk_error_(ptr, value, type);
		}
	}

	return fmte_("Invalid argument key ~S.", key, NULL);
}

int call_function_code(Execute ptr, CodeValue x)
{
	addr value;

	GetFunctionSymbol(x.pos, &value);
	if (value == Unbound) {
		Return(function_global_restart(ptr, x.pos, &value));
	}

	return execute_control(ptr, value);
}

int call_setf_code(Execute ptr, CodeValue x)
{
	addr value;

	getsetf_symbol(x.pos, &value);
	if (value == Unbound) {
		Return(setf_global_restart(ptr, x.pos, &value));
	}

	return execute_control(ptr, value);
}

int call_lexical_code(Execute ptr, CodeValue x)
{
	addr value;
	getlow_lexical_control(ptr, x.index, &value);
	return execute_control(ptr, value);
}


/*
 *  values
 */
int values_nil_code(Execute ptr, CodeValue x)
{
	setvalues_nil_control(ptr);
	return 0;
}

int values_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	getargs_list_control_unsafe(ptr, 0, &pos);
	setvalues_list_control(ptr, pos);

	return 0;
}

int the_set_code(Execute ptr, CodeValue x)
{
	return values_typep_error_(ptr, x.pos);
}

int the_push_code(Execute ptr, CodeValue x)
{
	addr value;

	Return(the_set_code(ptr, x));
	getresult_control(ptr, &value);
	pushargs_control(ptr, value);

	return 0;
}


/*
 *  control
 */
int if_unbound_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos == Unbound)
		return goto_control_(ptr, x.index);

	return 0;
}

int if_nil_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos == Nil)
		return goto_control_(ptr, x.index);

	return 0;
}

int if_t_code(Execute ptr, CodeValue x)
{
	addr pos;

	getresult_control(ptr, &pos);
	if (pos != Nil)
		return goto_control_(ptr, x.index);

	return 0;
}

int goto_code(Execute ptr, CodeValue x)
{
	return goto_control_(ptr, x.index);
}

int go_code(Execute ptr, CodeValue x)
{
	addr pos;
	get_lexical_control(ptr, x.index, &pos);
	return go_control_(ptr, pos);
}

int return_from_code(Execute ptr, CodeValue x)
{
	addr pos;
	get_lexical_control(ptr, x.index, &pos);
	return return_from_control_(ptr, pos);
}

int catch_code(Execute ptr, CodeValue x)
{
	getresult_control(ptr, &x.pos);
	catch_control(ptr, x.pos);
	return 0;
}

int throw_operator_code(Execute ptr, CodeValue x)
{
	getargs_control(ptr, 0, &x.pos);
	return throw_control_(ptr, x.pos);
}

int taginfo_code(Execute ptr, CodeValue x)
{
	set_taginfo_control(ptr, x.pos);
	return 0;
}

int blockinfo_code(Execute ptr, CodeValue x)
{
	set_blockinfo_control(ptr, x.pos);
	return 0;
}

int unwind_protect_code(Execute ptr, CodeValue x)
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

int handler_bind_code(Execute ptr, CodeValue x)
{
	return push_handler_code_(ptr, 0);
}

int handler_case_code(Execute ptr, CodeValue x)
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

int restart_bind_code(Execute ptr, CodeValue x)
{
	push_restart_code(ptr, 0);
	return 0;
}

int restart_case_code(Execute ptr, CodeValue x)
{
	push_restart_code(ptr, 1);
	return 0;
}


/*
 *  eval
 */
int funcall_code(Execute ptr, CodeValue x)
{
	int check;
	addr pos, list;

	/* multiple-value-call only used. */
	getargs_list_control_unsafe(ptr, 0, &pos);
	GetCons(pos, &pos, &list);
	Return(funcallp_(pos, &check));
	if (! check)
		return TypeError_(pos, FUNCTION);

	return apply_control(ptr, pos, list);
}

int nth_value_code(Execute ptr, CodeValue x)
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

int progv_code(Execute ptr, CodeValue x)
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
int load_alloc_code(Execute ptr, CodeValue x)
{
	execute_load_alloc(ptr, x.index);
	return 0;
}

int load_gensym_code(Execute ptr, CodeValue x)
{
	addr pos, index;
	size_t value;

	List_bind(x.pos, &pos, &index, NULL);
	GetIndex(index, &value);
	return execute_load_gensym_(ptr, pos, value);
}

int load_set_code(Execute ptr, CodeValue x)
{
	return execute_load_set_(ptr, x.index);
}

int reference_set_code(Execute ptr, CodeValue x)
{
	addr pos;

	Return(result_load_time_value_(ptr, x.pos, &pos));
	setresult_control(ptr, pos);

	return 0;
}

int reference_push_code(Execute ptr, CodeValue x)
{
	addr pos;

	Return(result_load_time_value_(ptr, x.pos, &pos));
	pushargs_control(ptr, pos);

	return 0;
}


/*
 *  step
 */
int step_code(Execute ptr, CodeValue x)
{
	addr expr, value;
	List_bind(x.pos, &expr, &value, NULL);
	return execute_step_code(ptr, expr, value);
}

