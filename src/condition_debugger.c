#include "clos.h"
#include "clos_class.h"
#include "clos_type.h"
#include "condition.h"
#include "condition_debugger.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_execute.h"
#include "eval_main.h"
#include "execute_object.h"
#include "format.h"
#include "function.h"
#include "hold.h"
#include "integer.h"
#include "integer_calc.h"
#include "pointer.h"
#include "prompt.h"
#include "reader.h"
#include "restart.h"
#include "restart_value.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "stream_string.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "terme.h"
#include "type_object.h"

/* enable-debugger */
static void enable_debugger_symbol(addr *ret)
{
	GetConst(SYSTEM_ENABLE_DEBUGGER, ret);
}

static void init_enable_debugger(void)
{
	addr symbol;
	enable_debugger_symbol(&symbol);
	SetValueSymbol(symbol, T);
}

void set_enable_debugger(Execute ptr, int value)
{
	addr symbol;
	enable_debugger_symbol(&symbol);
	setspecial_local(ptr, symbol, value? T: Nil);
}

static int enable_debugger_p_(Execute ptr, int *ret)
{
	addr symbol, pos;

	enable_debugger_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));

	return Result(ret, pos != Nil);
}

/* index-debugger */
static void index_debugger_symbol(addr *ret)
{
	GetConst(SYSTEM_INDEX_DEBUGGER, ret);
}

static void init_index_debugger(void)
{
	addr symbol, value;

	index_debugger_symbol(&symbol);
	fixnum_heap(&value, 0);
	SetValueSymbol(symbol, value);
}

static int push_index_debugger_(Execute ptr)
{
	addr symbol, value;

	/* increment */
	index_debugger_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &value));
	Return(oneplus_integer_common_(ptr->local, value, &value));
	pushspecial_control(ptr, symbol, value);
	Return(format_string_(ptr, &value, "[~A]* ", value, NULL));

	/* push prompt */
	push_prompt(ptr, value, prompt_debugger);

	return 0;
}


/*
 *  (handler-bind
 *    ((warning #'function-handler-warning))
 *    ...)
 */
static int function_handler_warning(Execute ptr, addr condition)
{
	int check;
	addr pos, stream, format, args;

	Return(error_output_stream_(ptr, &stream));
	GetConst(CONDITION_SIMPLE_WARNING, &pos);
	Return(clos_subtype_p_(condition, pos, &check));
	if (check) {
		Return(simple_condition_format_control_(condition, &format));
		Return(simple_condition_format_arguments_(condition, &args));
		Return(format_stream_(ptr, stream, "~&WARNING: ", NULL));
		Return(format_lisp_(ptr, stream, format, args, &args));
		Return(fresh_line_stream_(stream, NULL));
	}
	else {
		Return(format_stream_(ptr, stream, "~&WARNING: ~S~%", condition, NULL));
	}

	return force_output_stream_(stream);
}

int handler_warning_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_warning);
	return pushhandler_common_(ptr, pos, call, 0);
}


/*
 *  (handler-case
 *    ...
 *    ((system::savecore #'function-handler-empty)))
 */
static int function_handler_empty(Execute ptr, addr condition)
{
	/* do-nothing */
	return 0;
}

int handler_savecore_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_SAVECORE, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_empty);
	return pushhandler_common_(ptr, pos, call, 1);
}


/*
 *  (handler-case
 *    ...
 *    ((system::exit #'function-handler-empty)))
 */
int handler_exit_(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_EXIT, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_empty);
	return pushhandler_common_(ptr, pos, call, 1);
}


/*
 *  debugger
 */
static int output_unbound_variable_(Execute ptr, addr stream, addr condition)
{
	Return(cell_error_name_(condition, &condition));
	return format_stream_(ptr, stream, "Unbound variable ~S.~%", condition, NULL);
}

static int output_undefined_function_(Execute ptr, addr stream, addr condition)
{
	Return(cell_error_name_(condition, &condition));
	return format_stream_(ptr, stream, "Undefined function ~S.~%", condition, NULL);
}

static int output_unbound_slot_(Execute ptr, addr stream, addr condition)
{
	addr instance, name;

	Return(unbound_slot_instance_(condition, &instance));
	Return(cell_error_name_(condition, &name));
	return format_stream_(ptr, stream,
			"The slot ~S is unbound in the ~S.~%", name, instance, NULL);
}

static int output_simple_error_(Execute ptr, addr stream, addr condition)
{
	addr control, arguments;

	Return(simple_condition_format_(condition, &control, &arguments));
	Return(format_stream_lisp_(ptr, stream, control, arguments));
	Return(fresh_line_stream_(stream, NULL));
	Return(terpri_stream_(stream));

	return 0;
}

static int output_type_error_(Execute ptr, addr stream, addr instance)
{
	addr datum, expected;

	Return(type_error_datum_(instance, &datum));
	Return(type_error_expected_(instance, &expected));
	if (GetType(expected) == LISPTYPE_TYPE) {
		Return(type_object_(&expected, expected));
	}
	return format_stream_(ptr, stream,
			"The value ~S must be a ~S type.~%", datum, expected, NULL);
}

static int output_condition_(Execute ptr, addr stream, addr condition)
{
	return 0;
}

static int condition_check_p_(constindex index, addr condition, int *ret)
{
	int check;
	addr super;

	Return(condition_instance_p_(condition, &check));
	if (! check)
		return Result(ret, 0);
	GetConstant(index, &super);
	return clos_subtype_p_(condition, super, ret);
}
#define ConditionCheck_(x,y,r) condition_check_p_(CONSTANT_CONDITION_##x,(y),(r))

static int output_debugger_(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(ConditionCheck_(UNBOUND_VARIABLE, pos, &check));
	if (check)
		return output_unbound_variable_(ptr, stream, pos);
	Return(ConditionCheck_(UNDEFINED_FUNCTION, pos, &check));
	if (check)
		return output_undefined_function_(ptr, stream, pos);
	Return(ConditionCheck_(UNBOUND_SLOT, pos, &check));
	if (check)
		return output_unbound_slot_(ptr, stream, pos);
	Return(ConditionCheck_(SIMPLE_CONDITION, pos, &check));
	if (check)
		return output_simple_error_(ptr, stream, pos);
	Return(ConditionCheck_(TYPE_ERROR, pos, &check));
	if (check)
		return output_type_error_(ptr, stream, pos);
	Return(condition_instance_p_(pos, &check));
	if (check)
		return output_condition_(ptr, stream, pos);
	/* otherwise */
	return format_stream_(ptr, stream, "Invalid condition type ~S~%", pos, NULL);
}

static int invoke_standard_header_(Execute ptr, addr io, addr condition)
{
	addr pos;

	Return(clos_class_of_(condition, &pos));
	Return(stdget_class_name_(pos, &pos));
	(void)text_color_terme(ptr, print_color_bright_red);
	Return(finish_output_stream_(io));
	Return(format_stream_(ptr, io, "~&ERROR: ~S~%", pos, NULL));
	Return(finish_output_stream_(io));
	(void)text_color_terme(ptr, print_color_reset);
	Return(finish_output_stream_(io));
	Return(output_debugger_(ptr, io, condition));

	return 0;
}

static int output_restarts_debugger_(Execute ptr, addr io, addr list)
{
	int check;
	addr pos, symbol, name, str, id;
	size_t index;

	for (index = 0; list != Nil; index++) {
		GetCons(list, &pos, &list);
		CheckType(pos, LISPTYPE_RESTART);
		getname_restart(pos, &symbol);
		getreport_restart(pos, &name);
		if (name != Nil) {
			if (! stringp(name)) {
				open_output_string_stream(&str, 0);
				check = funcall1_control_(ptr, &name, name, str, NULL);
				if (check)
					return fmte_("Invalid restart report.", NULL);
				Return(string_stream_heap_(str, &name));
				close_output_string_stream(str);
			}
		}
		id = intsizeh(index);
		Return(format_stream_(ptr, io, "~2@A. ~16A ~A~%", id, symbol, name, NULL));
	}

	return 0;
}

static int eval_debugger_call_(Execute ptr, addr io, addr eval)
{
	Return(eval_execute_partial_(ptr, eval));
	return eval_loop_output_(ptr, io);
}

static int eval_debugger_(Execute ptr, addr io, addr eval)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_debugger_call_(ptr, io, eval);
	return pop_control_(ptr, control);
}

static int eval_symbol_debugger_(Execute ptr, addr io, addr list, addr eval)
{
	int check;
	addr root, restart, x, y;

	/* eq check */
	root = list;
	while (root != Nil) {
		GetCons(root, &restart, &root);
		getname_restart(restart, &x);
		if (x == eval)
			return invoke_restart_interactively_control_(ptr, restart);
	}

	/* symbol= check */
	Return(string_designator_heap_(&y, eval, &check));
	if (check) {
		root = list;
		while (root != Nil) {
			GetCons(root, &restart, &root);
			getname_restart(restart, &x);
			Return(string_designator_equal_(x, y, &check));
			if (check)
				return invoke_restart_interactively_control_(ptr, restart);
		}
	}

	/* eval */
	return eval_debugger_(ptr, io, eval);
}

static int enter_debugger_symbol_p(addr pos, const char *key, int keyword)
{
	if (! symbolp(pos))
		return 0;
	if (keyword && (! keywordp(pos)))
		return 0;
	GetNameSymbol(pos, &pos);
	if (! strvectp(pos))
		return 0;
	return strvect_equalp_char(pos, key);
}

static int enter_debugger_help_(Execute ptr, addr io)
{
	Return(format_stream_(ptr, io, "~&Help:~%", NULL));
	Return(format_stream_(ptr, io, ":help   This message.~%", NULL));
	Return(format_stream_(ptr, io, "?       Run :help.~%", NULL));
	Return(format_stream_(ptr, io, ":show   Debugger information.~%", NULL));
	Return(format_stream_(ptr, io, ":stack  Stack-frame.~%", NULL));
	Return(format_stream_(ptr, io, ":exit   Exit debugger.~%", NULL));
	Return(format_stream_(ptr, io, "^D      Exit debugger.~%", NULL));

	return 0;
}

static int enter_debugger_call_(Execute ptr, addr io, addr list, int *ret)
{
	int eof;
	addr pos;
	size_t select, size;

	Return(clear_input_stream_(io));
	Return(read_prompt_(ptr, io, &eof, &pos));

	/* :exit, EOF */
	if (eof || enter_debugger_symbol_p(pos, "EXIT", 1)) {
		if (eof) {
			Return(terpri_stream_(io));
		}
		Return(finish_output_stream_(io));
		/* restart abort */
		GetConst(COMMON_ABORT, &pos);
		Return(eval_symbol_debugger_(ptr, io, list, pos));
		return Result(ret, 0);
	}

	/* show */
	if (enter_debugger_symbol_p(pos, "SHOW", 1))
		return Result(ret, -1);

	/* stack */
	if (enter_debugger_symbol_p(pos, "STACK", 1)) {
		Return(stack_frame_stream_(ptr, io));
		return Result(ret, 0);
	}

	/* help */
	if (enter_debugger_symbol_p(pos, "HELP", 0)
			|| enter_debugger_symbol_p(pos, "?", 0)) {
		Return(enter_debugger_help_(ptr, io));
		return Result(ret, 0);
	}

	/* check */
	if (! fixnump(pos)) {
		Return(eval_symbol_debugger_(ptr, io, list, pos));
		return Result(ret, 0);
	}
	if (GetIndex_integer(pos, &select)) {
		Return(format_stream_(ptr, io, "Illegal integer value ~A.~%", pos, NULL));
		return Result(ret, 0);
	}

	size = length_list_unsafe(list);
	if (size <= select) {
		Return(format_stream_(ptr, io, "Too large index value ~A.~%", pos, NULL));
		return Result(ret, 0);
	}
	/* execute */
	getnth_unsafe(list, select, &pos);
	Return(invoke_restart_interactively_control_(ptr, pos));
	return Result(ret, 0);
}

static int enter_debugger_abort_(Execute ptr, addr io, addr list, int *ret)
{
	addr control;

	push_control(ptr, &control);
	abort_restart_char_control(ptr, "Exit the debugger on the inside.");
	(void)enter_debugger_call_(ptr, io, list, ret);
	if (equal_control_restart(ptr, control)) {
		normal_throw_control(ptr);
		*ret = 0;
	}
	return pop_control_(ptr, control);
}

static int enter_debugger_(Execute ptr, addr io, addr condition, addr list)
{
	int check;

	/* restarts */
	Return(push_index_debugger_(ptr));

show:
	Return(invoke_standard_header_(ptr, io, condition));
	Return(output_restarts_debugger_(ptr, io, list));

loop:
	Return(enter_debugger_abort_(ptr, io, list, &check));
	switch (check) {
		case -1:
			goto show;
		case 0:
			goto loop;
		default:
			return 0;
	}
}

static int invoke_standard_debugger_(Execute ptr, addr condition)
{
	int check;
	addr io, list, control;

	/* output condition */
	Return(debug_io_stream_(ptr, &io));
	Return(compute_restarts_control_(ptr, condition, &list));

	/* no-debugger */
	Return(enable_debugger_p_(ptr, &check));
	if (! check) {
		Return(invoke_standard_header_(ptr, io, condition));
		Return(format_stream_(ptr, io, "~2&Debugger is not enabled.~%", NULL));
		abort_execute();
		return 0;
	}

	/* no-restart */
	if (list == Nil) {
		Return(invoke_standard_header_(ptr, io, condition));
		Return(format_stream_(ptr, io, "There is no restarts, abort.~%", NULL));
		abort_execute();
		return 0;
	}

	/* debugger */
	push_control(ptr, &control);
	gchold_push_special(ptr, list);
	(void)enter_debugger_(ptr, io, condition, list);
	return pop_control_(ptr, control);
}

static int invoke_debugger_hook_(Execute ptr, addr prior, addr condition)
{
	addr symbol, call, control;

	/* call function */
	call = prior;
	if (symbolp(call)) {
		Return(getfunction_global_(call, &call));
	}

	/* funcall */
	push_control(ptr, &control);
	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	(void)funcall_control_(ptr, call, condition, prior, NULL);
	Return(pop_control_(ptr, control));

	/* invoke-debugger is not returned. */
	return invoke_standard_debugger_(ptr, condition);
}

int invoke_debugger_(Execute ptr, addr condition)
{
	addr symbol, prior;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &prior));
	if (prior == Nil)
		return invoke_standard_debugger_(ptr, condition);
	else
		return invoke_debugger_hook_(ptr, prior, condition);
}


/*
 *  initialize
 */
void build_condition_debugger(Execute ptr)
{
	init_enable_debugger();
	init_index_debugger();
	set_enable_debugger(ptr, 1);
}

void init_condition_debugger(void)
{
	SetPointerCall(defun, var1, handler_warning);
	SetPointerCall(defun, var1, handler_empty);
}

