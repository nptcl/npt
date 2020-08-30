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
#include "format.h"
#include "function.h"
#include "hold.h"
#include "integer.h"
#include "pointer.h"
#include "prompt.h"
#include "reader.h"
#include "restart.h"
#include "stream.h"
#include "stream_prompt.h"
#include "stream_string.h"
#include "strtype.h"
#include "symbol.h"
#include "type_object.h"

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
		Return(format_stream(ptr, stream, "~&WARNING: ", NULL));
		Return(format_lisp(ptr, stream, format, args, &args));
		Return(fresh_line_stream_(stream, NULL));
	}
	else {
		Return(format_stream(ptr, stream, "~&WARNING: ~S~%", condition, NULL));
	}

	return force_output_stream_(stream);
}

_g int handler_warning_(Execute ptr)
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

_g int handler_savecore_(Execute ptr)
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
_g int handler_exit_(Execute ptr)
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
	return format_stream(ptr, stream, "Unbound variable ~S.~%", condition, NULL);
}

static int output_undefined_function_(Execute ptr, addr stream, addr condition)
{
	Return(cell_error_name_(condition, &condition));
	return format_stream(ptr, stream, "Undefined function ~S.~%", condition, NULL);
}

static int output_simple_error_(Execute ptr, addr stream, addr condition)
{
	addr control, arguments;

	Return(simple_condition_format_(condition, &control, &arguments));
	Return(format_stream_lisp(ptr, stream, control, arguments));
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
	return format_stream(ptr, stream,
			"Value ~S must be a ~S type.~%", datum, expected, NULL);
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

static int output_debugger(Execute ptr, addr stream, addr pos)
{
	int check;

	Return(ConditionCheck_(UNBOUND_VARIABLE, pos, &check));
	if (check)
		return output_unbound_variable_(ptr, stream, pos);
	Return(ConditionCheck_(UNDEFINED_FUNCTION, pos, &check));
	if (check)
		return output_undefined_function_(ptr, stream, pos);
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
	return format_stream(ptr, stream, "Invalid condition type ~S~%", pos, NULL);
}

static int output_restarts_debugger(Execute ptr, addr io, addr list)
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
				check = callclang_funcall(ptr, &name, name, str, NULL);
				if (check)
					return fmte_("Invalid restart report.", NULL);
				Return(string_stream_heap_(str, &name));
				close_output_string_stream(str);
			}
		}
		id = intsizeh(index);
		Return(format_stream(ptr, io, "~2@A. ~16A ~A~%", id, symbol, name, NULL));
	}

	return 0;
}

static int eval_debugger_call_(Execute ptr, addr io, addr eval)
{
	Return(eval_execute_partial(ptr, eval));
	return eval_loop_output(ptr, io);
}

static int eval_debugger(Execute ptr, addr io, addr eval)
{
	addr control;

	push_control(ptr, &control);
	(void)eval_debugger_call_(ptr, io, eval);
	return pop_control_(ptr, control);
}

static int enter_debugger(Execute ptr, addr condition)
{
	int check, result;
	addr io, pos, list, exit;
	size_t index, select, size;
	LocalHold hold;

	/* restarts */
	mode_prompt_stream(ptr, PromptStreamMode_Normal);
	Return(debug_io_stream_(ptr, &io));
	Return(compute_restarts_control_(ptr, condition, &list));
	hold = LocalHold_local_push(ptr, list);
	if (list == Nil) {
		Return(format_stream(ptr, io, "There is no restarts, abort.~%", NULL));
		abort_execute();
		return 0;
	}
	Return(output_restarts_debugger(ptr, io, list));
	size = length_list_unsafe(list);

	/* eval loop */
	index = getindex_prompt(ptr) + 1ULL;
	GetConst(KEYWORD_EXIT, &exit);

loop:
	setindex_prompt(ptr, index);
	setshow_prompt(ptr, 1);
	Return(clear_input_stream_(io));
	check = read_stream(ptr, io, &result, &pos);
	/* Interupt */
	if (check) {
		goto exit;
	}
	/* EOF */
	if (result) {
		Return(terpri_stream_(io));
		goto exit;
	}
	/* :exit */
	if (getbreak_prompt(ptr) || pos == exit) {
		Return(terpri_stream_(io));
		goto exit;
	}
	/* check */
	if (! fixnump(pos)) {
		if (eval_debugger(ptr, io, pos))
			return 1;
		goto loop;
	}
	if (GetIndex_integer(pos, &select)) {
		Return(format_stream(ptr, io, "Illegal integer value ~A.~%", pos, NULL));
		goto loop;
	}
	if (size <= select) {
		Return(format_stream(ptr, io, "Too large index value ~A.~%", pos, NULL));
		goto loop;
	}
	/* execute */
	getnth_unsafe(list, select, &pos);
	localhold_end(hold);
	Return(invoke_restart_interactively_control_(ptr, pos));
	goto loop;

exit:
	abort_execute();
	return 0;
}

static int enable_debugger_p_(Execute ptr, int *ret)
{
	addr pos;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	return Result(ret, pos != Nil);
}

static int invoke_standard_debugger(Execute ptr, addr condition)
{
	int check;
	addr io, pos, control;

	/* output condition */
	Return(debug_io_stream_(ptr, &io));
	Return(clos_class_of_(condition, &pos));
	Return(stdget_class_name_(pos, &pos));
	Return(format_stream(ptr, io, "~&ERROR: ~S~%", pos, NULL));
	Return(output_debugger(ptr, io, condition));

	/* no-debugger */
	Return(enable_debugger_p_(ptr, &check));
	if (! check) {
		Return(format_stream(ptr, io, "~2&Debugger is not enabled.~%", NULL));
		abort_execute();
		return 0;
	}

	/* debugger */
	push_control(ptr, &control);
	(void)enter_debugger(ptr, condition);
	return pop_control_(ptr, control);
}

_g int invoke_debugger(Execute ptr, addr condition)
{
	addr symbol, prior, call, control;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	Return(getspecialcheck_local_(ptr, symbol, &prior));
	if (prior == Nil)
		return invoke_standard_debugger(ptr, condition);
	/* call function */
	call = prior;
	if (symbolp(call)) {
		Return(getfunction_global_(call, &call));
	}
	push_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	(void)funcall_control(ptr, call, condition, prior, NULL);
	Return(pop_control_(ptr, control));
	/* invoke-debugger is not returned. */
	return invoke_standard_debugger(ptr, condition);
}

_g void set_enable_debugger(Execute ptr, int value)
{
	addr pos;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &pos);
	setspecial_local(ptr, pos, value? T: Nil);
}


/*
 *  initialize
 */
_g void build_condition_debugger(Execute ptr)
{
	addr symbol;

	/* debugger */
	GetConst(SYSTEM_ENABLE_DEBUGGER, &symbol);
	SetValueSymbol(symbol, T);
	set_enable_debugger(ptr, 1);
}

_g void init_condition_debugger(void)
{
	SetPointerCall(defun, var1, handler_warning);
	SetPointerCall(defun, var1, handler_empty);
}

