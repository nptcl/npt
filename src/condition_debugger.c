#include "clos.h"
#include "clos_class.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval.h"
#include "eval_main.h"
#include "format.h"
#include "function.h"
#include "gc.h"
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
	addr pos, stream, format, args;

	error_output_stream(ptr, &stream);
	GetConst(CONDITION_SIMPLE_WARNING, &pos);
	if (clos_subtype_p(condition, pos)) {
		simple_condition_format_control(condition, &format);
		simple_condition_format_arguments(condition, &args);
		Return(format_stream(ptr, stream, "~&WARNING: ", NULL));
		Return(format_lisp(ptr, stream, format, args, &args));
		fresh_line_stream(stream);
	}
	else {
		Return(format_stream(ptr, stream, "~&WARNING: ~S~%", condition, NULL));
	}
	force_output_stream(stream);

	return 0;
}

_g void handler_warning(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_WARNING, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_warning);
	pushhandler_common(ptr, pos, call, 0);
}


/*
 *  (handler-case
 *    ...
 *    ((system::savecore #'function-handler-savecore)))
 */
static int function_handler_savecore(Execute ptr, addr condition)
{
	/* do-nothing */
	return 0;
}

_g void handler_savecore(Execute ptr)
{
	addr pos, call;

	GetConst(CONDITION_SAVECORE, &pos);
	compiled_local(ptr->local, &call, Nil);
	setcompiled_var1(call, p_defun_handler_savecore);
	pushhandler_common(ptr, pos, call, 1);
}


/*
 *  debugger
 */
static int output_unbound_variable(Execute ptr, addr stream, addr condition)
{
	cell_error_name(condition, &condition);
	return format_stream(ptr, stream, "Unbound variable ~S.~%", condition, NULL);
}

static int output_undefined_function(Execute ptr, addr stream, addr condition)
{
	cell_error_name(condition, &condition);
	return format_stream(ptr, stream, "Undefined function ~S.~%", condition, NULL);
}

static int output_simple_error(Execute ptr, addr stream, addr condition)
{
	addr control, arguments;

	simple_condition_format(condition, &control, &arguments);
	if (format_stream_lisp(ptr, stream, control, arguments))
		return 1;
	fresh_line_stream(stream);
	terpri_stream(stream);

	return 0;
}

static int output_type_error(Execute ptr, addr stream, addr instance)
{
	addr datum, expected;

	type_error_datum(instance, &datum);
	type_error_expected(instance, &expected);
	if (GetType(expected) == LISPTYPE_TYPE)
		type_object(&expected, expected);
	return format_stream(ptr, stream,
			"Value ~S must be a ~S type.~%", datum, expected, NULL);
}

static int output_condition(Execute ptr, addr stream, addr condition)
{
	return 0;
}

static int condition_check_p(constindex index, addr condition)
{
	addr super;

	if (! condition_instance_p(condition)) return 0;
	GetConstant(index, &super);
	return clos_subtype_p(condition, super);
}
#define ConditionCheck(x,y) condition_check_p(CONSTANT_CONDITION_##x,(y))

static int output_debugger(Execute ptr, addr stream, addr pos)
{
	if (ConditionCheck(UNBOUND_VARIABLE, pos))
		return output_unbound_variable(ptr, stream, pos);
	if (ConditionCheck(UNDEFINED_FUNCTION, pos))
		return output_undefined_function(ptr, stream, pos);
	if (ConditionCheck(SIMPLE_CONDITION, pos))
		return output_simple_error(ptr, stream, pos);
	if (ConditionCheck(TYPE_ERROR, pos))
		return output_type_error(ptr, stream, pos);
	if (condition_instance_p(pos))
		return output_condition(ptr, stream, pos);
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
					fmte("Invalid restart report.", NULL);
				string_stream_heap(str, &name);
				close_stream(str);
			}
		}
		id = intsizeh(index);
		Return(format_stream(ptr, io, "~2@A. ~16A ~A~%", id, symbol, name, NULL));
	}

	return 0;
}

static int eval_debugger(Execute ptr, addr io, addr eval)
{
	addr control;

	push_new_control(ptr, &control);
	Return(eval_execute(ptr, eval));
	Return(eval_loop_output(ptr, io, control));
	return free_control_(ptr, control);
}

static int enter_debugger(Execute ptr, addr condition)
{
	int check, result;
	addr io, pos, list, exit;
	size_t index, select, size;
	LocalHold hold;

	/* restarts */
	mode_prompt_stream(ptr, PromptStreamMode_Normal);
	debug_io_stream(ptr, &io);
	Return(compute_restarts_control_(ptr, condition, &list));
	hold = LocalHold_local_push(ptr, list);
	if (list == Nil) {
		Return(format_stream(ptr, io, "There is no restarts, abort.~%", NULL));
		abortthis();
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
	clear_input_stream(io);
	check = read_stream(ptr, io, &result, &pos);
	/* Interupt */
	if (check) {
		fmte("Invalid operation.", NULL);
		goto exit;
	}
	/* EOF */
	if (result) {
		terpri_stream(io);
		goto exit;
	}
	/* :exit */
	if (getbreak_prompt(ptr) || pos == exit) {
		terpri_stream(io);
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
	exit_code(ptr, LISPCODE_ERROR);
	return 0;
}

static int enable_debugger_p(Execute ptr)
{
	addr pos;
	GetConst(SYSTEM_ENABLE_DEBUGGER, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	return pos != Nil;
}

static int invoke_standard_debugger(Execute ptr, addr condition)
{
	addr io, pos, control;

	/* output condition */
	debug_io_stream(ptr, &io);
	clos_class_of(condition, &pos);
	stdget_class_name(pos, &pos);
	Return(format_stream(ptr, io, "~&ERROR: ~S~%", pos, NULL));
	Return(output_debugger(ptr, io, condition));

	/* no-debugger */
	if (! enable_debugger_p(ptr)) {
		Return(format_stream(ptr, io, "~2&Debugger is not enabled.~%", NULL));
		abortthis();
		return 0;
	}

	/* debugger */
	push_new_control(ptr, &control);
	return enter_debugger(ptr, condition);
}

_g int invoke_debugger(Execute ptr, addr condition)
{
	addr symbol, prior, call, control;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	getspecialcheck_local(ptr, symbol, &prior);
	if (prior == Nil)
		return invoke_standard_debugger(ptr, condition);
	/* call function */
	call = prior;
	if (symbolp(call))
		getfunction_global(call, &call);
	push_new_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	Return(funcall_control(ptr, call, condition, prior, NULL));
	Return(free_control_(ptr, control));
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
	SetPointerCall(defun, var1, handler_savecore);
}

