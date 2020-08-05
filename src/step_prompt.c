#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_main.h"
#include "execute.h"
#include "format.h"
#include "stream.h"
#include "stream_prompt.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  query
 */
static int step_prompt_exit_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret,
			"Q", "QUIT", "E", "EXIT", "S", "STEP", NULL);
}

static int step_prompt_over_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "O", "OVER", NULL);
}

static void step_prompt_over(Execute ptr)
{
	addr symbol;
	GetConst(SYSTEM_STEP_VALUE, &symbol);
	setspecial_local(ptr, symbol, Nil);
}

static int step_prompt_help_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "?", "H", "HELP", NULL);
}

static int step_prompt_help_(Execute ptr, addr io)
{
	static const char *const message[] = {
		"Step help.",
		"---",
		"step  Step in",
		"over  Step over",
		"---",
		"quit  Quit step.",
		"help  Output this message.",
		"---",
		NULL
	};
	int i;
	const char *str;

	for (i = 0; ; i++) {
		str = message[i];
		if (str == NULL)
			break;
		Return(print_ascii_stream_(io, str));
		Return(terpri_stream_(io));
		Return(force_output_stream_(io));
	}

	return 0;
}

static int step_prompt_loop_(Execute ptr, addr io, addr pos, int *exit, int *exec)
{
	int check;

	Return(step_prompt_exit_p_(pos, &check));
	if (check) {
		*exit = 1;
		*exec = 0;
		return 0;
	}
	Return(step_prompt_over_p_(pos, &check));
	if (check) {
		*exit = 1;
		*exec = 0;
		step_prompt_over(ptr);
		return 0;
	}
	Return(step_prompt_help_p_(pos, &check));
	if (check) {
		*exit = 0;
		*exec = 0;
		return step_prompt_help_(ptr, io);
	}
	*exit = 0;
	*exec = 1;
	return 0;
}

static int step_prompt_query_(Execute ptr, addr value, int *ret)
{
	int check;
	addr io, symbol, control;

	debug_io_stream(ptr, &io);
	Return(format_stream(ptr, io, "~&STEP: ~S~%", value, NULL));

	/* prompt */
	push_new_control(ptr, &control);
	GetConst(SYSTEM_STEP_VALUE, &symbol);
	pushspecial_control(ptr, symbol, T);
	mode_prompt_stream(ptr, PromptStreamMode_Step);
	Return(eval_custom_loop(ptr, io, step_prompt_loop_));
	getspecial_local(ptr, symbol, &value);
	check = (value != Nil);
	Return(free_control_(ptr, control));

	return Result(ret, check);
}


/*
 *  code
 */
_g int execute_step_code(Execute ptr, addr expr, addr value)
{
	int stepin;
	addr symbol, check, control;

	/* *step-break* */
	GetConst(SYSTEM_STEP_BREAK, &symbol);
	getspecial_local(ptr, symbol, &check);
	if (check == Nil) {
		/* step throw */
		return runcode_control(ptr, expr);
	}

	/* query */
	Return(step_prompt_query_(ptr, value, &stepin));
	if (stepin) {
		/* step in */
		return runcode_control(ptr, expr);
	}

	/* step over */
	push_new_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	Return(runcode_control(ptr, expr));
	return free_control_(ptr, control);
}

