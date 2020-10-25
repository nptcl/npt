#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_main.h"
#include "execute.h"
#include "format.h"
#include "step_prompt.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
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

static int step_prompt_query_call_(Execute ptr, addr io, addr value, int *ret)
{
	addr symbol;

	GetConst(SYSTEM_STEP_VALUE, &symbol);
	pushspecial_control(ptr, symbol, T);
	mode_prompt_stream(ptr, PromptStreamMode_Step);
	Return(eval_custom_loop_(ptr, io, step_prompt_loop_));
	getspecial_local(ptr, symbol, &value);

	return Result(ret, (value != Nil));
}

static int step_prompt_query_(Execute ptr, addr value, int *ret)
{
	addr io, control;

	Return(debug_io_stream_(ptr, &io));
	Return(format_stream(ptr, io, "~&STEP: ~S~%", value, NULL));

	push_control(ptr, &control);
	(void)step_prompt_query_call_(ptr, io, value, ret);
	return pop_control_(ptr, control);
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
		return runcode_control_(ptr, expr);
	}

	/* query */
	stepin = 0;
	Return(step_prompt_query_(ptr, value, &stepin));
	if (stepin) {
		/* step in */
		return runcode_control_(ptr, expr);
	}

	/* step over */
	push_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	(void)runcode_control_(ptr, expr);
	return pop_control_(ptr, control);
}

