#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "eval_main.h"
#include "execute.h"
#include "format.h"
#include "prompt.h"
#include "step_prompt.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  query
 */
static int step_prompt_in_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret, "S", "STEP", "I", "IN", NULL);
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
	ptr->step_over = ptr->step_depth;
	ptr->step_in = 0;
}

static int step_prompt_quit_p_(addr pos, int *ret)
{
	if (! symbolp(pos))
		return Result(ret, 0);
	GetNameSymbol(pos, &pos);
	return string_equalp_char_va_(pos, ret,
			"Q", "QUIT", "E", "EXIT", "C", "CONT", "CONTINUE", NULL);
}

static void step_prompt_quit(Execute ptr)
{
	ptr->step_begin = 0;
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
		"COMMAND   HELP",
		"---",
		"Step      Step in",
		"Over      Step over",
		"Continue  Quit step.",
		"---",
		"Quit      Quit step.",
		"Help      Output this message.",
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

	Return(step_prompt_in_p_(pos, &check));
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
	Return(step_prompt_quit_p_(pos, &check));
	if (check) {
		*exit = 1;
		*exec = 0;
		step_prompt_quit(ptr);
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

static int step_prompt_query_call_(Execute ptr, addr io)
{
	addr prompt;

	strvect_char_heap(&prompt, "Step> ");
	push_prompt(ptr, prompt, prompt_step);
	return eval_custom_loop_(ptr, io, step_prompt_loop_);
}

int execute_step_code(Execute ptr, addr expr)
{
	addr io, control;

	Return(debug_io_stream_(ptr, &io));
	Return(format_stream(ptr, io, "~&STEP: ~S~%", expr, NULL));

	push_control(ptr, &control);
	(void)step_prompt_query_call_(ptr, io);
	return pop_control_(ptr, control);
}

