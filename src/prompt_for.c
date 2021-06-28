#include "call_reader.h"
#include "character_check.h"
#include "condition.h"
#include "cons.h"
#include "control_object.h"
#include "format.h"
#include "hold.h"
#include "print_write.h"
#include "prompt.h"
#include "prompt_for.h"
#include "reader.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "strtype.h"
#include "typedef.h"
#include "type_parse.h"
#include "type_typep.h"

/*
 *  prompt-for
 */
static int prompt_for_call_(Execute ptr, addr io, addr *ret)
{
	addr value;

	Return(finish_output_stream_(io));
	Return(clear_input_stream_(io));
	Return(read_common_(ptr, io, T, Nil, Nil, &value));

	return Result(ret, value);
}

static int prompt_for_string_(Execute ptr, addr io, addr prompt, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	push_prompt(ptr, prompt, prompt_for);
	(void)prompt_for_call_(ptr, io, ret);
	return pop_control_(ptr, control);
}

static int prompt_for_module_(Execute ptr, LocalHold hold,
		addr io, addr type, addr prompt, addr *ret)
{
	int check;
	addr value;

	for (;;) {
		Return(prompt_for_string_(ptr, io, prompt, &value));
		localhold_set(hold, 1, value);
		if (type == T)
			break;
		Return(typep_clang_(ptr, value, type, &check));
		if (check)
			break;

		Return(format_string(ptr, &prompt, "Please answer ~A type: ", type, NULL));
		localhold_set(hold, 2, prompt);
	}

	return Result(ret, value);
}

static int prompt_for_lisp_(Execute ptr, LocalHold hold,
		addr io, addr type, addr prompt, addr *ret)
{
	int check;
	addr value;

	/* output */
	Return(princ_print(ptr, io, prompt));
	Return(finish_output_stream_(io));

	/* query */
	for (;;) {
		Return(clear_input_stream_(io));
		Return(read_common_(ptr, io, T, Nil, Nil, &value));
		localhold_set(hold, 1, value);
		if (type == T)
			break;
		Return(typep_clang_(ptr, value, type, &check));
		if (check)
			break;

		Return(format_stream(ptr, io, "~%Please answer ~A type: ", type, NULL));
		Return(finish_output_stream_(io));
	}

	return Result(ret, value);
}

int prompt_for_stream(Execute ptr, addr type, addr prompt, addr *ret)
{
	addr io;
	LocalHold hold;

	hold = LocalHold_array(ptr, 3);

	/* type */
	if (type != T) {
		Return(parse_type(ptr, &type, type, Nil));
		localhold_set(hold, 0, type);
	}

	/* input */
	Return(query_io_stream_(ptr, &io));
	if (use_prompt_stream(ptr, io)) {
		Return(prompt_for_module_(ptr, hold, io, type, prompt, ret));
	}
	else {
		Return(prompt_for_lisp_(ptr, hold, io, type, prompt, ret));
	}
	localhold_end(hold);

	return 0;
}


/*
 *  yes-or-no-p
 */
static int yes_or_no_p_check1_(Execute ptr, addr io, addr pos, int *retry, int *ret)
{
	int check;

	Return(string_equalp_char_(pos, "yes", &check));
	if (check) {
		*retry = 0;
		return Result(ret, 1);
	}
	Return(string_equalp_char_(pos, "no", &check));
	if (check) {
		*retry = 0;
		return Result(ret, 0);
	}

	*retry = 1;
	return format_stream(ptr, io, "~%Please answer yes or no: ", NULL);
}

static int yes_or_no_p_check2_(Execute ptr, addr io, addr pos, int *retry, int *ret)
{
	unicode c;
	size_t size;

	string_length(pos, &size);
	if (size != 0) {
		Return(string_getc_(pos, 0, &c));
		if (toUpperUnicode(c) == 'Y') {
			*retry = 0;
			return Result(ret, 1);
		}
		if (toUpperUnicode(c) == 'N') {
			*retry = 0;
			return Result(ret, 0);
		}
	}

	*retry = 1;
	return format_stream(ptr, io, "~%Please answer y or n: ", NULL);
}

static int yes_or_no_p_check_(Execute ptr, addr io, addr pos,
		int exactp, int *retry, int *ret)
{
	if (exactp)
		return yes_or_no_p_check1_(ptr, io, pos, retry, ret);
	else
		return yes_or_no_p_check2_(ptr, io, pos, retry, ret);
}

int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret)
{
	int miss, check;
	addr control, stream, pos;

	/* output */
	Return(query_io_stream_(ptr, &stream));
	if (args != Nil) {
		GetCons(args, &control, &args);
		Return(fresh_line_stream_(stream, NULL));
		Return(format_lisp(ptr, stream, control, args, &control));
		Return(print_ascii_stream_(stream, " "));
	}
	Return(print_ascii_stream_(stream, exactp? "(yes or no) ": "(y or n) "));
	Return(finish_output_stream_(stream));

	/* query */
	*ret = 0;
	for (;;) {
		Return(clear_input_stream_(stream));
		Return(read_line_stream_(ptr, &pos, &miss, stream, 1, Unbound, 0));
		if (pos == Unbound)
			return fmte_("*query-io* don't read yes/or question.", NULL);
		Return(yes_or_no_p_check_(ptr, stream, pos, exactp, &check, ret));
		if (! check)
			break;
		Return(finish_output_stream_(stream));
	}

	return 0;
}

