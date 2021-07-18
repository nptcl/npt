#include "call_reader.h"
#include "call_streams.h"
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
#include "stream_string.h"
#include "strvect.h"
#include "strtype.h"
#include "typedef.h"
#include "type_parse.h"
#include "type_typep.h"

/*
 *  prompt-for
 */
static int prompt_for_input_call_(Execute ptr, addr io, addr *ret)
{
	addr value;

	Return(finish_output_stream_(io));
	Return(clear_input_stream_(io));
	Return(read_common_(ptr, io, T, Nil, Nil, &value));

	return Result(ret, value);
}

static int prompt_for_input_(Execute ptr, addr io, addr prompt, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	push_prompt(ptr, prompt, prompt_for);
	(void)prompt_for_input_call_(ptr, io, ret);
	return pop_control_(ptr, control);
}

static int prompt_for_module_(Execute ptr, LocalHold hold,
		addr io, addr type, addr prompt, addr *ret)
{
	int check;
	addr value;

	value = Nil;
	for (;;) {
		Return(prompt_for_input_(ptr, io, prompt, &value));
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

int prompt_for_stream_(Execute ptr, addr type, addr prompt, addr *ret)
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
 *  prompt-string
 */
static int prompt_string_input_call_(Execute ptr, addr io, int errorp, addr *ret)
{
	addr value, ignore;

	Return(finish_output_stream_(io));
	Return(clear_input_stream_(io));
	Return(read_line_common(ptr, io, errorp? T: Nil, Nil, Nil, &value, &ignore));

	return Result(ret, value);
}

static int prompt_string_module_(Execute ptr,
		addr io, addr prompt, int errorp, addr *ret)
{
	addr control;

	push_control(ptr, &control);
	push_prompt(ptr, prompt, prompt_for);
	(void)prompt_string_input_call_(ptr, io, errorp, ret);
	return pop_control_(ptr, control);
}

static int prompt_string_lisp_(Execute ptr,
		addr io, addr prompt, int errorp, addr *ret)
{
	addr ignore;

	/* output */
	Return(princ_print(ptr, io, prompt));
	Return(finish_output_stream_(io));

	/* query */
	Return(clear_input_stream_(io));
	return read_line_common(ptr, io, errorp? T: Nil, Nil, Nil, ret, &ignore);
}

int prompt_string_stream_(Execute ptr, addr prompt, int errorp, addr *ret)
{
	addr io;

	Return(query_io_stream_(ptr, &io));
	if (use_prompt_stream(ptr, io))
		return prompt_string_module_(ptr, io, prompt, errorp, ret);
	else
		return prompt_string_lisp_(ptr, io, prompt, errorp, ret);
}


/*
 *  yes-or-no-p
 */
static int yes_or_no_p_char_common_(Execute ptr, addr args, int *ret,
		const char *str_prompt,
		int (yes_)(addr, int *),
		int (no_)(addr, int *),
		const char *str_error)
{
	int check, answer;
	addr stream, control, prompt, pos;

	/* prompt */
	if (args != Nil) {
		open_output_string_stream(&stream, 0);
		GetCons(args, &control, &args);
		Return(format_lisp(ptr, stream, control, args, &control));
		Return(print_ascii_stream_(stream, " "));
		Return(string_stream_heap_(stream, &prompt));
		Return(close_stream_(stream, NULL));
	}
	else {
		strvect_char_heap(&prompt, str_prompt);
	}

	/* loop */
	for (;;) {
		Return(prompt_string_stream_(ptr, prompt, 1, &pos));
		if (stringp(pos)) {
			/* yes */
			Return((*yes_)(pos, &check));
			if (check) {
				answer = 1;
				break;
			}
			/* no */
			Return((*no_)(pos, &check));
			if (check) {
				answer = 0;
				break;
			}
		}
		/* error */
		strvect_char_heap(&prompt, str_error);
	}

	return Result(ret, answer);
}

static int yes_or_no_p_yes_(addr pos, int *ret)
{
	return string_equalp_char_(pos, "yes", ret);
}

static int yes_or_no_p_no_(addr pos, int *ret)
{
	return string_equalp_char_(pos, "no", ret);
}

static int yes_or_no_p_y_(addr pos, int *ret)
{
	size_t size;
	unicode c;

	string_length(pos, &size);
	if (size == 0)
		return Result(ret, 0);
	Return(string_getc_(pos, 0, &c));
	return Result(ret, toUpperUnicode(c) == 'Y');
}

static int yes_or_no_p_n_(addr pos, int *ret)
{
	size_t size;
	unicode c;

	string_length(pos, &size);
	if (size == 0)
		return Result(ret, 0);
	Return(string_getc_(pos, 0, &c));
	return Result(ret, toUpperUnicode(c) == 'N');
}

int yes_or_no_p_common_(Execute ptr, addr args, int exactp, int *ret)
{
	if (exactp) {
		return yes_or_no_p_char_common_(ptr, args, ret,
				"(yes or no) ", yes_or_no_p_yes_, yes_or_no_p_no_,
				"Please answer yes or no: ");
	}
	else {
		return yes_or_no_p_char_common_(ptr, args, ret,
				"(y or n) ", yes_or_no_p_y_, yes_or_no_p_n_,
				"Please answer y or n: ");
	}

	return 0;
}

