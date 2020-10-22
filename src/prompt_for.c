#include "character_check.h"
#include "condition.h"
#include "cons.h"
#include "format.h"
#include "hold.h"
#include "print_write.h"
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
_g int prompt_for_stream(Execute ptr, addr type, addr prompt, addr *ret)
{
	int result;
	addr stream, spec, value;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	/* output */
	Return(query_io_stream_(ptr, &stream));
	localhold_push(hold, stream);
	Return(fresh_line_stream_(stream, NULL));
	Return(princ_print(ptr, stream, prompt));
	Return(finish_output_stream_(stream));

	/* query */
	spec = Nil;
	if (type != T) {
		Return(parse_type(ptr, &spec, type, Nil));
		localhold_push(hold, spec);
	}
	for (;;) {
		Return(clear_input_stream_(stream));
		Return(read_stream(ptr, stream, &result, &value));
		if (result)
			return fmte_("Can't read from *query-io* stream.", NULL);
		localhold_set(hold, 0, value);
		if (type == T)
			break;
		Return(typep_clang_(ptr, value, spec, &result));
		if (result)
			break;

		Return(format_stream(ptr, stream, "~%Please answer ~A type: ", type, NULL));
		Return(finish_output_stream_(stream));
	}
	localhold_end(hold);

	return Result(ret, value);
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

_g int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret)
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

