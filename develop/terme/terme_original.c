#ifdef LISP_TERME_DEVELOP
#include "execute.h"
#include "terme.h"
#include "terme_call.h"
#include "terme_escape.h"
#include "terme_prompt.h"
#include "terme_value.h"
#include "typedef.h"

void build_terme(void)
{
	terme_build();
}

int begin_terme(void)
{
	return terme_init();
}

int end_terme(void)
{
	return terme_free();
}

int prompt_terme_(Execute ptr, addr pos)
{
	return terme_prompt_(ptr, pos);
}

int readline_terme_(Execute ptr, addr *ret)
{
	return terme_readline_(ptr, ret);
}

int font_terme(PrintFont value)
{
	return terme_font(value);
}

int text_color_terme(PrintColor value)
{
	return terme_text_color(value);
}

int back_color_terme(PrintColor value)
{
	return terme_back_color(value);
}

#else

#include "constant.h"
#include "print_write.h"
#include "stream.h"
#include "stream_common.h"
#include "stream_function.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "terme.h"

void build_terme(void)
{
	addr symbol;
	GetConst(SYSTEM_TERME, &symbol);
	SetValueSymbol(symbol, Nil);
}

int begin_terme(void)
{
	return 0;
}

int end_terme(void)
{
	return 0;
}

int prompt_terme_(Execute ptr, addr pos)
{
	addr symbol;

	Check(! stringp(pos), "type error");
	GetConst(SYSTEM_TERME, &symbol);
	setspecial_local(ptr, symbol, pos);

	return 0;
}

static int readline_terme_append_newline_(addr pos, addr *ret)
{
	unicode c;
	addr value;
	size_t size, i;

	strvect_length(pos, &size);
	strvect_heap(&value, size + 1UL);
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		Return(strvect_setc_(value, i, c));
	}
	Return(strvect_setc_(value, i, 0x0A));

	return Result(ret, value);
}

int readline_terme_(Execute ptr, addr *ret)
{
	int check;
	addr input, output, prompt, pos;

	GetConst(STREAM_STDIN, &input);
	GetConst(STREAM_STDOUT, &output);
	GetConst(SYSTEM_TERME, &prompt);
	Return(getspecialcheck_local_(ptr, prompt, &prompt));
	Return(fresh_line_stream_(output, &check));
	if (prompt != Nil) {
		Return(princ_print(ptr, output, prompt));
	}
	Return(finish_output_stream_(output));
	Return(clear_input_stream_(input));
	Return(read_line_stream_(ptr, &pos, &check, input, 0, Nil, 0));
	if (pos == Nil)
		return Result(ret, Nil);
	Return(readline_terme_append_newline_(pos, &pos));
	return Result(ret, pos);
}

int font_terme(PrintFont value)
{
	return 0;
}

int text_color_terme(PrintColor value)
{
	return 0;
}

int back_color_terme(PrintColor value)
{
	return 0;
}
#endif

