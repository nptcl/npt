#include <stdio.h>
#include "prompt.h"
#include "strvect.h"
#include "typedef.h"

int show_prompt_(Execute ptr, addr io)
{
	addr pos;
	struct prompt_info *str;

	get_prompt_info(ptr, &pos);
	str = PtrPromptInfo(pos);
	if (! str->break_p)
		str->show_p = 1;

	return 0;
}

static void input_prompt_string(struct prompt_info *str, addr *ret, const char *msg)
{
	char buffer[64];

	if (msg == NULL) {
		if (str->index == 0)
			snprintf(buffer, 64, "* ");
		else
			snprintf(buffer, 64, "[%zu]* ", str->index);
		msg = buffer;
	}
	strvect_char_heap(ret, msg);
}

static int input_prompt_read_(Execute ptr,
		struct prompt_info *str, addr *rprompt, const char *msg)
{
	addr pos;

	input_prompt_string(str, &pos, msg);
	Return(prompt_term_(ptr, pos));
	str->show_p = 0;
	return Result(rprompt, pos);
}

int input_prompt(addr *ret, addr *rprompt, const char *msg)
{
	struct prompt_info *str;
	Execute ptr;
	addr pos;

	ptr = Execute_Thread;
	get_prompt_info(ptr, &pos);
	str = PtrPromptInfo(pos);

	*rprompt = NULL;
	if (str->break_p) {
		return 0;
	}
	if (str->show_p) {
		Return(input_prompt_read_(ptr, str, rprompt, msg));
	}
	Return(readline_term_(ptr, &pos));
	if (pos == Nil) {
		str->break_p = 1;
	}

	return Result(ret, pos);
}

