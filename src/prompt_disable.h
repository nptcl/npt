#include "condition.h"
#include "prompt.h"
#include "stream.h"
#include "stream_function.h"
#include "strvect.h"
#include "typedef.h"

int show_prompt_(Execute ptr, addr io)
{
	char buffer[64];
	addr pos;
	struct prompt_info *str;

	get_prompt_info(ptr, &pos);
	str = PtrPromptInfo(pos);
	if (str->break_p) {
		return 0;
	}
	if (str->index == 0) {
		Return(print_ascii_stream_(io, "~&* "));
	}
	else {
		snprintf(buffer, 64, "~&%zu* ", str->index);
		Return(print_ascii_stream_(io, buffer));
	}
	str->show_p = 0;
	return finish_output_stream_(io);
}

int input_prompt(addr *ret, addr *prompt, const char *str)
{
	return fmte_("input-prompt is not supported.", NULL);
}

