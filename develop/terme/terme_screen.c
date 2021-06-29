#include "print_font.h"
#include "prompt.h"
#include "stream.h"
#include "stream_common.h"
#include "terme_call.h"
#include "terme_screen.h"
#include "terme_value.h"
#include "typedef.h"

void terme_screen_clear(void)
{
}

static int terme_screen_prompt_string_(Execute ptr, addr pos, enum prompt_mode mode)
{
	return 0;
}

int terme_screen_prompt_(Execute ptr)
{
	int check;
	enum prompt_mode mode;
	addr io, pos;

	/* fresh-line */
	Return(terminal_io_stream_(ptr, &io));
	Return(fresh_line_stream_(io, &check));
	terme_screen_clear();

	/* prompt */
	Return(terme_get_prompt_(ptr, &pos, &mode));
	if (pos == Nil)
		return 0;

	/* screen */
	return terme_screen_prompt_string_(ptr, pos, mode);
}


