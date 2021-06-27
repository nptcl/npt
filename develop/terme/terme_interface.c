#include "execute.h"
#include "terme.h"
#include "terme_call.h"
#include "terme_escape.h"
#include "terme_prompt.h"
#include "terme_value.h"
#include "typedef.h"

void init_terme(void)
{
	terme_init();
}

void build_terme(void)
{
	terme_build();
}

int begin_terme(void)
{
	return terme_begin();
}

int end_terme(void)
{
	terme_end();
	return 0;
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

