#include "constant.h"
#include "define.h"
#include "execute.h"
#include "prompt.h"
#include "symbol.h"
#include "terme.h"
#include "terme_arch.h"
#include "terme_data.h"
#include "terme_display.h"
#include "terme_history.h"
#include "terme_input.h"
#include "terme_object.h"
#include "terme_output.h"
#include "terme_prompt.h"
#include "terme_screen.h"
#include "typedef.h"

/*
 *  interface
 */
void init_terme(void)
{
	if (terme_arch_init()) {
		Abort("terme_arch_init error.");
	}
	terme_input_init();
	terme_output_init();
}

static void build_terme_object(void)
{
	addr symbol, root, value;

	/* root */
	terme_root_build(&root);

	/* data */
	terme_data_build(&value);
	terme_set(root, terme_root_data, value);

	/* screen */
	terme_screen_build(&value);
	terme_set(root, terme_root_screen, value);

	/* display */
	terme_display_build(&value);
	terme_set(root, terme_root_display, value);

	/* history */
	terme_history_build(&value);
	terme_set(root, terme_root_history, value);

	/* special */
	GetConst(SYSTEM_SPECIAL_TERME, &symbol);
	SetValueSymbol(symbol, root);
}

void build_terme(void)
{
	build_terme_object();
	terme_arch_build();
}

int begin_terme(void)
{
	return terme_arch_begin();
}

int end_terme(void)
{
	return terme_arch_end();
}

int prompt_terme_(Execute ptr, addr pos, PromptMode mode)
{
	return terme_prompt_set_(ptr, pos, mode);
}

int readline_terme_(Execute ptr, addr *ret)
{
	return terme_readline_(ptr, ret);
}

int font_terme(Execute ptr, PrintFont value)
{
	return font_arch_terme(ptr, value);
}

int text_color_terme(Execute ptr, PrintColor value)
{
	return text_color_arch_terme(ptr, value);
}

int back_color_terme(Execute ptr, PrintColor value)
{
	return back_color_arch_terme(ptr, value);
}

