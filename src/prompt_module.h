#include <stdio.h>
#include "prompt.h"
#include "strvect.h"
#include "typedef.h"
#include "unicode.h"

/* readline */
#ifdef LISP_PROMPT_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

/* editline */
#ifdef LISP_PROMPT_EDITLINE
#if defined(HAVE_EDITLINE_EDITLINE_H)
#include <editline/editline.h>
#include <editline/history.h>
#elif defined(HAVE_EDITLINE_READLINE_H)
#include <editline/readline.h>
#include <editline/history.h>
#elif defined(HAVE_EDIT_READLINE_H)
#include <edit/readline.h>
#include <edit/history.h>
#elif defined(HAVE_EDIT_READLINE_READLINE_H)
#include <edit/readline/readline.h>
#include <edit/readline/history.h>
#else
#include <edit/readline/readline.h>
#include <edit/readline/history.h>
#endif
#endif

#define HISTORY_SIZE	100
static size_t ReadLine_Size = 0;

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

static char *input_prompt_string(struct prompt_info *str, addr *ret, const char *msg)
{
	char buffer[64];

	/* no-prompt */
	if (str->show_p == 0)
		return readline(NULL);

	/* output prompt */
	if (msg == NULL) {
		if (str->index == 0)
			snprintf(buffer, 64, "* ");
		else
			snprintf(buffer, 64, "[%zu]* ", str->index);
		msg = buffer;
	}
	strvect_char_heap(ret, msg);
	return readline(msg);
}

static char *make_prompt(addr *rprompt, const char *msg)
{
	addr pos;
	char *ret;
	struct prompt_info *str;

	/* prompt-info */
	get_prompt_info(Execute_Thread, &pos);
	str = PtrPromptInfo(pos);
	if (str->break_p)
		return NULL;

	/* readline */
	ret = input_prompt_string(str, rprompt, msg);
	str->show_p = 0;

	/* eof */
	if (ret == NULL)
		str->break_p = 1;

	return ret;
}

int input_prompt_(addr *ret, addr *rprompt, const char *msg)
{
	char *value;

	*rprompt = NULL;
	value = make_prompt(rprompt, msg);
	if (value == NULL) {
		return Result(ret, Nil); /* eof */
	}
	if (value[0]) {
		add_history(value);
	}
	if (HISTORY_SIZE < ReadLine_Size) {
		free(remove_history(0));
	}
	Return(string8_null_char1_heap_(ret, value, 0x0A));
	free(value);
	ReadLine_Size++;

	return 0;
}

