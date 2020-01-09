#include "charqueue.h"
#include "condition.h"
#include "constant.h"
#include "control.h"
#include "format.h"
#include "local.h"
#include "prompt.h"
#include "readtable.h"
#include "stream.h"
#include "symbol.h"

struct prompt_info {
	unsigned break_p : 1;
	unsigned show_p : 1;
	size_t index;
};

#define PtrPromptInfo(x) ((struct prompt_info *)PtrBodyB2(x))

static void prompt_alloc(LocalRoot local, addr *ret)
{
	addr pos;
	struct prompt_info *str;

	alloc_body2(local, &pos, LISPSYSTEM_PROMPT, sizeoft(struct prompt_info));
	str = PtrPromptInfo(pos);
	str->break_p = 0;
	str->show_p = 1;
	str->index = 0;
	*ret = pos;
}

static void symbol_prompt_info(addr *ret)
{
	GetConst(SYSTEM_PROMPT_INFO, ret);
}

static void get_prompt_info(Execute ptr, addr *ret)
{
	addr symbol;

	symbol_prompt_info(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
}

_g void push_prompt_info(Execute ptr)
{
	addr symbol, value;

	symbol_prompt_info(&symbol);
	prompt_alloc(NULL, &value);
	pushspecial_control(ptr, symbol, value);
}

_g size_t getindex_prompt(Execute ptr)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	return PtrPromptInfo(pos)->index;
}

_g void setindex_prompt(Execute ptr, size_t index)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	PtrPromptInfo(pos)->index = index;
}

_g int getbreak_prompt(Execute ptr)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	return PtrPromptInfo(pos)->break_p;
}

_g void setbreak_prompt(Execute ptr, int value)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	PtrPromptInfo(pos)->break_p = (value != 0);
}

_g int getshow_prompt(Execute ptr)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	return PtrPromptInfo(pos)->show_p;
}

_g void setshow_prompt(Execute ptr, int value)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	PtrPromptInfo(pos)->show_p = (value != 0);
}

#ifdef LISP_PROMPT_DEFAULT
_g void show_prompt(Execute ptr, addr io)
{
	char buffer[64];
	addr pos;
	struct prompt_info *str;

	get_prompt_info(ptr, &pos);
	str = PtrPromptInfo(pos);
	if (str->break_p) {
		return;
	}
	if (str->index == 0) {
		print_ascii_stream(io, "~&* ");
	}
	else {
		snprintf(buffer, 64, "~&%zu* ", str->index);
		print_ascii_stream(io, buffer);
	}
	str->show_p = 0;
	finish_output_stream(io);
}

_g int input_prompt(addr *ret, addr *prompt)
{
	fmte("input-prompt is not supported.", NULL);
	return 1;
}
#endif

#if defined(LISP_PROMPT_READLINE) || defined(LISP_PROMPT_EDITLINE)
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

_g void show_prompt(Execute ptr, addr io)
{
	addr pos;
	struct prompt_info *str;

	get_prompt_info(ptr, &pos);
	str = PtrPromptInfo(pos);
	if (! str->break_p)
		str->show_p = 1;
}

static char *make_prompt(addr *prompt)
{
	char buffer[64];
	addr info;
	char *ret;
	struct prompt_info *str;

	/* prompt-info */
	get_prompt_info(Execute_Thread, &info);
	str = PtrPromptInfo(info);
	if (prompt)
		*prompt = NULL;
	if (str->break_p) {
		return NULL;
	}

	/* readline */
	if (str->show_p) {
		if (str->index == 0)
			snprintf(buffer, 64, "* ");
		else
			snprintf(buffer, 64, "[%zu]* ", str->index);
		if (prompt)
			strvect_char_heap(prompt, buffer);
		ret = readline(buffer);
		str->show_p = 0;
	}
	else {
		ret = readline(NULL);
	}

	/* eof */
	if (ret == NULL) {
		str->break_p = 1;
	}

	return ret;
}

_g int input_prompt(addr *ret, addr *prompt)
{
	addr pos;
	char *str;
	size_t size;

	str = make_prompt(prompt);
	if (str == NULL) {
		return 1; /* eof */
	}
	if (str[0]) {
		add_history(str);
	}
	if (HISTORY_SIZE < ReadLine_Size) {
		free(remove_history(0));
	}
	strvect_char1_heap(&pos, str, 0x0A);
	free(str);
	strvect_length(pos, &size);
	ReadLine_Size++;
	*ret = pos;

	return 0;
}
#endif

