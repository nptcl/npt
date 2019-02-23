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

void push_prompt_info(Execute ptr)
{
	addr symbol, value;

	symbol_prompt_info(&symbol);
	prompt_alloc(NULL, &value);
	pushspecial_control(ptr, symbol, value);
}

size_t getindex_prompt(Execute ptr)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	return PtrPromptInfo(pos)->index;
}

void setindex_prompt(Execute ptr, size_t index)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	PtrPromptInfo(pos)->index = index;
}

int getbreak_prompt(Execute ptr)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	return PtrPromptInfo(pos)->break_p;
}

void setbreak_prompt(Execute ptr, int value)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	PtrPromptInfo(pos)->break_p = (value != 0);
}

int getshow_prompt(Execute ptr)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	return PtrPromptInfo(pos)->show_p;
}

void setshow_prompt(Execute ptr, int value)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	PtrPromptInfo(pos)->show_p = (value != 0);
}

#ifdef LISP_PROMPT_DEFAULT
void show_prompt(Execute ptr, addr io)
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
		fmts(io, "~&* ", NULL);
	}
	else {
		snprintf(buffer, 64, "~&%zu* ", str->index);
		fmts(io, buffer, NULL);
	}
	str->show_p = 0;
	finish_output_stream(io);
}

int input_prompt(addr *ret)
{
	fmte("input-prompt is not supported.", NULL);
	return 1;
}
#endif

#if defined(LISP_PROMPT_READLINE) || defined(LISP_PROMPT_EDITLINE)
#ifdef LISP_PROMPT_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif
#ifdef LISP_PROMPT_EDITLINE
#include <edit/readline/readline.h>
#include <edit/readline/history.h>
#endif

#define HISTORY_SIZE	100
static size_t ReadLine_Size = 0;

void show_prompt(Execute ptr, addr io)
{
	addr pos;
	struct prompt_info *str;

	get_prompt_info(ptr, &pos);
	str = PtrPromptInfo(pos);
	if (! str->break_p)
		str->show_p = 1;
}

static char *make_prompt(void)
{
	char buffer[64];
	addr info;
	char *ret;
	struct prompt_info *str;

	/* prompt-info */
	get_prompt_info(Execute_Thread, &info);
	str = PtrPromptInfo(info);
	if (str->break_p) {
		return NULL;
	}

	/* readline */
	if (str->show_p) {
		if (str->index == 0) {
			ret = readline("* ");
		}
		else {
			snprintf(buffer, 64, "[%zu]* ", str->index);
			ret = readline(buffer);
		}
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

int input_prompt(addr *ret)
{
	addr pos;
	char *str;
	size_t size;

	str = make_prompt();
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

