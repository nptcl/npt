#include "character_queue.h"
#include "condition.h"
#include "constant.h"
#include "control_object.h"
#include "format.h"
#include "local.h"
#include "prompt.h"
#include "reader.h"
#include "stream.h"
#include "stream_function.h"
#include "strvect.h"
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
	getspecial_local(ptr, symbol, ret);
	Check(*ret == Unbound, "unbound error");
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

size_t getindex_prompt_safe(Execute ptr)
{
	addr pos;
	struct prompt_info *str;

	symbol_prompt_info(&pos);
	getspecial_local(ptr, pos, &pos);
	str = PtrPromptInfo(pos);
	return (pos == Unbound)? 0: str->index;
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

int getshow_prompt_safe(Execute ptr)
{
	addr pos;
	struct prompt_info *str;

	symbol_prompt_info(&pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound)
		return 0;

	str = PtrPromptInfo(pos);
	return str->show_p;
}

void setshow_prompt(Execute ptr, int value)
{
	addr pos;
	get_prompt_info(ptr, &pos);
	PtrPromptInfo(pos)->show_p = (value != 0);
}

void endshow_prompt_safe(Execute ptr)
{
	addr pos;
	struct prompt_info *str;

	symbol_prompt_info(&pos);
	getspecial_local(ptr, pos, &pos);
	if (pos != Unbound) {
		str = PtrPromptInfo(pos);
		str->show_p = 0;
	}
}


#ifdef LISP_PROMPT_DEFAULT
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

int input_prompt(addr *ret, addr *prompt, const char *message)
{
	return fmte_("input-prompt is not supported.", NULL);
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

static char *make_prompt(addr *prompt, const char *message)
{
	char buffer[64];
	addr pos;
	char *ret;
	struct prompt_info *str;

	/* prompt-info */
	get_prompt_info(Execute_Thread, &pos);
	str = PtrPromptInfo(pos);
	if (prompt)
		*prompt = NULL;
	if (str->break_p) {
		return NULL;
	}

	/* readline */
	if (str->show_p) {
		if (message == NULL) {
			if (str->index == 0)
				snprintf(buffer, 64, "* ");
			else
				snprintf(buffer, 64, "[%zu]* ", str->index);
			message = buffer;
		}
		if (prompt)
			strvect_char_heap(prompt, message);
		ret = readline(message);
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

int input_prompt(addr *ret, addr *prompt, const char *message)
{
	addr pos;
	char *str;
	size_t size;

	str = make_prompt(prompt, message);
	if (str == NULL) {
		return Result(ret, Nil); /* eof */
	}
	if (str[0]) {
		add_history(str);
	}
	if (HISTORY_SIZE < ReadLine_Size) {
		free(remove_history(0));
	}
	Return(strvect_char1_heap_(&pos, str, 0x0A));
	free(str);
	strvect_length(pos, &size);
	ReadLine_Size++;

	return Result(ret, pos);
}
#endif

