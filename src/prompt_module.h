#include <stdio.h>
#include "condition.h"
#include "encode.h"
#include "prompt.h"
#include "strvect.h"
#include "strtype.h"
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

#define PROMPT_HISTORY_SIZE	64
static size_t Prompt_HistorySize = 0;

static int input_prompt_char_(Execute ptr, char **ret)
{
	addr value, data;

	getvalue_prompt(ptr, &value);
	if (value == Nil)
		return Result(ret, NULL);

	if (! stringp(value)) {
		*ret = NULL;
		return fmte_("The prompt value ~S must be a string type.", value, NULL);
	}
	Return(UTF8_buffer_clang_(ptr->local, &data, value));
	if (data == Unbound) {
		*ret = NULL;
		return fmte_("Invalid UTF-8 encoding ~S.", value, NULL);
	}
	posbody(data, (addr *)&data);

	return Result(ret, (char *)data);
}

static void input_prompt_history(char *value)
{
	if (value[0]) {
		add_history(value);
		Prompt_HistorySize++;
	}
	if (PROMPT_HISTORY_SIZE < Prompt_HistorySize) {
		free(remove_history(0));
	}
}

int input_prompt_(Execute ptr, addr *ret)
{
	int check;
	char *value;
	LocalRoot local;
	LocalStack stack;

	/* readline */
	local = ptr->local;
	push_local(local, &stack);
	Return(input_prompt_char_(ptr, &value));
	value = readline(value);
	rollback_local(local, stack);
	if (value == NULL)
		return Result(ret, Nil);

	/* result */
	input_prompt_history(value);
	check = string8_null_char1_heap_(ret, value, 0x0A);
	free(value);
	return check;
}

int clear_prompt_(void)
{
	return 0;
}

