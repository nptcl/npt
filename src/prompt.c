#include "condition_define.h"
#include "control_object.h"
#include "heap.h"
#include "prompt.h"
#include "reader.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

static void symbol_prompt(addr *ret)
{
	GetConst(SYSTEM_PROMPT, ret);
}

static void prompt_heap(addr *ret, addr value, PromptMode mode)
{
	addr pos;
	struct prompt_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PROMPT, 1, sizeoft(struct prompt_struct));
	str = PtrPromptStruct(pos);
	str->mode = mode;
	SetArraySS(pos, 0, value);
	*ret = pos;
}

void push_prompt(Execute ptr, addr value, PromptMode mode)
{
	addr symbol, pos;

	symbol_prompt(&symbol);
	prompt_heap(&pos, value, mode);
	pushspecial_control(ptr, symbol, pos);
}

void push_prompt_eval_loop(Execute ptr)
{
	addr value;
	strvect_char_heap(&value, "* ");
	push_prompt(ptr, value, prompt_eval);
}

void get_prompt(Execute ptr, addr *value, PromptMode *mode)
{
	addr pos;

	symbol_prompt(&pos);
	getspecial_local(ptr, pos, &pos);
	if (pos == Unbound) {
		*value = Nil;
		*mode = prompt_eval;
		return;
	}
	if (GetType(pos) != LISPSYSTEM_PROMPT) {
		*value = Nil;
		*mode = prompt_eval;
		return;
	}

	GetArraySS(pos, 0, value);
	*mode = PtrPromptStruct(pos)->mode;
}

void getvalue_prompt(Execute ptr, addr *ret)
{
	PromptMode ignore;
	get_prompt(ptr, ret, &ignore);
}

void getmode_prompt(Execute ptr, PromptMode *ret)
{
	addr ignore;
	get_prompt(ptr, &ignore, ret);
}


/*
 *  read_prompt
 */
int read_prompt_(Execute ptr, addr stream, int *result, addr *ret)
{
	addr control, symbol;

	/* *prompt-reading* */
	GetConst(SYSTEM_PROMPT_READING, &symbol);

	/* read-stream */
	push_control(ptr, &control);
	pushspecial_control(ptr, symbol, Nil);
	(void)read_stream_(ptr, stream, result, ret);
	return pop_control_(ptr, control);
}

int read_error_prompt_(Execute ptr, addr stream, addr *ret)
{
	int check;

	Return(read_prompt_(ptr, stream, &check, ret));
	if (check)
		return call_end_of_file_(ptr, stream);

	return 0;
}

