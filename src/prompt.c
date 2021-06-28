#include "control_object.h"
#include "heap.h"
#include "prompt.h"
#include "strvect.h"
#include "symbol.h"
#include "typedef.h"

static void symbol_prompt(addr *ret)
{
	GetConst(SYSTEM_PROMPT, ret);
}

static void prompt_heap(addr *ret, addr value, enum prompt_mode mode)
{
	addr pos;
	struct prompt_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_PROMPT, 1, sizeoft(struct prompt_struct));
	str = PtrPromptStruct(pos);
	str->mode = mode;
	SetArraySS(pos, 0, value);
	*ret = pos;
}

void push_prompt(Execute ptr, addr value, enum prompt_mode mode)
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

void get_prompt(Execute ptr, addr *value, enum prompt_mode *mode)
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
	enum prompt_mode ignore;
	get_prompt(ptr, ret, &ignore);
}

void getmode_prompt(Execute ptr, enum prompt_mode *ret)
{
	addr ignore;
	get_prompt(ptr, &ignore, ret);
}

