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

void get_prompt_info(Execute ptr, addr *ret)
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

