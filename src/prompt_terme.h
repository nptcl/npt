#include "prompt.h"
#include "terme.h"
#include "typedef.h"

int input_prompt_(Execute ptr, addr *ret)
{
	PromptMode mode;
	addr pos;

	get_prompt(ptr, &pos, &mode);
	Return(prompt_terme_(ptr, pos, mode));
	Return(readline_terme_(ptr, ret));

	return 0;
}

