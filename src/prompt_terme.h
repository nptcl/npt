#include "constant.h"
#include "eastasian.h"
#include "hold.h"
#include "prompt.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "terme.h"
#include "typedef.h"

static int input_prompt_reading_(Execute ptr, addr *ret, PromptMode *mode)
{
	int errorp;
	addr symbol, str, value;
	size_t size, i;

	/* prompt */
	get_prompt(ptr, &str, mode);

	/* normal */
	GetConst(SYSTEM_PROMPT_READING, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value == Unbound)
		return Result(ret, str);

	/* Output prompt */
	if (value == Nil) {
		setspecial_local(ptr, symbol, T);
		return Result(ret, str);
	}

	/* Reading */
	Return(eastasian_length_(str, &size, &errorp));
	if (errorp)
		string_length(str, &size);
	strvect_heap(&str, size);
	for (i = 0; i < size; i++)
		strvect_setc_unsafe(str, i, ' ');

	return Result(ret, str);
}

int input_prompt_(Execute ptr, addr *ret)
{
	LocalHold hold;
	PromptMode mode;
	addr pos;

	hold = LocalHold_array(ptr, 1);
	Return(input_prompt_reading_(ptr, &pos, &mode));
	localhold_set(hold, 0, pos);
	Return(prompt_terme_(ptr, pos, mode));
	Return(readline_terme_(ptr, ret));
	localhold_end(hold);

	return 0;
}

int clear_prompt_(void)
{
	return clear_terme_(Execute_Thread);
}

