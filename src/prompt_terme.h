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

	/*  Unbound -> output  (ignore)
	 *  Nil     -> output  (first)
	 *  T       -> space   (reading)
	 */
	if (value == Unbound)
		return Result(ret, str);

	/* first */
	if (value == Nil)
		return Result(ret, str);

	/* reading */
	Return(eastasian_length_(str, &size, &errorp));
	if (errorp)
		string_length(str, &size);
	strvect_heap(&str, size);
	for (i = 0; i < size; i++)
		strvect_setc_unsafe(str, i, ' ');

	return Result(ret, str);
}

static int check_prompt_reading_p(addr pos)
{
	int check;
	unicode c;
	size_t size, i;

	if (! strvectp(pos))
		return 1;  /* EOF */
	strvect_length(pos, &size);
	check = 0;
	for (i = 0; i < size; i++) {
		strvect_getc(pos, i, &c);
		if (c == 0x0A || c == 0x0D)
			continue;
		if (! (c == ' ' || c == '\t')) {
			check = 1;
			break;
		}
	}

	return check;
}

static int check_prompt_reading_(Execute ptr, addr pos)
{
	addr symbol, value;

	GetConst(SYSTEM_PROMPT_READING, &symbol);
	getspecial_local(ptr, symbol, &value);
	if (value == Unbound)
		return 0;

	/* value == Nil */
	if (value == Nil) {
		if (! check_prompt_reading_p(pos))
			return 0;
	}

	/* value != Nil */
	setspecial_local(ptr, symbol, T);
	return 0;
}

int input_prompt_(Execute ptr, addr *ret)
{
	LocalHold hold;
	PromptMode mode;
	addr pos;

	hold = LocalHold_array(ptr, 2);
	Return(input_prompt_reading_(ptr, &pos, &mode));
	localhold_set(hold, 0, pos);
	Return(prompt_terme_(ptr, pos, mode));
	Return(readline_terme_(ptr, &pos));
	localhold_set(hold, 1, pos);
	Return(check_prompt_reading_(ptr, pos));
	localhold_end(hold);

	return Result(ret, pos);
}

int clear_prompt_(void)
{
	return clear_terme_(Execute_Thread);
}

