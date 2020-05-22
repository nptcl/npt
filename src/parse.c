#include "callname.h"
#include "condition.h"
#include "control_object.h"
#include "gc.h"
#include "integer.h"
#include "parse.h"
#include "parse_function.h"
#include "parse_macro.h"

_g void check_variable(addr symbol)
{
	if (! IsSymbol(symbol))
		fmte("The variable ~S must be a symbol.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		fmte("The variable ~S don't allow constant symbol.", symbol, NULL);
}

_g void check_function_variable(addr symbol)
{
	addr check;

	if (IsSymbol(symbol)) {
		if (GetStatusReadOnly(symbol))
			fmte("The variable ~S don't allow constant symbol.", symbol, NULL);
	}
	else if (callnamep(symbol)) {
		GetCallName(symbol, &check);
		if (! IsSymbol(check))
			fmte("The variable ~S must be a symbol.", check, NULL);
		if (constantp_callname(symbol))
			fmte("The variable ~S don't allow constant symbol.", check, NULL);
	}
	else {
		fmte("The ~S don't allow variable.", symbol, NULL);
	}
}

_g int tagbody_tag_p(addr pos)
{
	/*
	 * Common Lisp the Language, 2nd Edition
	 * 7.8.5. The ``Program Feature''
	 * a symbol or an integer, in which case it is called a tag, ...
	 */
	return IsSymbol(pos) || integerp(pos);
}

_g int eval_parse(Execute ptr, addr *ret, addr pos)
{
	addr control;
	LocalHold hold;

	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	init_parse_environment(ptr);
	Return(parse_execute(ptr, ret, pos));
	localhold_set(hold, 0, *ret);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	return 0;
}

