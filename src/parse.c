#include "callname.h"
#include "compile_file.h"
#include "condition.h"
#include "constant.h"
#include "eval_value.h"
#include "integer.h"
#include "parse.h"
#include "parse_object.h"
#include "symbol.h"
#include "typedef.h"

int check_variable_(addr symbol)
{
	if (! symbolp(symbol))
		return fmte_("The variable ~S must be a symbol.", symbol, NULL);
	if (GetStatusReadOnly(symbol))
		return fmte_("The variable ~S don't allow constant symbol.", symbol, NULL);

	return 0;
}

int check_function_variable_(addr symbol)
{
	addr check;

	if (symbolp(symbol)) {
		if (GetStatusReadOnly(symbol))
			return fmte_("The variable ~S don't allow constant symbol.", symbol, NULL);
	}
	else if (callnamep(symbol)) {
		GetCallName(symbol, &check);
		if (! symbolp(check))
			return fmte_("The variable ~S must be a symbol.", check, NULL);
		if (constantp_callname(symbol))
			return fmte_("The variable ~S don't allow constant symbol.", check, NULL);
	}
	else {
		return fmte_("The ~S don't allow variable.", symbol, NULL);
	}

	return 0;
}

int tagbody_tag_p(addr pos)
{
	/*
	 * Common Lisp the Language, 2nd Edition
	 * 7.8.5. The ``Program Feature''
	 * a symbol or an integer, in which case it is called a tag, ...
	 */
	return symbolp(pos) || integerp(pos);
}

