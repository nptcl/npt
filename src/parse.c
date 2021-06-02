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


/*
 *  compile toplevel
 */
#define ParseCompileToplevel(pos, x) { \
	addr __check; \
	GetConst(COMMON_##x, &__check); \
	if (pos == __check) \
	return 1; \
}
static int parse_compile_toplevel_symbol(addr pos)
{
	ParseCompileToplevel(pos, DECLAIM);
	ParseCompileToplevel(pos, DEFCLASS);
	ParseCompileToplevel(pos, DEFINE_COMPILER_MACRO);
	ParseCompileToplevel(pos, DEFINE_CONDITION);
	ParseCompileToplevel(pos, DEFINE_MODIFY_MACRO);
	ParseCompileToplevel(pos, DEFINE_SETF_EXPANDER);
	ParseCompileToplevel(pos, DEFMACRO);
	ParseCompileToplevel(pos, DEFPACKAGE);
	ParseCompileToplevel(pos, DEFSETF);
	ParseCompileToplevel(pos, DEFSTRUCT);
	ParseCompileToplevel(pos, DEFTYPE);
	ParseCompileToplevel(pos, IN_PACKAGE);

	return 0;
}

int parse_compile_toplevel_(Execute ptr, addr expr, addr list, addr *ret)
{
	addr compile, load, exec, toplevel, mode, eval;

	/* compile */
	if (! eval_compile_p(ptr))
		goto return_throw;

	/* type */
	if (! consp(expr))
		goto return_throw;
	GetCar(expr, &expr);
	if (! parse_compile_toplevel_symbol(expr))
		goto return_throw;

	/* toplevel */
	Return(gettoplevel_eval_(ptr, &toplevel));
	if (toplevel == Nil)
		goto return_throw;

	/* :compile-toplevel */
	Return(get_compile_toplevel_eval_(ptr, &compile));
	if (compile != Nil)
		goto return_throw;

	/* compile-time-too */
	Return(get_compile_time_eval_(ptr, &mode));
	if (mode != Nil)
		goto return_throw;

	/* eval-when */
	Return(get_load_toplevel_eval_(ptr, &load));
	Return(get_execute_eval_(ptr, &exec));
	conscar_heap(&list, list);

	eval_parse_heap(&eval, EVAL_PARSE_EVAL_WHEN, 6);
	SetEvalParse(eval, 0, list);
	SetEvalParse(eval, 1, T);         /* :compile-toplevel */
	SetEvalParse(eval, 2, load);      /* :load-toplevel */
	SetEvalParse(eval, 3, exec);      /* :execute */
	SetEvalParse(eval, 4, toplevel);  /* toplevel */
	SetEvalParse(eval, 5, mode);      /* compile-time */
	return Result(ret, eval);

return_throw:
	return Result(ret, list);
}

