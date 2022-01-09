#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "control_object.h"
#include "eval_copy.h"
#include "execute.h"
#include "parse_function.h"
#include "parse_object.h"
#include "scope_object.h"
#include "step.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  step macro
 */
int step_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, expr, locally, declare, optimize, debug, value, step;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* `(locally
	 *    (declare (optimize (debug 3)))
	 *    (system::step ,expr))
	 */
	GetConst(COMMON_LOCALLY, &locally);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_OPTIMIZE, &optimize);
	GetConst(COMMON_DEBUG, &debug);
	GetConst(SYSTEM_STEP, &step);
	fixnum_heap(&value, 3);
	list_heap(&debug, debug, value, NULL);
	list_heap(&optimize, optimize, debug, NULL);
	list_heap(&declare, declare, optimize, NULL);
	list_heap(&step, step, expr, NULL);
	list_heap(ret, locally, declare, step, NULL);
	return 0;

error:
	return fmte_("The form ~S must be (expr).", form, NULL);
}


/*
 *  parse
 */
int parse_step_(Execute ptr, addr *ret, addr form)
{
	addr args, expr, eval;

	if (! consp_getcons(form, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* step */
	Return(parse_execute_(ptr, &expr, expr));
	eval_parse_heap(&eval, EVAL_PARSE_STEP, 1);
	SetEvalParse(eval, 0, expr);
	return Result(ret, eval);

error:
	*ret = Nil;
	return fmte_("The form ~S must be (eval).", form, NULL);
}


/*
 *  copy-eval
 */
void copy_eval_step(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_STEP, "parse error");
	GetEvalParse(eval, 0, &expr);
	copy_eval_parse(local, &expr, expr);
	eval_parse_alloc(local, &eval, type, 1);
	SetEvalParse(eval, 0, expr);
	*ret = eval;
}


/*
 *  scope
 */
int scope_step_(Execute ptr, addr *ret, addr eval)
{
	addr expr, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	Return(eval_scope_size_(ptr, &eval, 1, EVAL_PARSE_STEP, type, eval));
	SetEvalScopeIndex(eval, 0, expr);
	return Result(ret, eval);
}


/*
 *  initialize
 */
void init_parse_step(Execute ptr)
{
	addr symbol;

	GetConst(SYSTEM_STEP_BEGIN, &symbol);
	pushspecial_control(ptr, symbol, Nil);
}

