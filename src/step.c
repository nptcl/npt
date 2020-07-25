#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "control_object.h"
#include "eval_copy.h"
#include "execute.h"
#include "parse_function.h"
#include "parse_object.h"
#include "scope_object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  step macro
 */
_g int step_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, eval, let, special, step;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &eval, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* `(let ((system::*step-break* t))
	 *    (system::step ,expr))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(SYSTEM_STEP_BREAK, &special);
	GetConst(SYSTEM_STEP, &step);
	list_heap(&special, special, T, NULL);
	list_heap(&special, special, NULL);
	list_heap(&step, step, eval, NULL);
	list_heap(ret, let, special, step, NULL);
	return 0;

error:
	return fmte_("The form ~S must be (eval).", form, NULL);
}


/*
 *  parse
 */
static void parse_step_symbol(Execute ptr, addr *ret)
{
	GetConst(SYSTEM_STEP_PARSE, ret);
}

_g void init_parse_step(Execute ptr)
{
	addr symbol;
	parse_step_symbol(ptr, &symbol);
	pushspecial_control(ptr, symbol, Nil);
}

static int parse_step_p(Execute ptr)
{
	addr symbol, value;
	parse_step_symbol(ptr, &symbol);
	getspecialcheck_local(ptr, symbol, &value);
	return value != Nil;
}

_g int parse_step(Execute ptr, addr *ret, addr form)
{
	addr args, expr, symbol, value;

	if (! consp_getcons(form, &expr, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* step */
	parse_step_symbol(ptr, &symbol);
	getspecialcheck_local(ptr, symbol, &value);
	setspecial_local(ptr, symbol, T);
	Return(parse_execute_(ptr, ret, expr));
	setspecial_local(ptr, symbol, value);
	return 0;

error:
	return fmte_("The form ~S must be (eval).", form, NULL);
}

_g void parse_step_object(Execute ptr, addr *ret, addr value, addr expr)
{
	addr eval;

	if (! parse_step_p(ptr)) {
		*ret = expr;
		return;
	}

	eval_parse_heap(&eval, EVAL_PARSE_STEP, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, value);
	*ret = eval;
}


/*
 *  copy-eval
 */
_g void copy_eval_step(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr expr, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_STEP, "parse error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &value);

	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, expr);
	SetEvalParse(eval, 1, value);
	*ret = eval;
}


/*
 *  scope
 */
_g int scope_step(Execute ptr, addr *ret, addr eval)
{
	addr expr, value, type;
	LocalHold hold;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &expr);
	GetEvalParse(eval, 1, &value);

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);
	GetEvalScopeThe(expr, &type);

	eval_scope_size(ptr, &eval, 2, EVAL_PARSE_STEP, type, eval);
	SetEvalScopeIndex(eval, 0, expr);
	SetEvalScopeIndex(eval, 1, value);
	return Result(ret, eval);
}

