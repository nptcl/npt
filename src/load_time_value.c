#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_object.h"
#include "eval_copy.h"
#include "heap.h"
#include "hold.h"
#include "load_time_value.h"
#include "parse_function.h"
#include "parse_object.h"
#include "scope_object.h"
#include "symbol.h"

/*
 *  load-time-value
 */
void load_time_value_heap(addr *ret)
{
	heap_array2(ret, LISPTYPE_LOAD_TIME_VALUE, 1);
}

void get_load_time_value_heap(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	GetArrayA2(pos, 0, ret);
}

void set_load_time_value_heap(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	SetArrayA2(pos, 0, value);
}


/*
 *  *load-time-value*
 */
static void load_time_value_symbol(addr *ret)
{
	GetConst(SYSTEM_SPECIAL_LOAD_TIME_VALUE, ret);
}

static int get_load_time_value_symbol_(Execute ptr, addr *ret)
{
	addr symbol;
	load_time_value_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

void set_load_time_value_symbol(Execute ptr, addr value)
{
	addr symbol;
	load_time_value_symbol(&symbol);
	setspecial_local(ptr, symbol, value);
}


/*
 *  parse
 */
void init_parse_load_time_value(Execute ptr)
{
	addr symbol;
	load_time_value_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

int eval_parse_load_time_value(Execute ptr, addr *ret, addr pos)
{
	addr eval;

	Return(get_load_time_value_symbol_(ptr, &eval));
	if (eval == Nil)
		return Result(ret, pos);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOAD_TIME_VALUE, 2);
	SetEvalParse(eval, 0, T);
	SetEvalParse(eval, 1, pos);
	return Result(ret, eval);
}

int parse_load_time_value(Execute ptr, addr *ret, addr form)
{
	addr args, eval, expr, readonly;

	if (! consp_getcons(form, &expr, &args))
		goto error;
	if (args == Nil)
		readonly = Nil;
	else if (! consp_getcons(args, &readonly, &args))
		goto error;
	if (args != Nil)
		goto error;

	set_load_time_value_symbol(ptr, T);
	Return(parse_self_(ptr, expr));

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOAD_TIME_VALUE, 4);
	SetEvalParse(eval, 0, Nil);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, (readonly != Nil)? T: Nil);
	SetEvalParse(eval, 3, Nil); /* init */
	return Result(ret, eval);

error:
	return fmte_("The form ~S must be "
			"(load-time-value expr &optional read-only-p)).", form, NULL);
}


/*
 *  copy
 */
static void copy_eval_load_time_value_body(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr check, expr;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOAD_TIME_VALUE, "parse error");
	GetEvalParse(eval, 0, &check); /* T */
	GetEvalParse(eval, 1, &expr);
	Check(check == Nil, "check error");

	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 2);
	SetEvalParse(eval, 0, check); /* T */
	SetEvalParse(eval, 1, expr);
	*ret = eval;
}

static void copy_eval_load_time_value_expr(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr check, expr, readonly, init;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOAD_TIME_VALUE, "parse error");
	GetEvalParse(eval, 0, &check); /* Nil */
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &readonly);
	GetEvalParse(eval, 3, &init);
	Check(check != Nil, "check error");

	copy_eval_parse(local, &expr, expr);
	if (init != Nil)
		copy_eval_parse(local, &init, init);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, check); /* Nil */
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, readonly);
	SetEvalParse(eval, 3, init);
	*ret = eval;
}

void copy_eval_load_time_value(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr check;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOAD_TIME_VALUE, "parse error");
	GetEvalParse(eval, 0, &check);
	if (check != Nil)
		copy_eval_load_time_value_body(local, ret, eval);
	else
		copy_eval_load_time_value_expr(local, ret, eval);
}


/*
 *  scope
 */
void init_scope_load_time_value(Execute ptr)
{
	addr symbol;
	load_time_value_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

static int scope_load_time_value_list_(Execute ptr, addr *ret)
{
	addr list;

	Return(get_load_time_value_symbol_(ptr, &list));
	set_load_time_value_symbol(ptr, Nil);
	nreverse(ret, list);

	return 0;
}

static int scope_load_time_value_body(Execute ptr, addr *ret, addr eval)
{
	addr check, expr, list, type;

	GetEvalParse(eval, 0, &check); /* T */
	GetEvalParse(eval, 1, &expr);

	Return(scope_eval(ptr, &expr, expr));
	Return(scope_load_time_value_list_(ptr, &list));
	GetEvalScopeThe(expr, &type);

	/* eval */
	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_LOAD_TIME_VALUE, type, Nil));
	SetEvalScopeIndex(eval, 0, check); /* T */
	SetEvalScopeIndex(eval, 1, expr);
	SetEvalScopeIndex(eval, 2, list);
	return Result(ret, eval);
}

static int scope_load_time_value_expr(Execute ptr, addr *ret, addr eval)
{
	addr check, expr, readonly, init, type, value;
	LocalHold hold;

	GetEvalParse(eval, 0, &check); /* Nil */
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &readonly);
	GetEvalParse(eval, 3, &init);
	Check(check != Nil, "check error");

	hold = LocalHold_local(ptr);
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	if (init != Nil) {
		Return(localhold_scope_eval(hold, ptr, &init, init));
	}
	localhold_end(hold);

	GetEvalScopeThe(expr, &type);
	load_time_value_heap(&value);

	/* eval */
	Return(eval_scope_size_(ptr, &eval, 5, EVAL_PARSE_LOAD_TIME_VALUE, type, Nil));
	SetEvalScopeIndex(eval, 0, check); /* Nil */
	SetEvalScopeIndex(eval, 1, expr);
	SetEvalScopeIndex(eval, 2, readonly);
	SetEvalScopeIndex(eval, 3, init);
	SetEvalScopeIndex(eval, 4, value);

	/* push */
	Return(get_load_time_value_symbol_(ptr, &value));
	cons_heap(&value, eval, value);
	set_load_time_value_symbol(ptr, value);

	/* result */
	return Result(ret, eval);
}

int scope_load_time_value(Execute ptr, addr *ret, addr eval)
{
	addr check;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &check);
	if (check != Nil)
		return scope_load_time_value_body(ptr, ret, eval);
	else
		return scope_load_time_value_expr(ptr, ret, eval);
}


/*
 *  execute
 */
void execute_load_time_value_bind(Execute ptr, addr pos)
{
	addr value;

	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	getresult_control(ptr, &value);
	set_load_time_value_heap(pos, value);
}

void execute_load_time_value_init(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	get_load_time_value_heap(pos, &pos);
	pushargs_control(ptr, pos);
}

void execute_load_time_value_get(Execute ptr, addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	get_load_time_value_heap(pos, ret);
}

