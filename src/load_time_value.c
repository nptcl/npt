#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_object.h"
#include "control_operator.h"
#include "eval_copy.h"
#include "heap.h"
#include "hold.h"
#include "load_time_value.h"
#include "parse_function.h"
#include "parse_object.h"
#include "scope_object.h"
#include "symbol.h"

/*
 *  load_time_value_heap
 */
struct load_time_value_struct {
	size_t size;
};

#define StructLoadTimeValue(x)	((struct load_time_value_struct *)PtrBodySS(x))
#define GetLoadTimeValue(x,y)	GetArraySS((x),0,(y))
#define SetLoadTimeValue(x,y)	SetArraySS((x),0,(y))

static void eval_load_time_value_heap(addr *ret)
{
	addr pos;
	struct load_time_value_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_LOAD_TIME_VALUE,
			1, sizeoft(struct load_time_value_struct));
	str = StructLoadTimeValue(pos);
	str->size = 0;
	*ret = pos;
}

static void copy_load_time_value_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr copy;
	struct load_time_value_struct *str1, *str2;

	alloc_smallsize(local, &copy, LISPSYSTEM_LOAD_TIME_VALUE,
			1, sizeoft(struct load_time_value_struct));
	str1 = StructLoadTimeValue(copy);
	str2 = StructLoadTimeValue(pos);
	memcpy(str1, str2, sizeoft(struct load_time_value_struct));
	GetLoadTimeValue(pos, &pos);
	SetLoadTimeValue(copy, pos);
	*ret = copy;
}


/*
 *  *load-time-value*
 */
static void eval_load_time_value_symbol(addr *ret)
{
	GetConst(SYSTEM_EVAL_LOAD_TIME_VALUE, ret);
}

static void get_eval_load_time_value_symbol(Execute ptr, addr *ret)
{
	addr symbol;
	eval_load_time_value_symbol(&symbol);
	getspecialcheck_local(ptr, symbol, ret);
}


/*
 *  parse
 */
_g void init_parse_load_time_value(Execute ptr)
{
	addr symbol, pos;

	eval_load_time_value_symbol(&symbol);
	eval_load_time_value_heap(&pos);
	pushspecial_control(ptr, symbol, pos);
}

_g int eval_parse_load_time_value(Execute ptr, addr *ret, addr pos)
{
	addr value, eval, list;
	struct load_time_value_struct *str;

	get_eval_load_time_value_symbol(ptr, &value);
	str = StructLoadTimeValue(value);
	if (str->size == 0)
		return Result(ret, pos);

	/* nreverse */
	GetLoadTimeValue(value, &list);
	nreverse(&list, list);
	SetLoadTimeValue(value, list);

	/* load-time-value */
	eval_parse_heap(&eval, EVAL_PARSE_LOAD_TIME_VALUE, 3);
	SetEvalParse(eval, 0, T);
	SetEvalParse(eval, 1, pos);
	SetEvalParse(eval, 2, value);
	return Result(ret, eval);
}

static void eval_parse_load_time_value_index(Execute ptr, addr *ret)
{
	addr value;
	struct load_time_value_struct *str;

	get_eval_load_time_value_symbol(ptr, &value);
	str = StructLoadTimeValue(value);
	index_heap(ret, str->size++);
}

static void eval_parse_load_time_value_push(Execute ptr, addr pos)
{
	addr value, list;

	get_eval_load_time_value_symbol(ptr, &value);
	GetLoadTimeValue(value, &list);
	cons_heap(&list, pos, list);
	SetLoadTimeValue(value, list);
}

_g int parse_load_time_value(Execute ptr, addr *ret, addr form)
{
	addr eval, index, expr, readonly;
	LocalHold hold;

	if (! consp_getcons(form, &expr, &form))
		goto error;
	if (form == Nil)
		readonly = Nil;
	else if (! consp_getcons(form, &readonly, &form))
		goto error;
	if (form != Nil)
		goto error;

	hold = LocalHold_local(ptr);
	Return(localhold_parse_self(hold, ptr, expr));
	localhold_end(hold);
	eval_parse_load_time_value_index(ptr, &index);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOAD_TIME_VALUE, 4);
	SetEvalParse(eval, 0, Nil);
	SetEvalParse(eval, 1, index);
	SetEvalParse(eval, 2, expr);
	SetEvalParse(eval, 3, (readonly != Nil)? T: Nil);
	eval_parse_load_time_value_push(ptr, eval);
	return Result(ret, eval);

error:
	fmte("The form ~S must be "
			"(load-time-value expr &optional read-only-p)).", form, NULL);
	return 0;
}


/*
 *  copy
 */
static void copy_eval_load_time_value_object(LocalRoot local, addr *ret, addr value)
{
	addr root, list, pos;

	GetLoadTimeValue(value, &list);
	copy_load_time_value_alloc(local, &value, value);
	root = Nil;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		copy_eval_parse(local, &pos, pos);
		cons_alloc(local, &root, pos, root);
	}
	nreverse(&root, root);
	SetLoadTimeValue(value, root);
	*ret = value;
}

static void copy_eval_load_time_value_body(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr check, expr, value;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOAD_TIME_VALUE, "parse error");
	GetEvalParse(eval, 0, &check); /* T */
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &value);
	Check(check == Nil, "check error");

	copy_eval_parse(local, &expr, expr);
	copy_eval_load_time_value_object(local, &value, value);

	eval_parse_alloc(local, &eval, type, 3);
	SetEvalParse(eval, 0, check);
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, value);
	*ret = eval;
}

static void copy_eval_load_time_value_expr(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr check, index, expr, readonly;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOAD_TIME_VALUE, "parse error");
	GetEvalParse(eval, 0, &check); /* Nil */
	GetEvalParse(eval, 1, &index);
	GetEvalParse(eval, 2, &expr);
	GetEvalParse(eval, 3, &readonly);
	Check(check != Nil, "check error");

	copy_eval_parse(local, &expr, expr);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, check);
	SetEvalParse(eval, 1, index);
	SetEvalParse(eval, 2, expr);
	SetEvalParse(eval, 3, readonly);
	*ret = eval;
}

_g void copy_eval_load_time_value(LocalRoot local, addr *ret, addr eval)
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
_g void init_scope_load_time_value(Execute ptr)
{
	addr symbol, pos;

	eval_load_time_value_symbol(&symbol);
	eval_load_time_value_heap(&pos);
	pushspecial_control(ptr, symbol, Nil);
}

static int scope_load_time_value_loop(Execute ptr, addr vector, addr eval, addr *ret)
{
	addr check, index, expr, readonly;
	size_t i;

	Check(! eval_parse_p(eval), "type error");
	GetEvalParse(eval, 0, &check); /* Nil */
	GetEvalParse(eval, 1, &index);
	GetEvalParse(eval, 2, &expr);
	GetEvalParse(eval, 3, &readonly);

	Check(check != Nil, "load-time-value error");
	Return(scope_eval(ptr, &expr, expr));
	list_heap(&eval, index, expr, readonly, NULL);

	GetIndex(index, &i);
	SetArrayA4(vector, i, eval);

	return Result(ret, eval);
}

static int scope_load_time_value_list(LocalHold hold,
		Execute ptr, addr vector, addr list, addr *ret)
{
	addr root, eval;

	root = Nil;
	while (list != Nil) {
		GetCons(list, &eval, &list);
		Return(scope_load_time_value_loop(ptr, vector, eval, &eval));
		cons_heap(&root, eval, root);
		localhold_set(hold, 0, root);
	}
	nreverse(ret, root);

	return 0;
}

static int scope_load_time_value_vector(LocalHold hold,
		Execute ptr, addr value, addr *rindex, addr *ret)
{
	addr vector, symbol;
	struct load_time_value_struct *str;

	/* special variable */
	str = StructLoadTimeValue(value);
	vector4_heap(&vector, str->size);
	eval_load_time_value_symbol(&symbol);
	setspecial_local(ptr, symbol, vector);

	/* scope */
	GetLoadTimeValue(value, &value);
	Return(scope_load_time_value_list(hold, ptr, vector, value, &value));

	/* result */
	index_heap(rindex, str->size);
	return Result(ret, value);
}

static int scope_load_time_value_body(Execute ptr, addr *ret, addr eval)
{
	addr check, expr, value, index, type;
	LocalHold hold;

	GetEvalParse(eval, 0, &check); /* T */
	GetEvalParse(eval, 1, &expr);
	GetEvalParse(eval, 2, &value);

	hold = LocalHold_array(ptr, 1);
	Return(scope_load_time_value_vector(hold, ptr, value, &index, &value));
	Return(localhold_scope_eval(hold, ptr, &expr, expr));
	localhold_end(hold);

	GetEvalScopeThe(expr, &type);
	eval_scope_size(ptr, &eval, 4, EVAL_PARSE_LOAD_TIME_VALUE, type, Nil);
	SetEvalScopeIndex(eval, 0, check); /* T */
	SetEvalScopeIndex(eval, 1, expr);
	SetEvalScopeIndex(eval, 2, index);
	SetEvalScopeIndex(eval, 3, value);
	return Result(ret, eval);
}

static void scope_load_time_value_find(Execute ptr, addr index, addr *ret)
{
	addr pos, expr, readonly;
	size_t i;

	eval_load_time_value_symbol(&pos);
	getspecialcheck_local(ptr, pos, &pos);
	GetIndex(index, &i);
	GetArrayA4(pos, i, &pos);
	List_bind(pos, &index, &expr, &readonly, NULL);
	GetEvalScopeThe(expr, ret);
}

static int scope_load_time_value_expr(Execute ptr, addr *ret, addr eval)
{
	addr check, index, expr, readonly, type;

	GetEvalParse(eval, 0, &check); /* Nil */
	GetEvalParse(eval, 1, &index);
	GetEvalParse(eval, 2, &expr);
	GetEvalParse(eval, 3, &readonly);
	Check(check != Nil, "check error");

	scope_load_time_value_find(ptr, index, &type);
	eval_scope_size(ptr, &eval, 4, EVAL_PARSE_LOAD_TIME_VALUE, type, Nil);
	SetEvalScopeIndex(eval, 0, check); /* Nil */
	SetEvalScopeIndex(eval, 1, index);
	SetEvalScopeIndex(eval, 2, Nil);
	SetEvalScopeIndex(eval, 3, readonly);
	return Result(ret, eval);
}

_g int scope_load_time_value(Execute ptr, addr *ret, addr eval)
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
_g void execute_load_time_value_alloc(Execute ptr, size_t size)
{
	addr symbol, pos;

	GetConst(SYSTEM_SPECIAL_LOAD_TIME_VALUE, &symbol);
	vector4_heap(&pos, size);
	pushspecial_control(ptr, symbol, pos);
}

_g void execute_load_time_value_value(Execute ptr, addr list)
{
	addr pos, index, readonly, value;
	size_t i;

	getresult_control(ptr, &value);
	GetConst(SYSTEM_SPECIAL_LOAD_TIME_VALUE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	List_bind(list, &index, &readonly, NULL);
	GetIndex(index, &i);
	SetArrayA4(pos, i, value);
}

_g void execute_load_time_value_get(Execute ptr, size_t index, addr *ret)
{
	addr pos;

	GetConst(SYSTEM_SPECIAL_LOAD_TIME_VALUE, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	GetArrayA4(pos, index, ret);
}

