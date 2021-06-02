#include "compile_eval.h"
#include "compile_file.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control_object.h"
#include "eval_execute.h"
#include "hashtable.h"
#include "integer.h"
#include "load_depend.h"
#include "load_gensym.h"
#include "load_time_value.h"
#include "parse_function.h"
#include "parse_object.h"
#include "scope_object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  *load-table*
 */
static void load_table_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_TABLE, ret);
}

int intern_load_table_(Execute ptr, addr pos, addr value)
{
	addr symbol, table;

	load_table_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &table));
	Return(intern_hashheap_(table, pos, &pos));
	SetCdr(pos, value);

	return 0;
}

int get_load_table_(Execute ptr, addr pos, addr *ret)
{
	addr symbol, table;

	load_table_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &table));
	return findnil_hashtable_(table, pos, ret);
}

int get_index_load_table_(Execute ptr, addr pos, size_t *ret)
{
	Return(get_load_table_(ptr, pos, &pos));
	Check(pos == Nil, "nil error");
	GetIndex(pos, ret);

	return 0;
}


/*
 *  *load-size*
 */
static void load_size_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_SIZE, ret);
}

int incf_load_size_(Execute ptr, addr *ret)
{
	addr symbol, pos, value;
	size_t size;

	load_size_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &pos));
	Return(oneplus_integer_common_(ptr->local, pos, &value));
	setspecial_local(ptr, symbol, value);
	Return(getindex_integer_(pos, &size));
	index_heap(&pos, size);

	return Result(ret, pos);
}

int get_load_size_(Execute ptr, addr *ret)
{
	addr pos;
	size_t size;

	load_size_symbol(&pos);
	Return(getspecialcheck_local_(ptr, pos, &pos));
	Return(getindex_integer_(pos, &size));
	index_heap(&pos, size);

	return Result(ret, pos);
}


/*
 *  parse
 */
static int parse_load_time_value_make_(Execute ptr, addr *ret,
		addr value, addr readonly, addr index, addr type)
{
	addr eval;

	eval_parse_heap(&eval, EVAL_PARSE_LOAD_TIME_VALUE, 4);
	SetEvalParse(eval, 0, value);
	SetEvalParse(eval, 1, (readonly != Nil)? T: Nil);
	SetEvalParse(eval, 2, index);
	SetEvalParse(eval, 3, type);

	return Result(ret, eval);
}

static int parse_load_time_value_compile_(Execute ptr,
		addr *ret, addr value, addr readonly)
{
	addr index, type;
	Return(compile_partial_(ptr, value, &index, &type));
	return parse_load_time_value_make_(ptr, ret, value, readonly, index, type);
}

static int parse_load_time_value_eval_(Execute ptr,
		addr *ret, addr expr, addr readonly)
{
	Return(eval_result_partial_(ptr, expr, &expr));
	return parse_execute_(ptr, ret, expr);
}

int parse_load_time_value_(Execute ptr, addr *ret, addr form)
{
	addr args, expr, readonly;

	/* parse */
	if (! consp_getcons(form, &expr, &args))
		goto error;
	if (args == Nil)
		readonly = Nil;
	else if (! consp_getcons(args, &readonly, &args))
		goto error;
	if (args != Nil)
		goto error;

	/* mode */
	if (eval_compile_p(ptr))
		return parse_load_time_value_compile_(ptr, ret, expr, readonly);
	else
		return parse_load_time_value_eval_(ptr, ret, expr, readonly);

error:
	*ret = Nil;
	return fmte_("The form ~S must be "
			"(load-time-value expr &optional read-only-p)).", form, NULL);
}


/*
 *  copy
 */
void copy_eval_load_time_value(LocalRoot local, addr *ret, addr eval)
{
	EvalParse type;
	addr value, readonly, index, the;

	GetEvalParseType(eval, &type);
	Check(type != EVAL_PARSE_LOAD_TIME_VALUE, "parse error");
	GetEvalParse(eval, 0, &value);
	GetEvalParse(eval, 1, &readonly);
	GetEvalParse(eval, 2, &index);
	GetEvalParse(eval, 3, &the);

	eval_parse_alloc(local, &eval, type, 4);
	SetEvalParse(eval, 0, value);
	SetEvalParse(eval, 1, readonly);
	SetEvalParse(eval, 2, index);
	SetEvalParse(eval, 3, the);
	*ret = eval;
}


/*
 *  scope
 */
int scope_load_time_value_(Execute ptr, addr *ret, addr eval)
{
	addr value, readonly, index, type;

	if (! eval_compile_p(ptr)) {
		*ret = Nil;
		return fmte_("Invalid scope object: load-time-value.", NULL);
	}

	/* parse */
	GetEvalParse(eval, 0, &value);
	GetEvalParse(eval, 1, &readonly);
	GetEvalParse(eval, 2, &index);
	GetEvalParse(eval, 3, &type);

	/* eval */
	Return(eval_scope_size_(ptr, &eval, 3, EVAL_PARSE_LOAD_TIME_VALUE, type, Nil));
	SetEvalScopeIndex(eval, 0, value);
	SetEvalScopeIndex(eval, 1, index);
	SetEvalScopeIndex(eval, 2, readonly);

	/* result */
	return Result(ret, eval);
}


/*
 *  initialize
 */
static void init_load_symbol(Execute ptr)
{
	addr symbol, pos;

	/* *load-size* */
	load_size_symbol(&symbol);
	fixnum_heap(&pos, 0);
	pushspecial_control(ptr, symbol, pos);

	/* *load-table* */
	load_table_symbol(&symbol);
	hashtable_heap(&pos);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	pushspecial_control(ptr, symbol, pos);
}

static void disable_load_symbol(Execute ptr)
{
	addr symbol;

	/* *load-size* */
	load_size_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);

	/* *load-table* */
	load_table_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);
}

void init_load_time_value(Execute ptr)
{
	init_load_symbol(ptr);
	init_load_gensym(ptr);
	init_load_depend(ptr);
}

void disable_load_time_value(Execute ptr)
{
	disable_load_symbol(ptr);
	disable_load_gensym(ptr);
	disable_load_depend(ptr);
}

