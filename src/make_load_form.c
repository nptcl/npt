#include "array.h"
#include "array_access.h"
#include "array_make.h"
#include "compile_file.h"
#include "cons.h"
#include "control_execute.h"
#include "control_object.h"
#include "condition.h"
#include "execute_values.h"
#include "hashtable.h"
#include "hold.h"
#include "integer.h"
#include "load_time_value.h"
#include "parse_function.h"
#include "parse_object.h"
#include "parse_typedef.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  hash-table
 */
static void make_load_form_symbol(Execute ptr, addr *ret)
{
	GetConst(SYSTEM_SPECIAL_MAKE_LOAD_FORM, ret);
}

_g void init_parse_make_load_form(Execute ptr)
{
	addr symbol, value;

	if (eval_compile_p(ptr)) {
		make_load_form_symbol(ptr, &symbol);
		hashtable_heap(&value);
		settest_hashtable(value, HASHTABLE_TEST_EQ);
		pushspecial_control(ptr, symbol, value);
	}
}

/* found=1, notfound=0 */
static int get_make_load_form_(Execute ptr, addr key, addr *ret)
{
	addr table;

	make_load_form_symbol(ptr, &table);
	getspecialcheck_local(ptr, table, &table);

	return find_hashtable_(table, key, ret);
}


/*
 *  make-load-form
 */
static int parse_make_load_form_object(Execute ptr, addr *ret, addr expr, addr init)
{
	addr eval;
	LocalHold hold;

	set_load_time_value_symbol(ptr, T);
	hold = LocalHold_local(ptr);
	Return(localhold_parse_self_(hold, ptr, expr));
	if (init != Nil) {
		Return(localhold_parse_self_(hold, ptr, init));
	}
	localhold_end(hold);

	/* eval */
	eval_parse_heap(&eval, EVAL_PARSE_LOAD_TIME_VALUE, 4);
	SetEvalParse(eval, 0, Nil);  /* nil */
	SetEvalParse(eval, 1, expr);
	SetEvalParse(eval, 2, Nil);  /* readonly */
	SetEvalParse(eval, 3, init);
	return Result(ret, eval);
}

static void parse_make_load_lambda_body(addr pos, addr g, addr value, addr *ret);
static void parse_make_load_lambda_cons(addr pos, addr g, addr value, addr *ret)
{
	addr car, cdr;

	GetCons(value, &car, &cdr);
	parse_make_load_lambda_body(pos, g, car, &car);
	parse_make_load_lambda_body(pos, g, cdr, &cdr);
	cons_heap(ret, car, cdr);
}

static void parse_make_load_lambda_vector(addr pos, addr g, addr value, addr *ret)
{
	addr vector, x;
	size_t size, i;

	lenarray(value, &size);
	vector_type_heap(&vector, value, size);
	for (i = 0; i < size; i++) {
		getarray(value, i, &x);
		parse_make_load_lambda_body(pos, g, x, &x);
		setarray(vector, i, x);
	}
	*ret = vector;
}

static void parse_make_load_lambda_array_heap(addr *ret, addr value)
{
	addr array, x;
	struct array_struct *str;

	array_empty_heap(&array);
	str = ArrayInfoStruct(array);
	*str = *ArrayInfoStruct(value);
	str->displaced = 0;
	str->simple = str->adjustable == 0 && str->fillpointer == 0;
	str->offset = 0;

	GetArrayInfo(value, ARRAY_INDEX_TYPE, &x);
	SetArrayInfo(array, ARRAY_INDEX_TYPE, x);
	GetArrayInfo(value, ARRAY_INDEX_DIMENSION, &x);
	SetArrayInfo(array, ARRAY_INDEX_DIMENSION, x);
	array_allocate(NULL, array, str);
	*ret = array;
}

static void parse_make_load_lambda_array_copy(addr pos, addr g, addr array, addr value)
{
	addr x;
	size_t size, i;
	struct array_struct *str;

	str = ArrayInfoStruct(array);
	size = str->front;
	for (i = 0; i < size; i++) {
		array_get_t(value, i, &x);
		parse_make_load_lambda_body(pos, g, x, &x);
		array_set(array, i, x);
	}
}

static void parse_make_load_lambda_array(addr pos, addr g, addr value, addr *ret)
{
	addr array;

	if (array_system_specialized_p(value)) {
		*ret = value;
		return;
	}
	parse_make_load_lambda_array_heap(&array, value);
	parse_make_load_lambda_array_copy(pos, g, array, value);
	*ret = array;
}

static void parse_make_load_lambda_body(addr pos, addr g, addr value, addr *ret)
{
	if (pos == value) {
		*ret = g;
		return;
	}

	switch (GetType(value)) {
		case LISPTYPE_CONS:
			parse_make_load_lambda_cons(pos, g, value, ret);
			break;

		case LISPTYPE_VECTOR:
			parse_make_load_lambda_vector(pos, g, value, ret);
			break;

		case LISPTYPE_ARRAY:
			parse_make_load_lambda_array(pos, g, value, ret);
			break;

		default:
			*ret = value;
			break;
	}
}

static int parse_make_load_lambda_(Execute ptr, addr pos, addr init, addr *ret)
{
	addr g, lambda;

	if (init == Nil)
		return Result(ret, Nil);

	/* (lambda (g) [replace pos -> g]) */
	Return(make_gensym_(ptr, &g));
	GetConst(COMMON_LAMBDA, &lambda);
	parse_make_load_lambda_body(pos, g, init, &init);
	list_heap(&g, g, NULL);
	list_heap(ret, lambda, g, init, NULL);

	return 0;
}

static int parse_make_load_form_generic(Execute ptr, addr pos, addr *ret1, addr *ret2)
{
	addr control, call, expr, init;
	LocalHold hold;

	hold = LocalHold_array(ptr, 2);
	/* (make-load-form clos) */
	push_new_control(ptr, &control);
	GetConst(COMMON_MAKE_LOAD_FORM, &call);
	GetFunctionSymbol(call, &call);
	Return(funcall_control(ptr, call, pos, NULL));
	/* result */
	getresult_control(ptr, &expr);
	localhold_set(hold, 0, expr);
	getvalues_control(ptr, 1, &init);
	if (init == Unbound)
		init = Nil;
	localhold_set(hold, 1, init);
	Return(free_control_(ptr, control));
	localhold_end(hold);

	*ret1 = expr;
	return parse_make_load_lambda_(ptr, pos, init, ret2);
}

static int parse_make_load_form(Execute ptr, addr *ret, addr pos)
{
	addr expr, init;

	Return(parse_make_load_form_generic(ptr, pos, &expr, &init));
	Return(parse_make_load_form_object(ptr, ret, expr, init));

	return 0;
}

_g int parse_clos(Execute ptr, addr *ret, addr pos)
{
	addr value;

	if (! eval_compile_p(ptr)) {
		eval_single_parse_heap(ret, EVAL_PARSE_CLOS, pos);
		return 0;
	}

	Return(get_make_load_form_(ptr, pos, &value));
	if (value != Unbound)
		return Result(ret, value);
	else
		return parse_make_load_form(ptr, ret, pos);
}


/*
 *  init
 */
static void compile_make_load_form_symbol(Execute ptr, addr *ret)
{
	GetConst(SYSTEM_COMPILE_MAKE_LOAD_FORM, ret);
}

_g void init_write_make_load_form(Execute ptr)
{
	addr symbol, index, table, cons;

	compile_make_load_form_symbol(ptr, &symbol);
	/* (index . hash-table) */
	fixnum_heap(&index, 0);
	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQL);
	cons_heap(&cons, index, table);
	pushspecial_control(ptr, symbol, cons);
}

_g void init_read_make_load_form(Execute ptr)
{
	addr symbol, table;

	compile_make_load_form_symbol(ptr, &symbol);
	/* hash-table */
	hashtable_heap(&table);
	settest_hashtable(table, HASHTABLE_TEST_EQL);
	pushspecial_control(ptr, symbol, table);
}

_g int get_write_make_load_form_(Execute ptr, addr key, addr *ret)
{
	int check;
	addr symbol, special, index, table, cons, value;

	/* table */
	compile_make_load_form_symbol(ptr, &symbol);
	getspecialcheck_local(ptr, symbol, &special);
	GetCons(special, &index, &table); 

	/* object */
	CheckType(key, LISPTYPE_LOAD_TIME_VALUE);
	make_index_integer_heap(&key, (size_t)key);
	Return(internp_hashheap_(table, key, &cons, &check));
	if (check) {
		GetCdr(cons, ret);
		return 0;
	}

	/* intern */
	SetCdr(cons, index);
	oneplus_integer_common(ptr->local, index, &value);
	SetCar(special, value);

	return Result(ret, index);
}

_g int get_read_make_load_form_(Execute ptr, addr key, addr *ret)
{
	int check;
	addr table, cons, value;

	/* table */
	compile_make_load_form_symbol(ptr, &table);
	getspecialcheck_local(ptr, table, &table);

	/* intern */
	Check(! integerp(key), "type error");
	Return(internp_hashheap_(table, key, &cons, &check));
	if (check) {
		GetCdr(cons, ret);
		return 0;
	}

	/* add value */
	load_time_value_heap(&value);
	SetCdr(cons, value);

	return Result(ret, value);
}

