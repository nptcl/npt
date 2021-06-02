#include "call_symbols.h"
#include "code_queue.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute_values.h"
#include "load_code.h"
#include "object.h"
#include "symbol.h"

static void load_array_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_ARRAY, ret);
}


/*
 *  execute
 */
void execute_load_alloc(Execute ptr, size_t size)
{
	addr symbol, array;

	load_array_symbol(&symbol);
	vector_heap(&array, size);
	setspecial_local(ptr, symbol, array);
}

int execute_load_gensym_(Execute ptr, addr pos, size_t index)
{
	addr symbol, array;

	/* gensym */
	make_symbol_common(pos, &pos);
	/* set array &*/
	load_array_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &array));
	CheckType(array, LISPTYPE_VECTOR);
	setarray(array, index, pos);

	return 0;
}

int execute_load_set_(Execute ptr, size_t index)
{
	addr symbol, array, value;

	load_array_symbol(&symbol);
	getresult_control(ptr, &value);
	Return(getspecialcheck_local_(ptr, symbol, &array));
	CheckType(array, LISPTYPE_VECTOR);
	setarray(array, index, value);

	return 0;
}

int execute_load_get_(Execute ptr, size_t index, addr *ret)
{
	addr symbol, array;

	load_array_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &array));
	CheckType(array, LISPTYPE_VECTOR);
	getarray(array, index, ret);

	return 0;
}


/*
 *  load-time-value
 */
void code_make_load_alloc(LocalRoot local, addr *ret, addr index)
{
	addr code;
	LocalStack stack;

	push_local(local, &stack);
	code_queue_local(local, &code);
	CodeQueue_cons(local, code, LOAD_ALLOC, index);
	code_queue_pop(local, code, ret);
	rollback_local(local, stack);
}

void code_make_load_gensym(LocalRoot local, addr *ret, addr list)
{
	addr code, x, y;
	LocalStack stack;

	/* code-begin */
	push_local(local, &stack);
	code_queue_local(local, &code);

	/* code gensym */
	while (list != Nil) {
		GetCons(list, &x, &list);
		GetCons(x, &x, &y);
		GetNameSymbol(x, &x);
		CodeQueue_double(local, code, LOAD_GENSYM, x, y);
	}

	/* code-end */
	code_queue_pop(local, code, ret);
	rollback_local(local, stack);
}

void code_make_load_set(LocalRoot local, addr *ret, addr index)
{
	addr code;
	LocalStack stack;

	push_local(local, &stack);
	code_queue_local(local, &code);
	CodeQueue_cons(local, code, LOAD_SET, index);
	code_queue_pop(local, code, ret);
	rollback_local(local, stack);
}


/*
 *  initialize
 */
void fasl_load_time_value(Execute ptr)
{
	addr symbol;

	/* *load-array* */
	load_array_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

