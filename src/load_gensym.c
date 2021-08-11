#include "array.h"
#include "array_access.h"
#include "callname.h"
#include "clos.h"
#include "code_object.h"
#include "compile_file.h"
#include "constant.h"
#include "control_object.h"
#include "execute.h"
#include "hashtable.h"
#include "load_gensym.h"
#include "load_instance.h"
#include "load_time_value.h"
#include "pathname_object.h"
#include "quote.h"
#include "symbol.h"
#include "type_memory.h"

static void load_gensym_symbol(addr *ret)
{
	GetConst(SYSTEM_LOAD_GENSYM, ret);
}

void init_load_gensym(Execute ptr)
{
	addr symbol;
	load_gensym_symbol(&symbol);
	pushspecial_control(ptr, symbol, Nil);
}

void disable_load_gensym(Execute ptr)
{
	addr symbol;
	load_gensym_symbol(&symbol);
	pushspecial_control(ptr, symbol, Unbound);
}

int list_load_gensym_(Execute ptr, addr *ret)
{
	addr symbol;
	load_gensym_symbol(&symbol);
	return getspecialcheck_local_(ptr, symbol, ret);
}

static int intern_load_gensym_(Execute ptr, addr pos)
{
	addr index, symbol, cons;

	Return(get_load_table_(ptr, pos, &index));
	if (index != Nil)
		return 0;

	/* intern */
	Return(incf_load_size_(ptr, &index));
	Return(intern_load_table_(ptr, pos, index));

	/* push */
	load_gensym_symbol(&symbol);
	Return(getspecialcheck_local_(ptr, symbol, &cons));
	cons_heap(&pos, pos, index);
	cons_heap(&cons, pos, cons);
	setspecial_local(ptr, symbol, cons);

	return 0;
}


/*
 *  load_value
 */
static int load_value_call_(Execute ptr, addr pos);
static int load_value_cons_(Execute ptr, addr pos)
{
	addr car, cdr;

	CheckType(pos, LISPTYPE_CONS);
	GetCons(pos, &car, &cdr);
	Return(load_value_call_(ptr, car));
	Return(load_value_call_(ptr, cdr));

	return 0;
}

static int load_value_array_(Execute ptr, addr pos)
{
	addr x;
	size_t size, i;
	struct array_struct *str;

	CheckType(pos, LISPTYPE_ARRAY);
	if (! array_general_p(pos))
		return 0;

	str = ArrayInfoStruct(pos);
	size = str->size;
	for (i = 0; i < size; i++) {
		Return(array_get_t_(pos, i, &x));
		Return(load_value_call_(ptr, x));
	}

	return 0;
}

static int load_value_vector_(Execute ptr, addr pos)
{
	addr x;
	size_t size, i;

	CheckType(pos, LISPTYPE_VECTOR);
	lenarray(pos, &size);
	for (i = 0; i < size; i++) {
		getarray(pos, i, &x);
		Return(load_value_call_(ptr, x));
	}

	return 0;
}

static int load_value_hashtable_(Execute ptr, addr pos)
{
	addr key, value;
	LocalRoot local;
	LocalStack stack;

	CheckType(pos, LISPTYPE_HASHTABLE);
	local = ptr->local;
	push_local(local, &stack);
	hash_iterator_local(local, &pos, pos);
	while (next_hash_iterator(pos, &key, &value)) {
		Return(load_value_call_(ptr, key));
		Return(load_value_call_(ptr, value));
	}
	rollback_local(local, stack);

	return 0;
}

static int load_value_symbol_(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_SYMBOL);
	if (gensymp(pos))
		return intern_load_gensym_(ptr, pos);

	return 0;
}

static int load_value_callname_(Execute ptr, addr pos)
{
	CheckType(pos, LISPTYPE_CALLNAME);
	GetCallName(pos, &pos);
	return load_value_call_(ptr, pos);
}

static int load_value_pathname_(Execute ptr, addr pos)
{
	addr value;

	CheckType(pos, LISPTYPE_PATHNAME);
	/* name */
	GetHostPathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* device */
	GetDevicePathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* directory */
	GetDirectoryPathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* name */
	GetNamePathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* type */
	GetTypePathname(pos, &value);
	Return(load_value_call_(ptr, value));
	/* version */
	GetVersionPathname(pos, &value);
	Return(load_value_call_(ptr, value));

	return 0;
}

static int load_value_quote_(Execute ptr, addr pos)
{
	addr x, y;

	CheckType(pos, LISPTYPE_QUOTE);
	GetQuote(pos, 0, &x);
	GetQuote(pos, 1, &y);
	Return(load_value_call_(ptr, x));
	Return(load_value_call_(ptr, y));

	return 0;
}

static int load_value_type_(Execute ptr, addr pos)
{
	addr value;
	size_t size, i;

	CheckType(pos, LISPTYPE_TYPE);
	LenArrayType(pos, &size);
	for (i = 0; i < size; i++) {
		GetArrayType(pos, i, &value);
		Return(load_value_call_(ptr, value));
	}

	return 0;
}

static int load_value_call_(Execute ptr, addr pos)
{
	Check(! eval_compile_p(ptr), "eval-compile-p error.");

	switch (GetType(pos)) {
		case LISPTYPE_CLOS:
			return intern_load_instance_(ptr, pos);

		case LISPTYPE_CONS:
			return load_value_cons_(ptr, pos);

		case LISPTYPE_ARRAY:
			return load_value_array_(ptr, pos);

		case LISPTYPE_VECTOR:
			return load_value_vector_(ptr, pos);

		case LISPTYPE_HASHTABLE:
			return load_value_hashtable_(ptr, pos);

		case LISPTYPE_SYMBOL:
			return load_value_symbol_(ptr, pos);

		case LISPTYPE_CODE:
			return load_value_code_(ptr, pos);

		case LISPTYPE_CALLNAME:
			return load_value_callname_(ptr, pos);

		case LISPTYPE_PATHNAME:
			return load_value_pathname_(ptr, pos);

		case LISPTYPE_QUOTE:
			return load_value_quote_(ptr, pos);

		case LISPTYPE_TYPE:
			return load_value_type_(ptr, pos);

		default:
			return 0;
	}
}

int load_value_(Execute ptr, addr pos)
{
	if (! eval_compile_p(ptr))
		return 0;
	return load_value_call_(ptr, pos);
}


/*
 *  code
 */
int load_value_code_(Execute ptr, addr code)
{
	addr array, pos;
	struct code_struct *str;
	size_t size, i;

	CheckType(code, LISPTYPE_CODE);
	Check(! eval_compile_p(ptr), "mode error");
	GetArrayCode(code, Code_Array, &array);
	str = StructCode(code);
	size = str->size;

	for (i = 0; i < size; i++) {
		getarray(array, i, &pos);
		Return(load_value_call_(ptr, pos));
	}

	return 0;
}

