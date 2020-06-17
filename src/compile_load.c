#include "compile_read.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "execute.h"
#include "hashtable.h"
#include "integer.h"
#include "load_time_value.h"
#include "symbol.h"
#include "typedef.h"

_g void eval_compile_init(Execute ptr)
{
	addr symbol, value;

	GetConst(SYSTEM_COMPILE_VALUE, &symbol);
	hashtable_heap(&value);
	settest_hashtable(value, HASHTABLE_TEST_EQL);
	pushspecial_control(ptr, symbol, value);
}

_g void eval_compile_value(Execute ptr, size_t pointer, addr *ret)
{
	addr table, pos, value;

	/* table */
	GetConst(SYSTEM_COMPILE_VALUE, &table);
	getspecialcheck_local(ptr, table, &table);
	/* key */
	make_index_integer_heap(&pos, pointer);
	/* intern */
	if (intern_hashheap(table, pos, &pos)) {
		GetCdr(pos, ret);
		return;
	}
	/* add value */
	load_time_value_heap(&value);
	SetCdr(pos, value);
	*ret = value;
}

static int eval_compile_load_loop(Execute ptr, addr stream)
{
	addr code;

	Return(faslread_value(ptr, stream, &code));
	CheckType(code, LISPTYPE_CODE);
	Return(runcode_control(ptr, code));

	return 0;
}

_g int eval_compile_load(Execute ptr, addr stream)
{
	/* header */
	if (faslread_header(stream)) {
		fmte("Invalid fasl header.", NULL);
		return 0;
	}

	/* fasl body */
	Return(eval_compile_load_loop(ptr, stream));

	/* footer */
	if (faslread_footer(stream)) {
		fmte("Invalid fasl footer.", NULL);
		return 0;
	}

	return 0;
}

