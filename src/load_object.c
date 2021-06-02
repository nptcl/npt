#include "eval_execute.h"
#include "heap.h"
#include "load_object.h"
#include "typedef.h"

/*
 *  load-time-value
 */
void load_time_value_heap(addr *ret, addr value, addr index)
{
	addr pos;

	heap_array2(&pos, LISPTYPE_LOAD_TIME_VALUE, 2);
	SetArrayA2(pos, 0, value);
	SetArrayA2(pos, 1, index);
	*ret = pos;
}

void get_index_load_time_value(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_LOAD_TIME_VALUE);
	GetArrayA2(pos, 1, &pos);
	GetIndex(pos, ret);
}

int result_load_time_value_(Execute ptr, addr pos, addr *ret)
{
	if (GetType(pos) != LISPTYPE_LOAD_TIME_VALUE)
		return Result(ret, pos);

	GetArrayA2(pos, 0, &pos);
	Return(eval_result_partial_(ptr, pos, &pos));
	return Result(ret, pos);
}


