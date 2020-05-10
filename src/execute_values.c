#include "build.h"
#include "cons.h"
#include "cons_list.h"
#include "execute.h"
#include "execute_values.h"
#include "object.h"
#include "thread.h"

_g void clear_values_execute(Execute ptr)
{
	addr *values;
	size_t size;

	size = ptr->sizer;
	if (EXECUTE_VALUES < size)
		return;
	values = ptr->values;
	for (; size < EXECUTE_VALUES; size++)
		values[size] = Unbound;
	*(ptr->values_list) = Unbound;
}

_g void setresult_control(Execute ptr, addr value)
{
	ptr->values[0] = value;
	ptr->sizer = 1;
}

_g void setbool_control(Execute ptr, int value)
{
	ptr->values[0] = value? T: Nil;
	ptr->sizer = 1;
}

static void pushvalues_control(Execute ptr, size_t i, addr pos)
{
	addr *values;

	Check(i < EXECUTE_VALUES, "values error");
	values = ptr->values_list;
	if (i == EXECUTE_VALUES)
		conscar_heap(values, pos);
	else
		cons_heap(values, pos, *values);
}

static void nreverse_values_control(Execute ptr)
{
	addr *values;

	if (EXECUTE_VALUES < ptr->sizer) {
		values = ptr->values_list;
		nreverse_list_unsafe(values, *values);
	}
}

_g void setvalues_control(Execute ptr, ...)
{
	addr pos, *values;
	va_list args;
	size_t i;

	setvalues_nil_control(ptr);
	va_start(args, ptr);
	values = ptr->values;
	for (i = 0; ; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		Check(GetStatusDynamic(pos), "dynamic error");
		if (i < EXECUTE_VALUES - 1) {
			values[i] = pos;
		}
		else if (i == EXECUTE_VALUES - 1) {
			values[i] = pos;
			*(ptr->values_list) = Nil;
		}
		else {
			pushvalues_control(ptr, i, pos);
		}
	}
	va_end(args);
	ptr->sizer = i;
	nreverse_values_control(ptr);
}

_g void setvalues_nil_control(Execute ptr)
{
	ptr->sizer = 0;
}

_g void setvalues_list_control(Execute ptr, addr list)
{
	addr pos, *values;
	size_t i;

	values = ptr->values;
	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		Check(GetStatusDynamic(pos), "dynamic error");
		if (i < EXECUTE_VALUES - 1) {
			values[i] = pos;
		}
		else if (i == EXECUTE_VALUES - 1) {
			values[i] = pos;
			*(ptr->values_list) = Nil;
		}
		else {
			pushvalues_control(ptr, i, pos);
		}
	}
	ptr->sizer = i;
	nreverse_values_control(ptr);
}

_g void getresult_control(Execute ptr, addr *ret)
{
	*ret = ptr->sizer? ptr->values[0]: Nil;
}

_g void getvalues_control(Execute ptr, size_t index, addr *ret)
{
	if (ptr->sizer <= index) {
		*ret = Unbound;
		return;
	}
	if (index < EXECUTE_VALUES) {
		*ret = ptr->values[index];
	}
	else {
		index -= EXECUTE_VALUES;
		getnth_unsafe(*(ptr->values_list), index, ret);
	}
}

static void list_from_vector_control(LocalRoot local,
		addr *values, size_t size, addr cons, addr *ret)
{
	size_t i;

	Check(size == 0, "size error");
	Check(EXECUTE_VALUES < size, "size error");
	for (i = size - 1; ; i--) {
		cons_alloc(local, &cons, values[i], cons);
		if (i <= 0)
			break;
	}
	*ret = cons;
}

static void getvalues_list_control(Execute ptr, LocalRoot local, addr *ret)
{
	addr cons;
	size_t size;

	size = ptr->sizer;
	if (size == 0) {
		*ret = Nil;
		return;
	}
	if (size <= EXECUTE_VALUES) {
		list_from_vector_control(local, ptr->values, size, Nil, ret);
	}
	else {
		cons = *ptr->values_list;
		copy_list_alloc_unsafe(local, &cons, cons);
		list_from_vector_control(local, ptr->values, EXECUTE_VALUES, cons, ret);
	}
}

_g void getvalues_list_control_local(Execute ptr, addr *ret)
{
	getvalues_list_control(ptr, ptr->local, ret);
}

_g void getvalues_list_control_heap(Execute ptr, addr *ret)
{
	getvalues_list_control(ptr, NULL, ret);
}

_g size_t lengthvalues_control(Execute ptr)
{
	return ptr->sizer;
}

