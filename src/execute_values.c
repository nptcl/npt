#include "build.h"
#include "cons.h"
#include "cons_list.h"
#include "execute.h"
#include "execute_object.h"
#include "execute_values.h"
#include "object.h"
#include "thread.h"

_g void clear_values_execute(Execute ptr)
{
	addr values;
	size_t size;

	size = ptr->sizer;
	if (EXECUTE_VALUES < size)
		return;
	values = ptr->values_vector;
	for (; size < EXECUTE_VALUES; size++) {
		SetExecuteValues(values, size, Unbound);
	}
	SetExecuteValuesList(values, Unbound);
}

_g void setresult_control(Execute ptr, addr value)
{
	SetExecuteValues(ptr->values_vector, 0, value);
	ptr->sizer = 1;
}

_g void setbool_control(Execute ptr, int value)
{
	SetExecuteValues(ptr->values_vector, 0, value? T: Nil);
	ptr->sizer = 1;
}

static void pushvalues_control(addr values, size_t i, addr pos)
{
	addr list;

	Check(i < EXECUTE_VALUES, "values error");
	if (i == EXECUTE_VALUES) {
		conscar_heap(&list, pos);
	}
	else {
		GetExecuteValuesList(values, &list);
		cons_heap(&list, pos, list);
	}
	SetExecuteValuesList(values, list);
}

static void nreverse_values_control(addr values, size_t sizer)
{
	addr list;

	if (EXECUTE_VALUES < sizer) {
		GetExecuteValuesList(values, &list);
		nreverse_list_unsafe(&list, list);
		SetExecuteValuesList(values, list);
	}
}

_g void setvalues_control(Execute ptr, ...)
{
	addr values, pos;
	va_list args;
	size_t i;

	setvalues_nil_control(ptr);
	va_start(args, ptr);
	values = ptr->values_vector;
	for (i = 0; ; i++) {
		pos = va_arg(args, addr);
		if (pos == NULL)
			break;
		Check(GetStatusDynamic(pos), "dynamic error");
		if (i < EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
		}
		else if (i == EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
			SetExecuteValuesList(values, Nil);
		}
		else {
			pushvalues_control(values, i, pos);
		}
	}
	va_end(args);
	ptr->sizer = i;
	nreverse_values_control(values, i);
}

_g void setvalues_nil_control(Execute ptr)
{
	ptr->sizer = 0;
}

_g void setvalues_list_control(Execute ptr, addr list)
{
	addr values, pos;
	size_t i;

	values = ptr->values_vector;
	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		Check(GetStatusDynamic(pos), "dynamic error");
		if (i < EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
		}
		else if (i == EXECUTE_VALUES - 1) {
			SetExecuteValues(values, i, pos);
			SetExecuteValuesList(values, Nil);
		}
		else {
			pushvalues_control(values, i, pos);
		}
	}
	ptr->sizer = i;
	nreverse_values_control(values, i);
}

_g void getresult_control(Execute ptr, addr *ret)
{
	*ret = ptr->sizer? ptr->values_reader[0]: Nil;
}

_g void getvalues_control(Execute ptr, size_t index, addr *ret)
{
	addr list;

	if (ptr->sizer <= index) {
		*ret = Unbound;
		return;
	}
	if (index < EXECUTE_VALUES) {
		*ret = ptr->values_reader[index];
	}
	else {
		index -= EXECUTE_VALUES;
		GetExecuteValuesList(ptr->values_vector, &list);
		getnth_unsafe(list, index, ret);
	}
}

static void list_from_vector_control(LocalRoot local,
		addr *values, size_t size, addr list, addr *ret)
{
	size_t i;

	Check(size == 0, "size error");
	Check(EXECUTE_VALUES < size, "size error");
	for (i = size - 1; ; i--) {
		cons_alloc(local, &list, values[i], list);
		if (i <= 0)
			break;
	}
	*ret = list;
}

static void getvalues_list_control(Execute ptr, LocalRoot local, addr *ret)
{
	addr list;
	size_t size;

	size = ptr->sizer;
	if (size == 0) {
		*ret = Nil;
		return;
	}
	if (size <= EXECUTE_VALUES) {
		list_from_vector_control(local, ptr->values_reader, size, Nil, ret);
	}
	else {
		GetExecuteValuesList(ptr->values_vector, &list);
		copy_list_alloc_unsafe(local, &list, list);
		list_from_vector_control(local, ptr->values_reader, EXECUTE_VALUES, list, ret);
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

