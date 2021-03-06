#include <string.h>
#include "execute.h"
#include "execute_object.h"
#include "heap.h"
#include "local.h"
#include "typedef.h"

/*
 *  values
 */
#define PtrExecuteValues(x)		((addr *)PtrArrayA4(x))

static void execute_values_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_array4(local, ret, LISPSYSTEM_VALUES, size);
}

void init_execute_values(struct execute *bit)
{
	int i;
	addr vector, *values;

	execute_values_alloc(bit->local, &vector, EXECUTE_VALUES + 1);
	values = PtrExecuteValues(vector);
	for (i = 0; i < EXECUTE_VALUES + 1; i++)
		values[i] = Unbound;
	bit->values_vector = vector;
	bit->values_reader = values;
	bit->sizer = 0;
}

void save_values_control(struct execute *ptr, addr *ret, size_t *rsize)
{
	addr pos, *reader;
	size_t i, size;

	size = ptr->sizer;
	if (EXECUTE_VALUES + 1 < size)
		size = EXECUTE_VALUES + 1;
	execute_values_alloc(ptr->local, &pos, size);
	reader = ptr->values_reader;
	for (i = 0; i < size; i++) {
		SetExecuteValues(pos, i, reader[i]);
	}
	*ret = pos;
	*rsize = size;
}

void restore_values_control(struct execute *ptr, addr pos, size_t size)
{
	addr vector, *reader;
	size_t i;

	if (EXECUTE_VALUES + 1 < size)
		size = EXECUTE_VALUES + 1;
	vector = ptr->values_vector;
	reader = PtrExecuteValues(pos);
	for (i = 0; i < size; i++) {
		SetExecuteValues(vector, i, reader[i]);
	}
	ptr->sizer = size;
}


/*
 *  throw
 */
void normal_throw_control(struct execute *ptr)
{
	ptr->throw_value = throw_normal;
	ptr->throw_handler = NULL;
	ptr->throw_control = NULL;
	ptr->throw_point = 0;
	ptr->throw_point_p = 0;
}

void save_throw_control(struct execute *ptr, struct execute_throw *save)
{
	save->throw_point_p = ptr->throw_point_p;
	save->throw_value = ptr->throw_value;
	save->throw_point = ptr->throw_point;
	save->throw_handler = ptr->throw_handler;
	save->throw_control = ptr->throw_control;
}

void restore_throw_control(struct execute *ptr, const struct execute_throw *save)
{
	ptr->throw_point_p = save->throw_point_p;
	ptr->throw_value = save->throw_value;
	ptr->throw_point = save->throw_point;
	ptr->throw_handler = save->throw_handler;
	ptr->throw_control = save->throw_control;
}


/*
 *  save
 */
static void execute_object_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_smallsize(local, ret, LISPSYSTEM_EXECUTE, 1, size);
}

void save_execute_control(struct execute *ptr, addr *ret)
{
	addr pos, values;
	struct execute_throw *str;
	size_t size;

	/* object */
	execute_object_alloc(ptr->local, &pos, sizeoft(struct execute_throw));

	/* throw */
	str = (struct execute_throw *)PtrBodySS(pos);
	save_throw_control(ptr, str);

	/* values */
	save_values_control(ptr, &values, &size);
	str->size = size;
	SetArraySS(pos, 0, values);

	/* result */
	*ret = pos;
}

void restore_execute_control(struct execute *ptr, addr pos)
{
	addr values;
	struct execute_throw *str;
	size_t size;

	CheckType(pos, LISPSYSTEM_EXECUTE);
	str = (struct execute_throw *)PtrBodySS(pos);
	size = str->size;
	GetArraySS(pos, 0, &values);
	restore_throw_control(ptr, str);
	restore_values_control(ptr, values, size);
}


/*
 *  lexical
 */
#define PtrExecuteLexical(x)	((addr *)PtrArrayA4(x))
#define GetExecuteLexical		GetArrayA4

static void execute_lexical_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_array4(local, ret, LISPSYSTEM_LEXICAL, size);
}

void lexical_control(struct execute *ptr, size_t size)
{
	addr pos;

	if (size == 0) {
		ptr->lexical_reader = NULL;
		ptr->lexical_vector = Nil;
	}
	else {
		execute_lexical_alloc(ptr->local, &pos, size);
		ptr->lexical_reader = PtrExecuteLexical(pos);
		ptr->lexical_vector = pos;
	}
}

void getlow_lexical_debug(struct execute *ptr, size_t index, addr *ret)
{
	addr pos;

	pos = ptr->lexical_vector;
	Check(pos == NULL, "lexical_vector error.");
	CheckType(pos, LISPSYSTEM_LEXICAL);
	GetExecuteLexical(pos, index, &pos);
	Check(ptr->lexical_reader[index] != pos, "lexical check error.");
	*ret = pos;
}

void setlow_lexical_debug(struct execute *ptr, size_t index, addr value)
{
	addr pos;

	pos = ptr->lexical_vector;
	Check(pos == NULL, "lexical_vector error.");
	CheckType(pos, LISPSYSTEM_LEXICAL);
	SetExecuteLexical(pos, index, value);
}

void get_lexical_control(struct execute *ptr, size_t index, addr *ret)
{
	addr pos;
	getlow_lexical_control(ptr, index, &pos);
	getvalue_reference(pos, ret);
}

void set_lexical_control(struct execute *ptr, size_t index, addr value)
{
	addr pos;

	getlow_lexical_control(ptr, index, &pos);
	if (GetType(pos) == LISPSYSTEM_REFERENCE)
		set_reference(pos, value);
	else
		setlow_lexical_control(ptr, index, value);
}

void reference_lexical_control(struct execute *ptr, size_t index)
{
	addr pos;

	getlow_lexical_control(ptr, index, &pos);
	if (GetType(pos) != LISPSYSTEM_REFERENCE) {
		reference_heap(&pos, pos);
		setlow_lexical_control(ptr, index, pos);
	}
}


/*
 *  closure
 */
struct closure_struct {
	size_t lexical;
};
#define ClosureStruct(x) ((struct closure_struct *)PtrBodySS(x))

void closure_heap(addr *ret, addr value, size_t lexical)
{
	addr pos;
	struct closure_struct *str;

	Check(GetType(value) == LISPSYSTEM_CLOSURE, "type error");
	heap_smallsize(&pos, LISPSYSTEM_CLOSURE, 1, sizeoft(struct closure_struct));
	SetArraySS(pos, 0, value);
	str = ClosureStruct(pos);
	str->lexical = lexical;
	*ret = pos;
}

void get_closure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_CLOSURE);
	GetArraySS(pos, 0, ret);
}

size_t lexical_closure(addr pos)
{
	CheckType(pos, LISPSYSTEM_CLOSURE);
	return ClosureStruct(pos)->lexical;
}


/*
 *  reference
 */
void reference_heap(addr *ret, addr value)
{
	Check(GetType(value) == LISPSYSTEM_REFERENCE, "type error");
	heap_array2(ret, LISPSYSTEM_REFERENCE, 1);
	SetArrayA2(*ret, 0, value);
}

void get_reference(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_REFERENCE);
	GetArrayA2(pos, 0, ret);
}

void set_reference(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_REFERENCE);
	SetArrayA2(pos, 0, value);
}

void getvalue_reference(addr pos, addr *ret)
{
	if (GetType(pos) == LISPSYSTEM_REFERENCE) {
		GetArrayA2(pos, 0, ret);
	}
	else {
		*ret = pos;
	}
}

