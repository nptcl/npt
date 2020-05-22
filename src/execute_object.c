#include <string.h>
#include "execute.h"
#include "execute_object.h"
#include "heap.h"
#include "local.h"
#include "typedef.h"

/*
 *  values
 */
static void execute_values_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_array4(local, ret, LISPSYSTEM_VALUES, size);
}
#define PtrExecuteValues(x)		((addr *)PtrArrayA4(x))

_g void init_execute_values(struct execute *bit)
{
	int i;
	addr pos, *values;

	execute_values_alloc(bit->local, &pos, EXECUTE_VALUES + 1);
	values = PtrExecuteValues(pos);
	for (i = 0; i < EXECUTE_VALUES + 1; i++)
		values[i] = Unbound;
	bit->values = values;
	bit->values_list = values + EXECUTE_VALUES;
	bit->sizer = 0;
}

_g void save_values_control(struct execute *ptr, addr *ret, size_t *rsize)
{
	addr pos;
	size_t size;

	size = ptr->sizer;
	if (EXECUTE_VALUES + 1 < size)
		size = EXECUTE_VALUES + 1;
	execute_values_alloc(ptr->local, &pos, size);
	memcpy(PtrExecuteValues(pos), ptr->values, sizeoft(addr) * size);
	*ret = pos;
	*rsize = size;
}

_g void restore_values_control(struct execute *ptr, addr pos, size_t size)
{
	if (EXECUTE_VALUES + 1 < size)
		size = EXECUTE_VALUES + 1;
	memcpy(ptr->values, PtrExecuteValues(pos), sizeoft(addr) * size);
	ptr->sizer = size;
}


/*
 *  lexical
 */
static void execute_lexical_alloc(LocalRoot local, addr *ret, size_t size)
{
	local_array4(local, ret, LISPSYSTEM_LEXICAL, size);
}
#define PtrExecuteLexical(x)		((addr *)PtrArrayA4(x))

_g void lexical_control(struct execute *ptr, size_t size)
{
	addr pos;

	if (size == 0) {
		ptr->lexical = NULL;
#ifdef LISP_DEBUG
		ptr->lexical_vector = Nil;
#endif
		return;
	}
	execute_lexical_alloc(ptr->local, &pos, size);
	ptr->lexical = PtrExecuteLexical(pos);
#ifdef LISP_DEBUG
	ptr->lexical_vector = pos;
#endif
}

_g void getlow_lexical_control(struct execute *ptr, size_t index, addr *ret)
{
#ifdef LISP_DEBUG
	addr pos;

	pos = ptr->lexical_vector;
	Check(pos == NULL, "lexical_vector error.");
	CheckType(pos, LISPSYSTEM_LEXICAL);
	GetArrayA4(pos, index, &pos);
	Check(ptr->lexical[index] != pos, "lexical check error.");
	*ret = pos;
#else
	*ret = ptr->lexical[index];
#endif
}

_g void setlow_lexical_control(struct execute *ptr, size_t index, addr value)
{
#ifdef LISP_DEBUG
	addr pos;

	pos = ptr->lexical_vector;
	Check(pos == NULL, "lexical_vector error.");
	CheckType(pos, LISPSYSTEM_LEXICAL);
	SetArrayA4(pos, index, value);
#else
	ptr->lexical[index] = value;
#endif
}

_g void get_lexical_control(struct execute *ptr, size_t index, addr *ret)
{
#ifdef LISP_DEBUG
	addr pos;
	getlow_lexical_control(ptr, index, &pos);
	getvalue_reference(pos, ret);
#else
	getvalue_reference(ptr->lexical[index], ret);
#endif
}

_g void set_lexical_control(struct execute *ptr, size_t index, addr value)
{
	addr pos;

#ifdef LISP_DEBUG
	getlow_lexical_control(ptr, index, &pos);
#else
	pos = ptr->lexical[index];
#endif
	if (GetType(pos) == LISPSYSTEM_REFERENCE)
		set_reference(pos, value);
	else
		ptr->lexical[index] = value;
}

_g void reference_lexical_control(struct execute *ptr, size_t index)
{
	addr pos;

#ifdef LISP_DEBUG
	getlow_lexical_control(ptr, index, &pos);
#else
	pos = ptr->lexical[index];
#endif
	if (GetType(pos) != LISPSYSTEM_REFERENCE) {
		reference_heap(&pos, pos);
		ptr->lexical[index] = pos;
	}
}


/*
 *  closure
 */
struct closure_struct {
	size_t lexical;
};
#define ClosureStruct(x) ((struct closure_struct *)PtrBodySS(x))

_g void closure_heap(addr *ret, addr value, size_t lexical)
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

_g void get_closure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_CLOSURE);
	GetArraySS(pos, 0, ret);
}

_g size_t lexical_closure(addr pos)
{
	CheckType(pos, LISPSYSTEM_CLOSURE);
	return ClosureStruct(pos)->lexical;
}


/*
 *  reference
 */
_g void reference_heap(addr *ret, addr value)
{
	Check(GetType(value) == LISPSYSTEM_REFERENCE, "type error");
	heap_array2(ret, LISPSYSTEM_REFERENCE, 1);
	SetArrayA2(*ret, 0, value);
}

_g void get_reference(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_REFERENCE);
	GetArrayA2(pos, 0, ret);
}

_g void set_reference(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_REFERENCE);
	SetArrayA2(pos, 0, value);
}

_g void getvalue_reference(addr pos, addr *ret)
{
	if (GetType(pos) == LISPSYSTEM_REFERENCE) {
		GetArrayA2(pos, 0, ret);
	}
	else {
		*ret = pos;
	}
}

