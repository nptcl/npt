#include "array_access.h"
#include "array_make.h"
#include "condition.h"
#include "cons_list.h"
#include "heap.h"
#include "integer.h"
#include "local.h"
#include "memory.h"
#include "object.h"
#include "paper.h"
#include "typedef.h"

void paper_array_alloc(LocalRoot local, addr *ret, size_t array)
{
	alloc_array(local, ret, LISPTYPE_PAPER, array);
	SetUser(*ret, 0);
}

void paper_array_local(LocalRoot local, addr *ret, size_t array)
{
	CheckLocal(local);
	paper_array_alloc(local, ret, array);
}

void paper_array_heap(addr *ret, size_t array)
{
	paper_array_alloc(NULL, ret, array);
}

void paper_body_alloc(LocalRoot local, addr *ret, size_t body)
{
	alloc_body(local, ret, LISPTYPE_PAPER, body);
	SetUser(*ret, 0);
}

void paper_body_local(LocalRoot local, addr *ret, size_t body)
{
	CheckLocal(local);
	paper_body_alloc(local, ret, body);
}

void paper_body_heap(addr *ret, size_t body)
{
	paper_body_alloc(NULL, ret, body);
}

static int paper_arraybody(LocalRoot local, addr *ret, size_t array, size_t body)
{
	if (array == 0) {
		paper_body_alloc(local, ret, body);
		return 0;
	}
	if (body == 0) {
		paper_array_alloc(local, ret, array);
		return 0;
	}
	if (array <= 0xFFULL && body <= 0xFFULL) {
		alloc_smallsize(local, ret, LISPTYPE_PAPER, array, body);
		SetUser(*ret, 0);
		return 0;
	}
	if (array <= 0xFFFFULL && body <= 0xFFFFULL) {
		alloc_arraybody(local, ret, LISPTYPE_PAPER, array, body);
		SetUser(*ret, 0);
		return 0;
	}
	*ret = Nil;
	return 1;
}

int paper_arraybody_alloc_(LocalRoot local, addr *ret, size_t array, size_t body)
{
	if (paper_arraybody(local, ret, array, body))
		return fmte_("Invalid paper size.", NULL);

	return 0;
}

int paper_arraybody_local_(LocalRoot local, addr *ret, size_t array, size_t body)
{
	CheckLocal(local);
	return paper_arraybody_alloc_(local, ret, array, body);
}

int paper_arraybody_heap_(addr *ret, size_t array, size_t body)
{
	return paper_arraybody_alloc_(NULL, ret, array, body);
}

int paperp(addr pos)
{
	return GetType(pos) == LISPTYPE_PAPER;
}

int paper_array_p(addr pos)
{
	if (! paperp(pos))
		return 0;
	switch (GetStatusSize(pos)) {
		case LISPSIZE_ARRAY2:
		case LISPSIZE_ARRAY4:
		case LISPSIZE_ARRAY8:
		case LISPSIZE_SMALLSIZE:
		case LISPSIZE_ARRAYBODY:
			return 1;

		default:
			return 0;
	}
}

int paper_body_p(addr pos)
{
	if (! paperp(pos))
		return 0;
	switch (GetStatusSize(pos)) {
		case LISPSIZE_SMALLSIZE:
		case LISPSIZE_ARRAYBODY:
		case LISPSIZE_BODY2:
		case LISPSIZE_BODY4:
		case LISPSIZE_BODY8:
			return 1;

		default:
			return 0;
	}
}

void paper_copy_body_alloc(LocalRoot local, addr *ret, addr pos)
{
#ifdef LISP_DEBUG
	int check;
#endif
	addr x, src, dst;
	size_t array, body;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &array);
	paper_len_body(pos, &body);
#ifdef LISP_DEBUG
	check = paper_arraybody(local, &x, array, body);
	Check(check, "paper_arraybody error.");
#else
	(void)paper_arraybody_alloc_(local, &x, array, body);
#endif
	*ret = x;
	if (body == 0)
		return;

	/* copy */
	posbody(pos, &src);
	posbody(x, &dst);
	memcpy(dst, src, body);
}

void paper_get_type(addr pos, byte *ret)
{
	CheckType(pos, LISPTYPE_PAPER);
	*ret = GetUser(pos);
}

void paper_set_type(addr pos, byte value)
{
	CheckType(pos, LISPTYPE_PAPER);
	SetUser(pos, value);
}

void paper_len_array(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_PAPER);
	if (paper_array_p(pos))
		lenarray(pos, ret);
	else
		*ret = 0;
}

void paper_len_body(addr pos, size_t *ret)
{
	CheckType(pos, LISPTYPE_PAPER);
	if (paper_body_p(pos))
		lenbody(pos, ret);
	else
		*ret = 0;
}

void paper_get_array(addr pos, size_t index, addr *ret)
{
	Check(! paper_array_p(pos), "type error");
	getarray(pos, index, ret);
}

void paper_set_array(addr pos, size_t index, addr value)
{
	Check(! paper_array_p(pos), "type error");
	setarray(pos, index, value);
}

void paper_get_body(addr pos, size_t index, byte *ret)
{
	addr body;
#ifdef LISP_DEBUG
	size_t size;

	Check(! paper_body_p(pos), "type error");
	lenbody(pos, &size);
	Check(size <= index, "size error");
#endif
	posbody(pos, &body);
	*ret = body[index];
}

void paper_set_body(addr pos, size_t index, byte value)
{
	addr body;
#ifdef LISP_DEBUG
	size_t size;

	Check(! paper_body_p(pos), "type error");
	lenbody(pos, &size);
	Check(size <= index, "size error");
#endif
	posbody(pos, &body);
	body[index] = value;
}

void paper_get_memory(addr pos, size_t a, size_t b, void *ptr, size_t *ret)
{
	addr body;
	size_t size;

	Check(! paper_body_p(pos), "type error");
	if (b <= a)
		goto zero;
	lenbody(pos, &size);
	if (size <= a)
		goto zero;
	b = (size < b)? size: b;
	size = b - a;
	posbody(pos, &body);
	memcpy(ptr, body + a, size);
	if (ret)
		*ret = size;
	return;

zero:
	if (ret)
		*ret = 0;
}

void paper_set_memory(addr pos, size_t a, size_t b, const void *ptr, size_t *ret)
{
	addr body;
	size_t size;

	Check(! paper_body_p(pos), "type error");
	if (b <= a)
		goto zero;
	lenbody(pos, &size);
	if (size <= a)
		goto zero;
	b = (size < b)? size: b;
	size = b - a;
	posbody(pos, &body);
	memcpy(body + a, ptr, size);
	if (ret)
		*ret = size;
	return;

zero:
	if (ret)
		*ret = 0;
}


/*
 *  syscall
 */
int paper_length_body_(addr pos, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

int paper_length_array_(addr pos, addr *ret)
{
	size_t size;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	make_index_integer_heap(ret, size);

	return 0;
}

int paper_list_body_(addr pos, addr *ret)
{
	byte c;
	addr list, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		paper_get_body(pos, i, &c);
		fixnum_heap(&x, (fixnum)c);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);

	return 0;
}

int paper_list_array_(addr pos, addr *ret)
{
	addr list, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		paper_get_array(pos, i, &x);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);

	return 0;
}

int paper_vector_body_(addr pos, addr *ret)
{
	byte c;
	addr array;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	Return(array_unsigned8_heap_(&array, size));
	for (i = 0; i < size; i++) {
		paper_get_body(pos, i, &c);
		Return(array_set_unsigned8_(array, i, c));
	}
	*ret = array;

	return 0;
}

int paper_vector_array_(addr pos, addr *ret)
{
	addr vector, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	vector_heap(&vector, size);
	for (i = 0; i < size; i++) {
		paper_get_array(pos, i, &x);
		setarray(vector, i, x);
	}
	*ret = vector;

	return 0;
}

int paper_get_type_(addr pos, addr *ret)
{
	byte c;

	CheckType(pos, LISPTYPE_PAPER);
	paper_get_type(pos, &c);
	fixnum_heap(ret, (fixnum)c);

	return 0;
}

int paper_set_type_(addr pos, addr second)
{
	byte c;

	CheckType(pos, LISPTYPE_PAPER);
	if (GetByte_integer(second, &c))
		return fmte_("Invalid paper type, ~S.", second, NULL);
	paper_set_type(pos, c);

	return 0;
}

int paper_get_array_(addr pos, addr index, addr *ret)
{
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i) {
		*ret = Nil;
		return fmte_("Too large index, ~S.", index, NULL);
	}
	paper_get_array(pos, i, ret);

	return 0;
}

int paper_set_array_(addr pos, addr index, addr value)
{
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_array(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i)
		return fmte_("Too large index, ~S.", index, NULL);
	paper_set_array(pos, i, value);

	return 0;
}

int paper_get_body_(addr pos, addr index, addr *ret)
{
	byte c;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i) {
		*ret = Nil;
		return fmte_("Too large index, ~S.", index, NULL);
	}
	paper_get_body(pos, i, &c);
	fixnum_heap(ret, (fixnum)c);

	return 0;
}

int paper_set_body_(addr pos, addr index, addr value)
{
	byte c;
	size_t size, i;

	CheckType(pos, LISPTYPE_PAPER);
	paper_len_body(pos, &size);
	Return(getindex_integer_(index, &i));
	if (size <= i)
		return fmte_("Too large index, ~S.", index, NULL);
	if (GetByte_integer(value, &c))
		return fmte_("Invalid paper value, ~S.", value, NULL);
	paper_set_body(pos, i, c);

	return 0;
}

