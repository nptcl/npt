#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "alloc.h"
#include "build.h"
#include "heap.h"
#include "local.h"
#include "memory.h"

#define LocalLimit		(40UL * 1024UL)

/*
 *  memory error
 */
static void memoryerror(void)
{
	Debug("local memory overflow.");
	exitthis(LISPCODE_MEMORY);
}


/*
 *  init / free
 */
addr lowlevel_unsafe(struct localroot *ptr, size_t size)
{
	addr front, check;

	AlignSize8Front(size, &size);
	front = ptr->front;
	check = front + size;
	if (ptr->tail < check) {
		Debug("stack overflow");
		return NULL;
	}
	ptr->front = check;
	CheckAlign8(front, "align8 error1");
	CheckAlign8(check, "align8 error2");

	return front;
}

addr lowlevel_local(struct localroot *ptr, size_t size)
{
	addr point;

	point = lowlevel_unsafe(ptr, size);
	if (point == NULL) {
		memoryerror();
	}

	return point;
}

static struct localcell *pushcell(struct localroot *ptr)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_unsafe(ptr, sizeoft(struct localcell));
	if (cell == NULL) {
		Debug("pushcell error");
		return NULL;
	}
#ifdef LISP_DEBUG
	memset(cell, 0xAA, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = ptr->cell;
	ptr->cell = cell;

	return cell;
}

addr alloc_local(struct localroot *ptr, size_t size)
{
	struct localcell *cell;
	addr pos;

	/* cell */
	cell = ptr->cell;
	if (LocalCount <= cell->count) {
		cell = pushcell(ptr);
		if (cell == NULL) {
			Debug("pushcell error");
			goto error;
		}
	}

	/* memory */
	pos = lowlevel_unsafe(ptr, size);
	if (pos == NULL) {
		Debug("lowlevel_unsafe error");
		goto error;
	}
	cell->point[cell->count++] = pos;
	return pos;

error:
	memoryerror();
	return NULL;
}

struct localroot *make_local(size_t size)
{
	struct localroot *ptr;
	addr alloc;

	if (size < LocalLimit)
		size = LocalLimit;

	ptr = malloctype(struct localroot);
	if (ptr == NULL) {
		Debug("malloctype error");
		return NULL;
	}

	alloc = (addr)malloc(size);
	if (alloc == NULL) {
		Debug("malloc error");
		goto error;
	}
#ifdef LISP_MEMORY_INIT
	memset(alloc, 0xAA, size);
#endif

	ptr->alloc = (void *)alloc;
	Align8Front(ptr->alloc, &(ptr->front));
	ptr->tail = size + alloc;
	ptr->size = size;
	ptr->stack = NULL;
	ptr->cell = NULL;
	CheckAlign8(ptr->front, "align8 error1");
	if (pushcell(ptr) == NULL) {
		Debug("pushcell error");
		goto error;
	}
	CheckAlign8(ptr->front, "align8 error2");
	return ptr;

error:
	free(ptr);
	return NULL;
}

void free_local(struct localroot *ptr)
{
	if (ptr) {
		free(ptr->alloc);
		free(ptr);
	}
}


/*
 *  stack
 */
void unsafe_push_local(struct localroot *ptr)
{
	struct localstack *stack;

	/* Don't use alloc_local */
	stack = (struct localstack *)lowlevel_unsafe(ptr, sizeof(struct localstack));
	if (stack == NULL) {
		Debug("lowlevel_unsafe error");
		memoryerror();
		return;
	}
	stack->stack = ptr->stack;
	stack->cell = ptr->cell;
	stack->cellcount = ptr->cell->count;
	ptr->stack = stack;
	CheckAlign8(ptr->front, "align8 error2");
}

void push_local(struct localroot *ptr, struct localstack **stack)
{
	unsafe_push_local(ptr);
	*stack = ptr->stack;
}

void unsafe_pop_local(struct localroot *ptr)
{
	struct localstack *stack;
#ifdef LISP_DEBUG
	addr front;
	size_t i;

	front = ptr->front;
#endif
	stack = ptr->stack;
	if (ptr->stack == NULL) {
		Align8Front(ptr->alloc, &(ptr->front));
		ptr->cell = NULL;
		pushcell(ptr);
	}
	else {
		ptr->front = (addr)stack;
		ptr->stack = stack->stack;
		ptr->cell = stack->cell;
		ptr->cell->count = stack->cellcount;
	}
#ifdef LISP_DEBUG
	memset(ptr->front, 0xAA, front - ptr->front);
	for (i = ptr->cell->count; i < LocalCount; i++)
		ptr->cell->point[i] = Unbound;
#endif
}

void rollback_local(struct localroot *ptr, struct localstack *stack)
{
#ifdef LISP_DEBUG
	struct localstack *root;
	addr front;
	size_t i;

	front = ptr->front;
#endif
	if (stack == NULL) {
		Align8Front(ptr->alloc, &(ptr->front));
		ptr->stack = NULL;
		ptr->cell = NULL;
		pushcell(ptr);
		Check(ptr->cell == NULL, "pushcell error");
	}
	else {
#ifdef LISP_DEBUG
		for (root = ptr->stack; root == stack; root = root->stack) {
			Check(root == NULL, "rollback_local check error");
		}
#endif
		ptr->front = (addr)stack;
		ptr->stack = stack->stack;
		ptr->cell = stack->cell;
		ptr->cell->count = stack->cellcount;
	}
#ifdef LISP_DEBUG
	memset(ptr->front, 0xAA, front - ptr->front);
	for (i = ptr->cell->count; i < LocalCount; i++)
		ptr->cell->point[i] = Unbound;
#endif
}

static int valid_range_local(struct localroot *ptr, const void *pos)
{
	return (ptr->alloc <= pos) && (pos <= (const void *)ptr->front);
}

int valid_local(struct localroot *ptr, const void *pos)
{
	return (! valid_heap(pos)) && valid_range_local(ptr, pos);
}

int valid_memory(struct localroot *ptr, const void *pos)
{
	return valid_heap(pos) || valid_range_local(ptr, pos);
}

int valid_object(struct localroot *ptr, addr pos)
{
	return valid_header(pos) && valid_memory(ptr, (const void *)pos);
}


/*
 *  allocate
 */
static void allocobject(struct localroot *local,
		size_t size, enum LISPTYPE type, addr *root, int size2)
{
	enum LISPCLASS index;
	addr pos;

	/* memory */
	size_and_class(size, &index, &size);
	pos = alloc_local(local, size);

	/* body */
	SetType(pos, (byte)type);
	if (size2)
		*PtrValue2L(pos) = (byte16)size;
	else
		*PtrValueL(pos) = size;
	*root = pos;
}

addr localr_cons(struct localroot *local)
{
	addr pos;
	local_array2_memory(local, &pos, LISPTYPE_CONS, 2);
	SetCheckValue(pos, LISPCHECK_LIST, 1);
	return pos;
}
void local_cons(struct localroot *local, addr *root)
{
	*root = localr_cons(local);
}

addr localr_symbol(struct localroot *local)
{
	addr pos;
	local_array2_memory(local, &pos, LISPTYPE_SYMBOL, SYMBOL_INDEX_SIZE);
	SetCheckValue(pos, LISPCHECK_SYMBOL, 1);
	SetCheckValue(pos, LISPCHECK_LIST, 1);
	return pos;
}
void local_symbol(struct localroot *local, addr *root)
{
	*root = localr_symbol(local);
}

addr localr_array2_memory(struct localroot *local, enum LISPTYPE type, byte16 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA2(array);
	Check(0xFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 1);
	SetStatusSize(pos, LISPSIZE_ARRAY2, LISPSTATUS_DYNAMIC);
	SetCheck2(pos, LISPCHECK_SIZE2, LISPCHECK_ARRAY);
	*PtrLenArrayA2(pos) = array;
	nilarray2(pos, array);

	return pos;
}
void local_array2_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte16 array)
{
	*root = localr_array2_memory(local, type, array);
}

addr localr_array4_memory(struct localroot *local, enum LISPTYPE type, byte32 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA4(array);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAY4, LISPSTATUS_DYNAMIC);
	SetCheck2(pos, LISPCHECK_SIZE4, LISPCHECK_ARRAY);
	*PtrLenArrayA4(pos) = array;
	nilarray4(pos, array);

	return pos;
}
void local_array4_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte32 array)
{
	*root = localr_array4_memory(local, type, array);
}

#ifdef LISP_ARCH_64BIT
addr localr_array8(struct localroot *local, enum LISPTYPE type, size_t array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA8(array);
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAY8, LISPSTATUS_DYNAMIC);
	SetCheck2(pos, LISPCHECK_SIZE8, LISPCHECK_ARRAY);
	*PtrLenArrayA8(pos) = array;
	nilarray8(pos, array);

	return pos;
}
void local_array8(struct localroot *local,
		addr *root, enum LISPTYPE type, size_t array)
{
	*root = localr_array8(local, type, array);
}
#endif

addr localr_body2_memory(struct localroot *local, enum LISPTYPE type, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB2(body);
	Check(0xFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 1);
	SetStatusSize(pos, LISPSIZE_BODY2, LISPSTATUS_DYNAMIC);
	SetCheck2(pos, LISPCHECK_SIZE2, LISPCHECK_BODY);
	*PtrLenBodyB2(pos) = body;

	return pos;
}
void local_body2_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte16 body)
{
	*root = localr_body2_memory(local, type, body);
}

addr localr_body4_memory(struct localroot *local, enum LISPTYPE type, byte32 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB4(body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_BODY4, LISPSTATUS_DYNAMIC);
	SetCheck2(pos, LISPCHECK_SIZE4, LISPCHECK_BODY);
	*PtrLenBodyB4(pos) = body;

	return pos;
}
void local_body4_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte32 body)
{
	*root = localr_body4_memory(local, type, body);
}

#ifdef LISP_ARCH_64BIT
addr localr_body8(struct localroot *local, enum LISPTYPE type, size_t body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB8(body);
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_BODY8, LISPSTATUS_DYNAMIC);
	SetCheck2(pos, LISPCHECK_SIZE8, LISPCHECK_BODY);
	*PtrLenBodyB8(pos) = body;

	return pos;
}
void local_body8(struct localroot *local, addr *root, enum LISPTYPE type, size_t body)
{
	*root = localr_body8(local, type, body);
}
#endif

addr localr_smallsize_memory(struct localroot *local,
		enum LISPTYPE type, byte array, byte body)
{
	addr pos;
	size_t size;

	size = MemoryLengthSS(array, body);
	Check(0xFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 1);
	SetStatusSize(pos, LISPSIZE_SMALLSIZE, LISPSTATUS_DYNAMIC);
	SetCheck4(pos, LISPCHECK_SIZE2,
			LISPCHECK_ARRAY, LISPCHECK_BODY, LISPCHECK_ARRAYBODY);
	nilarray2(pos, array);
	*PtrLenArraySS(pos) = array;
	*PtrLenBodySS(pos) = body;

	return pos;
}
void local_smallsize_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte array, byte body)
{
	*root = localr_smallsize_memory(local, type, array, body);
}

addr localr_arraybody_memory(struct localroot *local,
		enum LISPTYPE type, byte16 array, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthAB(array, body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAYBODY, LISPSTATUS_DYNAMIC);
	SetCheck4(pos, LISPCHECK_SIZE4,
			LISPCHECK_ARRAY, LISPCHECK_BODY, LISPCHECK_ARRAYBODY);
	nilarray4(pos, array);
	*PtrLenArrayAB(pos) = array;
	*PtrLenBodyAB(pos) = body;

	return pos;
}
void local_arraybody_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte16 array, byte16 body)
{
	*root = localr_arraybody_memory(local, type, array, body);
}

addr localr_array4_unbound_memory(struct localroot *local,
		enum LISPTYPE type, byte32 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA4(array);
	Check(0xFFFFFFFFUL < size, "size error");
	allocobject(local, size, type, &pos, 0);
	SetStatusSize(pos, LISPSIZE_ARRAY4, LISPSTATUS_DYNAMIC);
	*PtrLenArrayA4(pos) = array;
	unboundarray4(pos, array);

	return pos;
}
void local_array4_unbound_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte32 array)
{
	*root = localr_array4_unbound_memory(local, type, array);
}

addr localr_array(struct localroot *local, enum LISPTYPE type, size_t array)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		return localr_array2_memory(local, type, (byte16)array);
	else if (MemoryLengthA4(array) <= 0xFFFFFFFFUL)
		return localr_array4_memory(local, type, (byte32)array);
	else
		return localr_array8(local, type, array);
#else
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		return localr_array2_memory(local, type, (byte16)array);
	else
		return localr_array4_memory(local, type, array);
#endif
}
void local_array(struct localroot *local, addr *root, enum LISPTYPE type, size_t array)
{
	*root = localr_array(local, type, array);
}

addr localr_body(struct localroot *local, enum LISPTYPE type, size_t body)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		return localr_body2_memory(local, type, (byte16)body);
	else if (MemoryLengthB4(body) <= 0xFFFFFFFFUL)
		return localr_body4_memory(local, type, (byte32)body);
	else
		return localr_body8(local, type, body);
#else
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		return localr_body2_memory(local, type, (byte16)body);
	else
		return localr_body4_memory(local, type, body);
#endif
}
void local_body(struct localroot *local, addr *root, enum LISPTYPE type, size_t body)
{
	*root = localr_body(local, type, body);
}

#ifdef LISP_DEBUG
addr localr_array2_debug(struct localroot *local, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	return localr_array2_memory(local, type, (byte16)array);
}
addr localr_array4_debug(struct localroot *local, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	return localr_array4_memory(local, type, (byte32)array);
}
addr localr_body2_debug(struct localroot *local, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	return localr_body2_memory(local, type, (byte16)body);
}
addr localr_body4_debug(struct localroot *local, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	return localr_body4_memory(local, type, (byte32)body);
}
addr localr_smallsize_debug(struct localroot *local,
		enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	return localr_smallsize_memory(local, type, (byte)array, (byte)body);
}
addr localr_arraybody_debug(struct localroot *local,
		enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	return localr_arraybody_memory(local, type, (byte16)array, (byte16)body);
}
addr localr_array4_unbound_debug(struct localroot *local,
		enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	return localr_array4_unbound_memory(local, type, (byte32)array);
}

void local_array2_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	local_array2_memory(local, ret, type, (byte16)array);
}
void local_array4_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	local_array4_memory(local, ret, type, (byte32)array);
}
void local_body2_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	local_body2_memory(local, ret, type, (byte16)body);
}
void local_body4_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	local_body4_memory(local, ret, type, (byte32)body);
}
void local_smallsize_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	local_smallsize_memory(local, ret, type, (byte)array, (byte)body);
}
void local_arraybody_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	local_arraybody_memory(local, ret, type, (byte16)array, (byte16)body);
}
void local_array4_unbound_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	local_array4_unbound_memory(local, ret, type, (byte32)array);
}
#endif

