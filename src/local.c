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
static void memoryerror_local(void)
{
	Debug("local memory overflow.");
	exitthis(LISPCODE_MEMORY);
}


#ifdef LISP_MEMORY_MALLOC
/***********************************************************************
 *  Use malloc
 ***********************************************************************/
static void *lowlevel_unsafe(struct localroot *local, size_t size)
{
	struct localmemory *mem;
	void *ptr;

	/* localmemory */
	mem = local->mem;
	if (LocalCount <= mem->count) {
		mem = malloctype(struct localmemory);
		if (mem == NULL)
			return NULL;
#ifdef LISP_DEBUG
		aamemory(mem, sizeoft(struct localmemory));
#endif
		mem->count = 0;
		mem->next = local->mem;
		local->mem = mem;
	}

	/* memory */
	ptr = malloc(size);
	if (ptr == NULL)
		return NULL;
#ifdef LISP_MEMORY_INIT
	aamemory(ptr, size);
#endif
	mem->point[mem->count++] = ptr;
	return ptr;
}

_g void *lowlevel_local(struct localroot *local, size_t size)
{
	void *ptr;

	ptr = lowlevel_unsafe(local, size);
	if (ptr == NULL)
		memoryerror_local();

	return ptr;
}

static struct localcell *pushcell_local(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_local(local, sizeoft(struct localcell));
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;

	return cell;
}

_g addr alloc_local(struct localroot *local, size_t size)
{
	struct localcell *cell;
	addr pos;

	cell = local->cell;
	if (LocalCount <= cell->count)
		cell = pushcell_local(local);
	pos = (addr)lowlevel_local(local, size);
	cell->point[cell->count++] = pos;

	return pos;
}

static struct localmemory *make_local_memory(size_t size)
{
	struct localmemory *mem;

	mem = malloctype(struct localmemory);
	if (mem == NULL) {
		Debug("malloctype error");
		return NULL;
	}
#ifdef LISP_MEMORY_INIT
	aamemory(mem, sizeoft(struct localmemory));
#endif
	mem->count = 0;
	mem->next = NULL;

	return mem;
}

static struct localroot *make_local_localroot(struct localmemory *mem, size_t size)
{
	struct localroot *local;

	local = malloctype(struct localroot);
	if (local == NULL) {
		Debug("malloctype error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(local, sizeoft(struct localroot));
#endif
	local->size = size;
	local->mem = mem;
	local->cell = NULL;
	local->stack = NULL;

	return local;
}

static struct localcell *make_local_localcell(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_unsafe(local, sizeoft(struct localcell));
	if (cell == NULL) {
		Debug("lowlevel_unsafe error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;

	return cell;
}

_g struct localroot *make_local(size_t size)
{
	struct localmemory *mem;
	struct localcell *cell;
	struct localroot *local;

	mem = NULL;
	cell = NULL;
	local = NULL;
	if (size < LocalLimit)
		size = LocalLimit;

	/* memory */
	mem = make_local_memory(size);
	if (mem == NULL)
		goto error;

	/* localroot */
	local = make_local_localroot(mem, size);
	if (local == NULL)
		goto error;

	/* localcell */
	cell = make_local_localcell(local);
	if (cell == NULL) {
		free_local(local);
		return NULL;
	}

	/* result */
	return local;

error:
	free(mem);
	free(local);
	return NULL;
}

_g void free_local(struct localroot *local)
{
	void **mem_point;
	struct localmemory *x, *next;
	size_t i, count;

	if (local == NULL)
		return;

	/* localmemory */
	for (x = local->mem; x; x = next) {
		next = x->next;
		count = x->count;
		mem_point = x->point;
		for (i = 0; i < count; i++) {
			free(mem_point[i]);
			mem_point[i] = NULL;
		}
		free(x);
	}

	/* localroot */
	free(local);
}

_g void push_local(struct localroot *local, struct localstack **ret)
{
	struct localstack *stack;

	stack = (struct localstack *)lowlevel_local(local, sizeof(struct localstack));
	stack->stack = local->stack;
	stack->mem = local->mem;
	stack->cell = local->cell;
	stack->memcount = local->mem->count;
	stack->cellcount = local->cell->count;
	local->stack = stack;
	*ret = stack;
}

static void rollback_memory_local(struct localroot *local, struct localstack *stack)
{
	void **mem_point;
	struct localmemory *x, *y, *next;
	size_t i, count;

	y = stack->mem;
	for (x = local->mem; x != y; x = next) {
		next = x->next;
		count = x->count;
		mem_point = x->point;
		for (i = 0; i < count; i++) {
			free(mem_point[i]);
			mem_point[i] = NULL;
		}
		free(x);
	}
}

_g void rollback_local(struct localroot *local, struct localstack *stack)
{
	void **mem_point;
	struct localmemory *local_mem;
	struct localstack save;
	size_t i, count;
#ifdef LISP_DEBUG
	struct localstack *root;

	Check(stack == NULL, "stack error.");
	for (root = local->stack; root == stack; root = root->stack) {
		Check(root == NULL, "rollback_local check error");
	}
#endif
	save = *stack;
	rollback_memory_local(local, stack);
	local->stack = save.stack;
	local->mem = save.mem;
	local->cell = save.cell;

	local_mem = local->mem;
	count = local_mem->count;
	mem_point = local_mem->point;
	for (i = save.memcount; i < count; i++)
		free(mem_point[i]);
	local_mem->count = save.memcount;
	local->cell->count = save.cellcount;
#ifdef LISP_DEBUG
	for (i = local->cell->count; i < LocalCount; i++)
		local->cell->point[i] = Unbound;
	for (i = local->mem->count; i < LocalCount; i++)
		local->mem->point[i] = Unbound;
#endif
}

#else
/***********************************************************************
 *  Memory pool
 ***********************************************************************/
static void *lowlevel_unsafe(struct localroot *local, size_t size)
{
	addr front, check;

	AlignSize8Front(size, &size);
	front = local->front;
	check = front + size;
	if (local->tail < check) {
		Debug("stack overflow");
		return NULL;
	}
	local->front = check;
	CheckAlign8(front, "align8 error1");
	CheckAlign8(check, "align8 error2");

	return front;
}

_g void *lowlevel_local(struct localroot *local, size_t size)
{
	void *ptr;

	ptr = lowlevel_unsafe(local, size);
	if (ptr == NULL)
		memoryerror_local();

	return ptr;
}

static struct localcell *pushcell_local(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_local(local, sizeoft(struct localcell));
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;

	return cell;
}

_g addr alloc_local(struct localroot *local, size_t size)
{
	struct localcell *cell;
	addr pos;

	cell = local->cell;
	if (LocalCount <= cell->count)
		cell = pushcell_local(local);
	pos = (addr)lowlevel_local(local, size);
	cell->point[cell->count++] = pos;

	return pos;
}

static void *make_local_memory(size_t size)
{
	void *ptr;

	ptr = malloc(size + 8UL);
	if (ptr == NULL) {
		Debug("malloc error");
		return NULL;
	}
#ifdef LISP_MEMORY_INIT
	aamemory(ptr, size);
#endif

	return ptr;
}

static struct localroot *make_local_localroot(void *ptr, size_t size)
{
	struct localroot *local;

	local = malloctype(struct localroot);
	if (local == NULL) {
		Debug("malloctype error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(local, sizeoft(struct localroot));
#endif
	local->alloc = ptr;
	Align8Front(ptr, &(local->front));
	local->tail = size + ((addr)ptr);
	local->size = size;
	local->stack = NULL;
	local->cell = NULL;
	CheckAlign8(local->front, "align8 error1");

	return local;
}

static struct localcell *make_local_localcell(struct localroot *local)
{
	struct localcell *cell;

	cell = (struct localcell *)lowlevel_unsafe(local, sizeoft(struct localcell));
	if (cell == NULL) {
		Debug("lowlevel_unsafe error");
		return NULL;
	}
#ifdef LISP_DEBUG
	aamemory(cell, sizeoft(struct localcell));
#endif
	cell->count = 0;
	cell->next = local->cell;
	local->cell = cell;
	CheckAlign8(local->front, "align8 error2");

	return cell;
}

_g struct localroot *make_local(size_t size)
{
	struct localroot *local;
	struct localcell *cell;
	void *ptr;

	if (size < LocalLimit)
		size = LocalLimit;

	/* memory */
	local = NULL;
	cell = NULL;
	ptr = make_local_memory(size);
	if (ptr == NULL)
		goto error;

	/* localroot */
	local = make_local_localroot(ptr, size);
	if (local == NULL)
		goto error;

	/* localcell */
	cell = make_local_localcell(local);
	if (cell == NULL)
		goto error;

	/* result */
	return local;

error:
	free(ptr);
	free(local);
	return NULL;
}

_g void free_local(struct localroot *local)
{
	if (local == NULL)
		return;
	free(local->alloc);
	local->alloc = NULL;
	free(local);
}

_g void push_local(struct localroot *local, struct localstack **ret)
{
	struct localstack *stack;

	stack = (struct localstack *)lowlevel_local(local, sizeof(struct localstack));
	stack->stack = local->stack;
	stack->cell = local->cell;
	stack->cellcount = local->cell->count;
	local->stack = stack;
	*ret = stack;
}

_g void rollback_local(struct localroot *local, struct localstack *stack)
{
#ifdef LISP_DEBUG
	struct localstack *root;
	addr front;
	size_t i;

	front = local->front;
	Check(stack == NULL, "stack error.");
	for (root = local->stack; root == stack; root = root->stack) {
		Check(root == NULL, "rollback_local check error");
	}
#endif
	local->front = (addr)stack;
	local->stack = stack->stack;
	local->cell = stack->cell;
	local->cell->count = stack->cellcount;
#ifdef LISP_DEBUG
	memset(local->front, 0xAA, front - local->front);
	for (i = local->cell->count; i < LocalCount; i++)
		local->cell->point[i] = Unbound;
#endif
}
#endif


/***********************************************************************
 *  Allocate
 ***********************************************************************/
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

_g addr localr_cons(struct localroot *local)
{
	addr pos;
	local_array2_memory(local, &pos, LISPTYPE_CONS, 2);
	SetCheckValue(pos, LISPCHECK_LIST, 1);
	return pos;
}
_g void local_cons(struct localroot *local, addr *root)
{
	*root = localr_cons(local);
}

_g addr localr_symbol(struct localroot *local)
{
	addr pos;
	local_array2_memory(local, &pos, LISPTYPE_SYMBOL, SYMBOL_INDEX_SIZE);
	SetCheckValue(pos, LISPCHECK_SYMBOL, 1);
	SetCheckValue(pos, LISPCHECK_LIST, 1);
	return pos;
}
_g void local_symbol(struct localroot *local, addr *root)
{
	*root = localr_symbol(local);
}

_g addr localr_array2_memory(struct localroot *local, enum LISPTYPE type, byte16 array)
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
_g void local_array2_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte16 array)
{
	*root = localr_array2_memory(local, type, array);
}

_g addr localr_array4_memory(struct localroot *local, enum LISPTYPE type, byte32 array)
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
_g void local_array4_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte32 array)
{
	*root = localr_array4_memory(local, type, array);
}

#ifdef LISP_ARCH_64BIT
_g addr localr_array8(struct localroot *local, enum LISPTYPE type, size_t array)
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
_g void local_array8(struct localroot *local,
		addr *root, enum LISPTYPE type, size_t array)
{
	*root = localr_array8(local, type, array);
}
#endif

_g addr localr_body2_memory(struct localroot *local, enum LISPTYPE type, byte16 body)
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
_g void local_body2_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte16 body)
{
	*root = localr_body2_memory(local, type, body);
}

_g addr localr_body4_memory(struct localroot *local, enum LISPTYPE type, byte32 body)
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
_g void local_body4_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte32 body)
{
	*root = localr_body4_memory(local, type, body);
}

#ifdef LISP_ARCH_64BIT
_g addr localr_body8(struct localroot *local, enum LISPTYPE type, size_t body)
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
_g void local_body8(struct localroot *local,
		addr *root, enum LISPTYPE type, size_t body)
{
	*root = localr_body8(local, type, body);
}
#endif

_g addr localr_smallsize_memory(struct localroot *local,
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
_g void local_smallsize_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte array, byte body)
{
	*root = localr_smallsize_memory(local, type, array, body);
}

_g addr localr_arraybody_memory(struct localroot *local,
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
_g void local_arraybody_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte16 array, byte16 body)
{
	*root = localr_arraybody_memory(local, type, array, body);
}

_g addr localr_array4_unbound_memory(struct localroot *local,
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
_g void local_array4_unbound_memory(struct localroot *local,
		addr *root, enum LISPTYPE type, byte32 array)
{
	*root = localr_array4_unbound_memory(local, type, array);
}

_g addr localr_array(struct localroot *local, enum LISPTYPE type, size_t array)
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
_g void local_array(struct localroot *local,
		addr *root, enum LISPTYPE type, size_t array)
{
	*root = localr_array(local, type, array);
}

_g addr localr_body(struct localroot *local, enum LISPTYPE type, size_t body)
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
_g void local_body(struct localroot *local,
		addr *root, enum LISPTYPE type, size_t body)
{
	*root = localr_body(local, type, body);
}

#ifdef LISP_DEBUG
_g addr localr_array2_debug(struct localroot *local, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	return localr_array2_memory(local, type, (byte16)array);
}
_g addr localr_array4_debug(struct localroot *local, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	return localr_array4_memory(local, type, (byte32)array);
}
_g addr localr_body2_debug(struct localroot *local, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	return localr_body2_memory(local, type, (byte16)body);
}
_g addr localr_body4_debug(struct localroot *local, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	return localr_body4_memory(local, type, (byte32)body);
}
_g addr localr_smallsize_debug(struct localroot *local,
		enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	return localr_smallsize_memory(local, type, (byte)array, (byte)body);
}
_g addr localr_arraybody_debug(struct localroot *local,
		enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	return localr_arraybody_memory(local, type, (byte16)array, (byte16)body);
}
_g addr localr_array4_unbound_debug(struct localroot *local,
		enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	return localr_array4_unbound_memory(local, type, (byte32)array);
}

_g void local_array2_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	local_array2_memory(local, ret, type, (byte16)array);
}
_g void local_array4_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	local_array4_memory(local, ret, type, (byte32)array);
}
_g void local_body2_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	local_body2_memory(local, ret, type, (byte16)body);
}
_g void local_body4_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	local_body4_memory(local, ret, type, (byte32)body);
}
_g void local_smallsize_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	local_smallsize_memory(local, ret, type, (byte)array, (byte)body);
}
_g void local_arraybody_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	local_arraybody_memory(local, ret, type, (byte16)array, (byte16)body);
}
_g void local_array4_unbound_debug(struct localroot *local,
		addr *ret, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	local_array4_unbound_memory(local, ret, type, (byte32)array);
}
#endif

