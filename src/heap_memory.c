#include <string.h>
#include "execute.h"
#include "heap_memory.h"
#include "memory.h"

/* static varibales */
static size_t            Size = 0;
static addr              FrontMax = 0;

static size_t            GcCounter = 0;
static addr              GcCheck1 = 0;
static addr              GcCheck2 = 0;
static addr              GcCheck3 = 0;
static addr              GcCheck4 = 0;


/*
 *  memory overflow
 */
static void memoryerror_heap(void)
{
	Debug("heap memory overflow.");
	abort_execute();
}


/*
 *  allocate front
 */
static int allocfront_size(addr pos, size_t *ret)
{
	enum LISPTYPE type;
	addr now;
	size_t size, value;

	size = 0;
	now = pos;
	for (;;) {
		if (heap_front <= now) {
			*ret = 0;
			return 1;
		}
		type = (enum LISPTYPE)GetType(now);
		if (type == LISPSYSTEM_SPACE1) {
			GetSizeSpace1(now, &value);
		}
		else if (type == LISPSYSTEM_SPACE) {
			GetSizeSpace(now, &value);
		}
		else {
			break;
		}
		size += value;
		now += value;
	}

	if (size == 0) {
		*ret = getobjectlength(pos);
		return 0;
	}
	else {
		*ret = size;
		return 1;
	}
}

static addr allocfront_search(addr pos, size_t *ret)
{
	size_t size;

	for (;;) {
		/* object */
		if (allocfront_size(pos, &size) == 0) {
			pos += size;
			continue;
		}
		/* expand */
		if (size == 0) {
			heap_front = pos;
			return NULL;
		}
		/* space */
		break;
	}
	*ret = size;
	return pos;
}

void makespace_heap(addr pos, size_t size)
{
	Check(size < 2, "size error");
	if (size < (8UL + IdxSize)) {
#ifdef LISP_DEBUG_MEMORY
		memset(pos, 0xAA, size);
#endif
		SetType(pos, LISPSYSTEM_SPACE1);
		SetSizeSpace1(pos, size);
	}
	else {
#ifdef LISP_DEBUG_MEMORY
		memset(pos, 0xAA, size);
#endif
		SetType(pos, LISPSYSTEM_SPACE);
		SetSizeSpace(pos, size);
	}
}

static void writereserved_heap(addr pos, size_t size, size_t check)
{
	/*
	 * |---------------| check
	 * |--------|        size
	 * [xxxxxxxx][space]
	 */
	Check(check < size, "writereserver error");
	heap_pos = pos + size;
	check -= size;
	if (check)
		makespace_heap(heap_pos, check);
}

static addr allocfront_expand(size_t size)
{
	addr check, result;

	check = heap_front + size;
	if (heap_tail < check) {
		Debug("allocfront_expand error");
		return NULL;
	}
	result = heap_front;
	heap_pos = heap_front = check;
	if (FrontMax < heap_front)
		FrontMax = heap_front;

	return result;
}

static addr allocfront_object(size_t size)
{
	addr pos;
	size_t check;

	CheckAlignSize8(size, "alignsize8 error");
	pos = heap_pos;
	for (;;) {
		pos = allocfront_search(pos, &check);
		if (pos == NULL) {
			pos = allocfront_expand(size);
			if (pos == NULL)
				return NULL;
			break;
		}
		if (size <= check) {
			writereserved_heap(pos, size, check);
			break;
		}
		pos += check;
	}
#ifdef LISP_DEBUG
	memset(pos, 0xAA, size);
#endif

	return pos;
}

/*
 *  gccheck
 *   0xF0	93%
 *  *0xE0	87%		5bit	32tims
 *   0xD0	81%
 *  *0xC0	75%		6bit	64tims
 *   0xB0	68%
 *  *0xA0	62%		7bit	128tims
 *   0x90	56%
 *  *0x80	50%		8bit	256tims
 */
static void gccheck_execute_heap(void)
{
	GcCounter = 0;
	gcstate_execute(GcMode_Default);
}

static void gccheck_heap(void)
{
	if (lisp_gcsync != GcMode_Off)
		return;
	GcCounter++;

	/* GcCheck1 */
	if (heap_pos < GcCheck1)
		return;
	if (GcCounter & (1UL << 8UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}

	/* GcCheck2 */
	if (heap_pos < GcCheck2)
		return;
	if (GcCounter & (1UL << 7UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}

	/* GcCheck3 */
	if (heap_pos < GcCheck3)
		return;
	if (GcCounter & (1UL << 6UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}

	/* GcCheck4 */
	if (heap_pos < GcCheck4)
		return;
	if (GcCounter & (1UL << 5UL)) {
		gccheck_execute_heap();
		if (lisp_gcsync != GcMode_Off)
			return;
	}
}

static addr allocfront(size_t size)
{
	addr ret;

	ret = allocfront_object(size);
	gccheck_heap();
	if (ret == NULL) {
		memoryerror_heap();
		return NULL;
	}

	return ret;
}


/*
 *  allocate tail
 */
#define alloctail_size		(sizeoft(struct heap_addr))
struct heap_addr *alloctail(void)
{
	addr check;

	check = heap_tail - alloctail_size;
	if (check < heap_front) {
		memoryerror_heap();
		return NULL;
	}
	heap_tail = check;
#ifdef LISP_DEBUG
	memset(heap_tail, 0xAA, alloctail_size);
#endif

	return (struct heap_addr *)heap_tail;
}


/*
 *  allocate
 */
static void allocheap_object(size_t size, addr *ret)
{
	addr pos;
	struct heap_addr *info;

	/* front */
	pos = allocfront(size);
	/* tail */
	info = alloctail();
	info->pos = pos;
	/* result */
	*ret = pos;
}

void allocheap(size_t size, enum LISPTYPE type, addr *root, int size2)
{
	addr pos;

	/* alloc */
	AlignSize8Front(size, &size);
	allocheap_object(size, &pos);
	heap_object += size;
	heap_count++;

	/* initialize */
	SetType(pos, (byte)type);
	SetChain(pos, 0);
	if (size2)
		*PtrValue2L(pos) = (byte16)size;
	else
		*PtrValueL(pos) = size;
	*root = pos;
}


/*
 *  init heap
 */
static void tailheap(void)
{
	heap_tail = (addr)Align8Cut(Size + (uintptr_t)heap_root);
	heap_range = heap_tail;
}

static void frontheap(void *ptr, size_t size)
{
	int align;
	size_t q;

	/* memory */
	align = Align8Space(ptr);
	Size = size - align;
	heap_pos = heap_front = FrontMax = heap_root = align + (addr)ptr;
	heap_object = 0;
	heap_count = 0;
	heap_gc_count = 0;
	CheckAlign8(heap_pos, "align8 error");

	/* gccheck */
	q = Size / 0x10;
	GcCheck1 = heap_root + (q * 0x08);
	GcCheck2 = heap_root + (q * 0x0A);
	GcCheck3 = heap_root + (q * 0x0C);
	GcCheck4 = heap_root + (q * 0x0E);
	GcCounter = 0;
}

int alloc_heap(size_t size)
{
	void *ptr;

	if (heap_alloc) {
		Debug("heap memory already allocated.");
		return 1;
	}
	if (size < 1000UL * 1000UL) {
		Debug("heap size must be greater than 1MByte.");
		return 1;
	}

	ptr = malloc(size);
	if (ptr == NULL) {
		Debug("malloc error");
		return 1;
	}
#ifdef LISP_MEMORY_INIT
	memset(ptr, 0xAA, size);
#endif

	/* make front */
	frontheap(ptr, size);

	/* make tail */
	tailheap();

	/* result */
	heap_alloc = ptr;
	return 0;
}

void free_heap(void)
{
	if (heap_alloc) {
		free(heap_alloc);
		heap_root = 0;
		heap_front = 0;
		heap_pos = 0;
		heap_tail = 0;
		heap_range = 0;
		heap_object = 0;
		heap_count = 0;
		heap_gc_count = 0;
		heap_gc_partial = 0;
		heap_gc_full = 0;
		heap_cons_count = 0;
		heap_symbol_count = 0;
		Size = 0;
		FrontMax = 0;
		GcCounter = 0;
		GcCheck1 = 0;
		GcCheck2 = 0;
		GcCheck3 = 0;
		GcCheck4 = 0;
		heap_alloc = 0;
	}
}


/*
 *  gc
 */
int valid_heap(const void *pos)
{
	return (heap_root <= (addr)pos) && ((addr)pos <= heap_front);
}

size_t get_heap_object(void)
{
	return heap_object;
}

size_t get_heap_count(void)
{
	return heap_count;
}

size_t get_heap_gc_count(void)
{
	return heap_gc_count;
}

size_t get_heap_size(void)
{
	return Size;
}

