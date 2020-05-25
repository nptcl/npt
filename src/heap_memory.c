#include <string.h>
#include "execute.h"
#include "heap_memory.h"
#include "memory.h"

/* static varibales */
_g mutexlite         heap_cons_mutex;
_g mutexlite         heap_symbol_mutex;
_g int               heap_cons_count = 0;
_g int               heap_symbol_count = 0;

static mutexlite         Mutex;
static size_t            Size = 0;
static addr              FrontMax = 0;
static addr              Tail = 0;

static size_t            GcCounter = 0;
static addr              GcCheck1 = 0;
static addr              GcCheck2 = 0;
static addr              GcCheck3 = 0;
static addr              GcCheck4 = 0;

_g struct heapinfo  *heap_info = 0;
static struct heapcell  *CellPos = 0;
static struct heapcell  *CellRoot = 0;

#ifdef LISP_DEGRADE
_g struct heapinfo **Degrade_heap_Info(void) { return &heap_info; }
#endif


/*
 *  memory overflow
 */
static void memoryerror_heap(void)
{
	Debug("heap memory overflow.");
	exitthis(LISPCODE_MEMORY);
}


/*
 *  allocate front
 */
static int length_space(addr pos, size_t *size)
{
	enum LISPTYPE type;
	addr now;
	size_t len, value;

	len = 0;
	now = pos;
	while (now < heap_front) {
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
		len += value;
		now += value;
	}

	if (len == 0) {
		return 0;
	}
	else {
		*size = len;
		return 1;
	}
}

static int check_spacememory(addr pos, size_t *size)
{
	if (length_space(pos, size)) {
		return 1;
	}
	if (GetType(pos) == LISPSYSTEM_RESERVED) {
		GetSizeReserved(pos, size);
	}
	else {
		*size = getobjectlength(pos);
	}

	return 0;
}

static addr searchmemory(addr pos, size_t *size)
{
	size_t check;

	for (;;) {
		if (heap_front <= pos) return NULL;
		if (check_spacememory(pos, &check)) break;
		pos += check;
	}
	*size = check;

	return pos;
}

_g void makespace(addr pos, size_t size)
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

_g void makereserved(addr pos, size_t size)
{
	Check(size < (8UL + IdxSize), "size error");
	SetType(pos, LISPSYSTEM_RESERVED);
	SetSizeReserved(pos, size);
}

static void writereserved(addr pos, size_t size, size_t check)
{
	/*
	 * |---------------| check
	 * |--------|        size
	 * [reserved][space]
	 */
	Check(check < size, "writereserver error");
	Check(size < (8UL + IdxSize), "size error");
	makereserved(pos, size);
	heap_pos = pos + size;
	check -= size;
	if (check)
		makespace(heap_pos, check);
}

static addr expandmemory(size_t size)
{
	addr check, result;

	check = heap_front + size;
	if (Tail < check) {
		Debug("expandmemory error");
		return NULL;
	}
	result = heap_front;
	heap_pos = heap_front = check;
	if (FrontMax < heap_front)
		FrontMax = heap_front;

	return result;
}

static addr allocfront_unlock(size_t size)
{
	addr pos;
	size_t check;

	CheckAlignSize8(size, "alignsize8 error");
	pos = heap_pos;
	for (;;) {
		pos = searchmemory(pos, &check);
		if (pos == NULL) {
			pos = expandmemory(size);
			if (pos == NULL) return NULL;
			break;
		}
		if (size <= check) {
			writereserved(pos, size, check);
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
	GcCounter++;
	if (GcCheck4 < heap_pos) {
		if (GcCounter & (1UL << 5UL))
			gccheck_execute_heap();
		return;
	}
	if (GcCheck3 < heap_pos) {
		if (GcCounter & (1UL << 6UL))
			gccheck_execute_heap();
		return;
	}
	if (GcCheck2 < heap_pos) {
		if (GcCounter & (1UL << 7UL))
			gccheck_execute_heap();
		return;
	}
	if (GcCheck1 < heap_pos) {
		if (GcCounter & (1UL << 8UL))
			gccheck_execute_heap();
		return;
	}
}

static addr alloclock(size_t size, addr (*call)(size_t))
{
	addr result;

	lock_mutexlite(&Mutex);
	result = call(size);
	gccheck_heap();
	unlock_mutexlite(&Mutex);
	if (result == NULL) memoryerror_heap();

	return result;
}

static addr allocfront(size_t size)
{
	return alloclock(size, allocfront_unlock);
}


/*
 *  allocate tail
 */
static addr alloctail_unlock(size_t size)
{
	addr check;

	AlignSize8Front(size, &size);
	check = Tail - size;
	if (check < heap_front) {
		Debug("alloctail_unlock error");
		return NULL;
	}
	Tail = check;
#ifdef LISP_DEBUG
	memset(Tail, 0xAA, size);
#endif
	CheckAlign8(Tail, "align8 error");

	return Tail;
}

static addr alloctail(size_t size)
{
	return alloclock(size, alloctail_unlock);
}


/*
 *  allocate
 */
static void fillheapmemory(struct heapcell *cell, size_t size)
{
	int i;
	addr pos, *array;

	AlignSize8Front(size, &size);
	lock_mutexlite(&Mutex);
	array = cell->point;
	for (i = 0; i < HeapCount; i++) {
		pos = allocfront_unlock(size);
		if (pos == NULL) {
			unlock_mutexlite(&Mutex);
			Debug("allocfront_unlock error");
			memoryerror_heap();
			return;
		}
		SetType(pos, LISPSYSTEM_RESERVED);
		SetSizeReserved(pos, size);
		array[i] = pos;
	}
	unlock_mutexlite(&Mutex);
}

static void fillcellunbound(struct heapcell *cell)
{
	int i;
	addr *array;

	array = cell->point;
	for (i = 0; i < HeapCount; i++) {
		array[i] = Unbound;
	}
}

#define MakeCell(make) { \
	(make) = (struct heapcell *)alloctail(sizeoft(struct heapcell)); \
	(make)->next = (make)->chain = NULL; \
	(make)->search = 0; \
	(make)->count = 0; \
}
static struct heapcell *cellalloc(void)
{
	struct heapcell *make, *chain;

	/* first make */
	if (CellRoot == NULL) {
		MakeCell(make);
		CellRoot = CellPos = make;
		return make;
	}

	/* search */
	for (;;) {
		if (CellPos->count == 0) {
			CellPos->next = NULL;
			return CellPos;
		}
		chain = CellPos->chain;
		if (chain == NULL) break;
		CellPos = chain;
	}

	/* make chain */
	MakeCell(make);
	CellPos = CellPos->chain = make;

	return make;
}

_g struct heapcell *cellexpand(struct heapinfo *root, struct heapcell *cell)
{
	struct heapcell *make;

	make = cellalloc();
	if (cell == NULL)
		root->root = root->front = make;
	else
		root->front = root->front->next = make;

	return make;
}

_g void allocheap_small(size_t size, int index, addr *ret)
{
	addr pos;
	struct heapinfo *root;
	struct heapcell *cell;

	root = &heap_info[index];
	cell = root->front;
	if (cell == NULL || HeapCount <= cell->count) {
		cell = cellexpand(root, cell);
		fillheapmemory(cell, size);
	}

	pos = cell->point[cell->count++];
	Check(GetType(pos) != LISPSYSTEM_RESERVED, "type error");
	*ret = pos;

	/* statistics */
	heap_object += size;
	heap_count++;
}

_g void allocheap_large(size_t size, int index, addr *ret)
{
	addr pos;
	struct heapinfo *root;
	struct heapcell *cell;

	root = &heap_info[index];
	cell = root->front;
	if (cell == NULL || HeapCount <= cell->count) {
		cell = cellexpand(root, cell);
		fillcellunbound(cell);
	}

	Check(cell->point[cell->count] != Unbound, "type error");
	pos = allocfront(size);
	cell->point[cell->count++] = pos;
	*ret = pos;

	/* statistics */
	heap_object += size;
	heap_count++;
}

_g void allocheap(size_t size, enum LISPTYPE type, addr *root, int size2)
{
	enum LISPCLASS index;
	addr pos;

	/* alloc */
	size_and_class(size, &index, &size);
	if (IsClassSmall(index))
		allocheap_small(size, (int)index, &pos);
	else
		allocheap_large(size, (int)index, &pos);

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
 *  cell mutex
 */
static mutexlite *make_cellmutex(void)
{
	mutexlite *ptr;

	ptr = malloctype(mutexlite);
	if (ptr == NULL) {
		Debug("malloctype error");
		return NULL;
	}
	if (make_mutexlite(ptr)) {
		Debug("make_mutexlite error");
		free(ptr);
		return NULL;
	}

	return ptr;
}

static void destroy_cellmutex(mutexlite *ptr)
{
	destroy_mutexlite(ptr);
	free(ptr);
}


/*
 *  init heap
 */
static int make_mutexheap(void)
{
	int i, k;
	mutexlite *mutex[LISPCLASS_Length], *ptr;

	for (i = 0; i < LISPCLASS_Length; i++) {
		ptr = make_cellmutex();
		if (ptr == NULL) {
			Debug("make_cellmutex error");
			for (k = 0; k < i; k++)
				destroy_cellmutex(mutex[k]);
			return 1;
		}
		mutex[i] = ptr;
		heap_info[i].mutex = ptr;
	}

	return 0;
}

static void free_mutexheap(void)
{
	int i;
	mutexlite *ptr;

	for (i = 0; i < LISPCLASS_Length; i++) {
		ptr = heap_info[i].mutex;
		destroy_cellmutex(ptr);
	}
}

#define HeapInfoSize (sizeof(struct heapinfo) * LISPCLASS_Length)
static int tailheap(void)
{
	size_t i;
	struct heapinfo *ptr;

	/* Tail memory */
	Tail = (addr)Align8Cut(Size + (uintptr_t)heap_root);
	heap_info = (struct heapinfo *)alloctail((size_t)HeapInfoSize);
	memset(heap_info, 0, (size_t)HeapInfoSize);
	for (i = 0; i < LISPCLASS_Length; i++) {
		ptr = &(heap_info[i]);
		ptr->type = (enum LISPCLASS)i;
		ptr->direct = (size_t)LISPCLASS_SizeK <= i;
	}

	/* mutex */
	if (make_mutexheap()) {
		Debug("make_mutexheap error.");
		return 1;
	}

	return 0;
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

static int initmutex(void)
{
	if (make_mutexlite(&Mutex)) return 1;
	if (make_mutexlite(&heap_cons_mutex)) goto error1;
	if (make_mutexlite(&heap_symbol_mutex)) goto error2;
	return 0;

error2:
	destroy_mutexlite(&heap_cons_mutex);
error1:
	destroy_mutexlite(&Mutex);
	return 1;
}

static void freemutex(void)
{
	destroy_mutexlite(&heap_symbol_mutex);
	destroy_mutexlite(&heap_cons_mutex);
	destroy_mutexlite(&Mutex);
}

_g int alloc_heap(size_t size)
{
	void *ptr;

	if (heap_alloc) {
		Debug("heap memory already allocated.");
		return 1;
	}
	if (size < 1000UL * 1000UL) {  /* use 1000 (not 1024) */
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

	/* make global mutex */
	if (initmutex()) {
		Debug("make_mutexlite error");
		goto error1;
	}

	/* make front */
	frontheap(ptr, size);

	/* make tail */
	if (tailheap()) {
		Debug("tailheap error");
		goto error2;
	}


	heap_cons_count = 0;
	heap_symbol_count = 0;
	CellRoot = CellPos = 0;
	heap_alloc = ptr;
	return 0;

error2:
	freemutex();
error1:
	free(ptr);
	return 1;
}

_g void free_heap(void)
{
	if (heap_alloc) {
		free_mutexheap();
		freemutex();
		free(heap_alloc);
		heap_root = 0;
		heap_front = 0;
		heap_pos = 0;
		heap_object = 0;
		heap_count = 0;
		heap_gc_count = 0;
		Size = 0;
		FrontMax = 0;
		Tail = 0;
		heap_cons_count = 0;
		heap_symbol_count = 0;
		heap_info = 0;
		CellPos = 0;
		CellRoot = 0;
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
_g void foreach_heap(void (*call)(struct heapinfo *))
{
	size_t index;

	for (index = 0; index < LISPCLASS_Length; index++)
		call(&heap_info[index]);
}

_g int foreach_check_heap(int (*call)(struct heapinfo *))
{
	size_t index;

	for (index = 0; index < LISPCLASS_Length; index++) {
		if (call(&heap_info[index]))
			return 1;
	}

	return 0;
}

_g void cellupdate_heap(void)
{
	struct heapcell *chain;

	Check(CellRoot == NULL, "CellRoot error");
	for (CellPos = CellRoot; ; CellPos = chain) {
		if (CellPos->count == 0) break;
		chain = CellPos->chain;
		if (chain == NULL) break;
	}
}

_g int valid_heap(const void *pos)
{
	return (heap_root <= (addr)pos) && ((addr)pos <= heap_front);
}

_g size_t get_heap_object(void)
{
	return heap_object;
}

_g size_t get_heap_count(void)
{
	return heap_count;
}

_g size_t get_heap_gc_count(void)
{
	return heap_gc_count;
}

_g size_t get_heap_size(void)
{
	return Size;
}

