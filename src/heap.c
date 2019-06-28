#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "alloc.h"
#include "build.h"
#include "file_memory.h"
#include "heap.h"
#include "memory.h"
#include "stream.h"
#include "thread.h"
#include "typedef.h"

/* external variable */
void					*heap_alloc = 0;
addr					 heap_root = 0;
addr					 heap_front = 0;
addr                     heap_pos = 0;

/* static varibales */
static mutexlite         Mutex;
static size_t            Size = 0;
static addr              FrontMax = 0;
static addr              Tail = 0;
static int               Cons = 0;
static mutexlite         ConsMutex;
static int               Symbol = 0;
static mutexlite         SymbolMutex;

static struct heapinfo  *Info = 0;
static struct heapcell  *CellPos = 0;
static struct heapcell  *CellRoot = 0;

#ifdef LISP_DEGRADE
struct heapinfo **Degrade_heap_Info(void) { return &Info; }
#endif


/*
 *  memory overflow
 */
static void memoryerror(void)
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
	for (;;) {
		Check(heap_front <= now, "space error");
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

void makespace(addr pos, size_t size)
{
#ifdef LISP_DEBUG
	Check(size < 2, "size error");
#endif
	if (size < (8UL + IdxSize)) {
		SetType(pos, LISPSYSTEM_SPACE1);
		SetSizeSpace1(pos, size);
	}
	else {
		SetType(pos, LISPSYSTEM_SPACE);
		SetSizeSpace(pos, size);
	}
}

void makereserved(addr pos, size_t size)
{
#ifdef LISP_DEBUG
	Check(size < (8UL + IdxSize), "size error");
#endif
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

static addr alloclock(size_t size, addr (*call)(size_t))
{
	addr result;

	lock_mutexlite(&Mutex);
	result = call(size);
	unlock_mutexlite(&Mutex);
	if (result == NULL) memoryerror();

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
			memoryerror();
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

static struct heapcell *cellexpand(struct heapinfo *root, struct heapcell *cell)
{
	struct heapcell *make;

	make = cellalloc();
	if (cell == NULL)
		root->root = root->front = make;
	else
		root->front = root->front->next = make;

	return make;
}

static void allocheap_small(size_t size, int index, addr *ret)
{
	addr pos;
	struct heapinfo *root;
	struct heapcell *cell;

	root = &Info[index];
	cell = root->front;
	if (cell == NULL || HeapCount <= cell->count) {
		cell = cellexpand(root, cell);
		fillheapmemory(cell, size);
	}

	pos = cell->point[cell->count++];
	Check(GetType(pos) != LISPSYSTEM_RESERVED, "type error");
	*ret = pos;
}

static void allocheap_large(size_t size, int index, addr *ret)
{
	addr pos;
	struct heapinfo *root;
	struct heapcell *cell;

	root = &Info[index];
	cell = root->front;
	if (cell == NULL || HeapCount <= cell->count) {
		cell = cellexpand(root, cell);
		fillcellunbound(cell);
	}

	Check(cell->point[cell->count] != Unbound, "type error");
	pos = allocfront(size);
	cell->point[cell->count++] = pos;
	*ret = pos;
}

static void allocheap(size_t size, enum LISPTYPE type, addr *root, int size2)
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
		Info[i].mutex = ptr;
	}

	return 0;
}

static void free_mutexheap(void)
{
	int i;
	mutexlite *ptr;

	for (i = 0; i < LISPCLASS_Length; i++) {
		ptr = Info[i].mutex;
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
	Info = (struct heapinfo *)alloctail((size_t)HeapInfoSize);
	memset(Info, 0, (size_t)HeapInfoSize);
	for (i = 0; i < LISPCLASS_Length; i++) {
		ptr = &(Info[i]);
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

	align = Align8Space(ptr);
	Size = size - align;
	heap_pos = heap_front = FrontMax = heap_root = align + (addr)ptr;
	CheckAlign8(heap_pos, "align8 error");
}

static int initmutex(void)
{
	if (make_mutexlite(&Mutex)) return 1;
	if (make_mutexlite(&ConsMutex)) goto error1;
	if (make_mutexlite(&SymbolMutex)) goto error2;
	return 0;

error2:
	destroy_mutexlite(&ConsMutex);
error1:
	destroy_mutexlite(&Mutex);
	return 1;
}

static void freemutex(void)
{
	destroy_mutexlite(&SymbolMutex);
	destroy_mutexlite(&ConsMutex);
	destroy_mutexlite(&Mutex);
}

int alloc_heap(size_t size)
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


	Cons = 0;
	Symbol = 0;
	CellRoot = CellPos = 0;
	heap_alloc = ptr;
	return 0;

error2:
	freemutex();
error1:
	free(ptr);
	return 1;
}

void free_heap(void)
{
	if (heap_alloc) {
		free_mutexheap();
		freemutex();
		free(heap_alloc);
		heap_root = 0;
		heap_front = 0;
		heap_pos = 0;
		Size = 0;
		FrontMax = 0;
		Tail = 0;
		Cons = 0;
		Symbol = 0;
		Info = 0;
		CellPos = 0;
		CellRoot = 0;
		heap_alloc = 0;
	}
}


/*
 *  gc
 */
void foreach_heap(void (*call)(struct heapinfo *))
{
	size_t index;

	for (index = 0; index < LISPCLASS_Length; index++)
		call(&Info[index]);
}

int foreach_check_heap(int (*call)(struct heapinfo *))
{
	size_t index;

	for (index = 0; index < LISPCLASS_Length; index++) {
		if (call(&Info[index]))
			return 1;
	}

	return 0;
}

void cellupdate_heap(void)
{
	struct heapcell *chain;

	Check(CellRoot == NULL, "CellRoot error");
	for (CellPos = CellRoot; ; CellPos = chain) {
		if (CellPos->count == 0) break;
		chain = CellPos->chain;
		if (chain == NULL) break;
	}
}

int valid_heap(const void *pos)
{
	return (heap_root <= (addr)pos) && ((addr)pos <= heap_front);
}


/*
 *  allocate
 */
addr heapr_cons(void)
{
	int index;
	addr pos;
	addr *array;

	/* rotate index */
	lock_mutexlite(&ConsMutex);
	index = Cons++;
	if (LISPCLASS_ConsLength <= Cons) Cons = 0;
	unlock_mutexlite(&ConsMutex);

	/* alloc memory */
	allocheap_small((size_t)ConsLength, index, &pos);
	*PtrValue2L(pos) = ConsLength;
	SetType(pos, LISPTYPE_CONS);
	SetStatus(pos, LISPSIZE_ARRAY2);
	SetCheck2(pos, LISPCHECK_SIZE2, LISPCHECK_ARRAY);
	SetCheck3(pos, LISPCHECK_LIST, LISPCHECK_SIZE2, LISPCHECK_ARRAY);
	*PtrLenArrayA2(pos) = 2; /* left, right */
	array = (addr *)PtrByte2P(pos);
	array[0] = array[1] = Nil;

	return pos;
}
void heap_cons(addr *root)
{
	*root = heapr_cons();
}

addr heapr_symbol(void)
{
	int index;
	addr pos;

	/* rotate index */
	lock_mutexlite(&SymbolMutex);
	index = Symbol++;
	if (LISPCLASS_SymbolLength <= Symbol) Symbol = 0;
	unlock_mutexlite(&SymbolMutex);
	index += LISPCLASS_ConsLength;

	/* alloc memory */
	allocheap_small((size_t)SymbolLength, index, &pos);
	*PtrValue2L(pos) = SymbolLength;
	SetType(pos, LISPTYPE_SYMBOL);
	SetStatus(pos, LISPSIZE_ARRAY2);
	SetCheck4(pos, LISPCHECK_SYMBOL, LISPCHECK_LIST, LISPCHECK_SIZE2, LISPCHECK_ARRAY);
	*PtrLenArrayA2(pos) = SYMBOL_INDEX_SIZE;
	nilarray2(pos, SYMBOL_INDEX_SIZE);

	return pos;
}
void heap_symbol(addr *root)
{
	*root = heapr_symbol();
}

addr heapr_array2_memory(enum LISPTYPE type, byte16 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA2(array);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_ARRAY2);
	SetCheck2(pos, LISPCHECK_SIZE2, LISPCHECK_ARRAY);
	*PtrLenArrayA2(pos) = array;
	nilarray2(pos, array);

	return pos;
}
void heap_array2_memory(addr *root, enum LISPTYPE type, byte16 array)
{
	*root = heapr_array2_memory(type, array);
}

addr heapr_array4_memory(enum LISPTYPE type, byte32 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA4(array);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAY4);
	SetCheck2(pos, LISPCHECK_SIZE4, LISPCHECK_ARRAY);
	*PtrLenArrayA4(pos) = array;
	nilarray4(pos, array);

	return pos;
}
void heap_array4_memory(addr *root, enum LISPTYPE type, byte32 array)
{
	*root = heapr_array4_memory(type, array);
}

#ifdef LISP_ARCH_64BIT
addr heapr_array8(enum LISPTYPE type, size_t array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA8(array);
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAY8);
	SetCheck2(pos, LISPCHECK_SIZE8, LISPCHECK_ARRAY);
	*PtrLenArrayA8(pos) = array;
	nilarray8(pos, array);

	return pos;
}
void heap_array8(addr *root, enum LISPTYPE type, size_t array)
{
	*root = heapr_array8(type, array);
}
#endif

addr heapr_body2_memory(enum LISPTYPE type, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB2(body);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_BODY2);
	SetCheck2(pos, LISPCHECK_SIZE2, LISPCHECK_BODY);
	*PtrLenBodyB2(pos) = body;

	return pos;
}
void heap_body2_memory(addr *root, enum LISPTYPE type, byte16 body)
{
	*root = heapr_body2_memory(type, body);
}

addr heapr_body4_memory(enum LISPTYPE type, byte32 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB4(body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_BODY4);
	SetCheck2(pos, LISPCHECK_SIZE4, LISPCHECK_BODY);
	*PtrLenBodyB4(pos) = body;

	return pos;
}
void heap_body4_memory(addr *root, enum LISPTYPE type, byte32 body)
{
	*root = heapr_body4_memory(type, body);
}

#ifdef LISP_ARCH_64BIT
addr heapr_body8(enum LISPTYPE type, size_t body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB8(body);
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_BODY8);
	SetCheck2(pos, LISPCHECK_SIZE8, LISPCHECK_BODY);
	*PtrLenBodyB8(pos) = body;

	return pos;
}
void heap_body8(addr *root, enum LISPTYPE type, size_t body)
{
	*root = heapr_body8(type, body);
}
#endif

addr heapr_smallsize_memory(enum LISPTYPE type, byte array, byte body)
{
	addr pos;
	size_t size;

	size = MemoryLengthSS(array, body);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	SetCheck4(pos, LISPCHECK_SIZE2,
			LISPCHECK_ARRAY, LISPCHECK_BODY, LISPCHECK_ARRAYBODY);
	nilarray2(pos, array);
	*PtrLenArraySS(pos) = array;
	*PtrLenBodySS(pos) = body;

	return pos;
}
void heap_smallsize_memory(addr *root, enum LISPTYPE type, byte array, byte body)
{
	*root = heapr_smallsize_memory(type, array, body);
}

addr heapr_arraybody_memory(enum LISPTYPE type, byte16 array, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthAB(array, body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	SetCheck4(pos, LISPCHECK_SIZE4,
			LISPCHECK_ARRAY, LISPCHECK_BODY, LISPCHECK_ARRAYBODY);
	nilarray4(pos, array);
	*PtrLenArrayAB(pos) = array;
	*PtrLenBodyAB(pos) = body;

	return pos;
}
void heap_arraybody_memory(addr *root, enum LISPTYPE type, byte16 array, byte16 body)
{
	*root = heapr_arraybody_memory(type, array, body);
}

addr heapr_array(enum LISPTYPE type, size_t array)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		return heapr_array2_memory(type, (byte16)array);
	else if (MemoryLengthA4(array) <= 0xFFFFFFFFUL)
		return heapr_array4_memory(type, (byte32)array);
	else
		return heapr_array8(type, array);
#else
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		return heapr_array2_memory(type, (byte16)array);
	else
		return heapr_array4_memory(type, array);
#endif
}
void heap_array(addr *root, enum LISPTYPE type, size_t array)
{
	*root = heapr_array(type, array);
}

addr heapr_body(enum LISPTYPE type, size_t body)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		return heapr_body2_memory(type, (byte16)body);
	else if (MemoryLengthB4(body) <= 0xFFFFFFFFUL)
		return heapr_body4_memory(type, (byte32)body);
	else
		return heapr_body8(type, body);
#else
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		return heapr_body2_memory(type, (byte16)body);
	else
		return heapr_body4_memory(type, body);
#endif
}
void heap_body(addr *root, enum LISPTYPE type, size_t body)
{
	*root = heapr_body(type, body);
}

#ifdef LISP_DEBUG
addr heapr_array2_debug(enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	return heapr_array2_memory(type, (byte16)array);
}
addr heapr_array4_debug(enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	return heapr_array4_memory(type, (byte32)array);
}
addr heapr_body2_debug(enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	return heapr_body2_memory(type, (byte16)body);
}
addr heapr_body4_debug(enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	return heapr_body4_memory(type, (byte32)body);
}
addr heapr_smallsize_debug(enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	return heapr_smallsize_memory(type, (byte)array, (byte)body);
}
addr heapr_arraybody_debug(enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	return heapr_arraybody_memory(type, (byte16)array, (byte16)body);
}

void heap_array2_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	heap_array2_memory(root, type, (byte16)array);
}
void heap_array4_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	heap_array4_memory(root, type, (byte32)array);
}
void heap_body2_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	heap_body2_memory(root, type, (byte16)body);
}
void heap_body4_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	heap_body4_memory(root, type, (byte32)body);
}
void heap_smallsize_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	heap_smallsize_memory(root, type, (byte)array, (byte)body);
}
void heap_arraybody_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	heap_arraybody_memory(root, type, (byte16)array, (byte16)body);
}
#endif


/*
 *  core
 */
#define IfWriteCheck(fm,p,s,m) IfDebug(writecheck_filememory((fm),(p),(s)),(m))
#define IfWriteAddr(fm,p,m) IfDebug(writeaddr_filememory((fm),(p)),(m))
#define IfWritePtr(fm,p,m) IfDebug(writeptr_filememory((fm),(p)),(m))
#define IfReadCheck(fm,p,s,m) IfDebug(readcheck_filememory((fm),(p),(s)),(m))
#define IfReadAddr(fm,p,m) IfDebug(readaddr_filememory((fm),(p)),(m))
#define IfReadPtr(fm,p,m) IfDebug(readptr_filememory((fm),(p)),(m))

/* save/load array2 */
static int writearray(struct filememory *fm, const addr *array, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		IfWriteAddr(fm, array[i], "writeaddr error.");
	}

	return 0;
}
static int readarray(struct filememory *fm, addr *array, size_t size)
{
	size_t i;

	for (i = 0; i < size; i++) {
		IfReadAddr(fm, array + i, "readaddr error.");
	}

	return 0;
}

static int save_object_array2(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	array = PtrArrayA2(pos);
	size = (size_t)GetLenArrayA2(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array2(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	array = PtrArrayA2(pos);
	size = (size_t)GetLenArrayA2(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}


/* save/load array4 */
static int save_object_array4(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 8UL, "writecheck error.");
	array = PtrArrayA4(pos);
	size = (size_t)GetLenArrayA4(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array4(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 8UL, "readcheck error");
	array = PtrArrayA4(pos);
	size = (size_t)GetLenArrayA4(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}


/* save/load array8 */
#ifdef LISP_ARCH_64BIT
static int save_object_array8(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 16UL, "writecheck error");
	array = PtrArrayA8(pos);
	size = (size_t)GetLenArrayA8(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");

	return 0;
}
static int load_object_array8(struct filememory *fm, addr pos)
{
	addr *array;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 16UL, "readcheck error");
	array = PtrArrayA8(pos);
	size = (size_t)GetLenArrayA8(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");

	return 0;
}
#else
static int save_object_array8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
static int load_object_array8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
#endif


/* save/load smallsize */
static int save_object_smallsize(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	/* array */
	array = PtrArraySS(pos);
	size = (size_t)GetLenArraySS(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");
	/* body */
	body = PtrBodySSa(pos, size);
	size = (size_t)GetLenBodySS(pos);
	IfWriteCheck(fm, body, size, "writecheck error");

	return 0;
}
static int load_object_smallsize(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	/* array */
	array = PtrArraySS(pos);
	size = (size_t)GetLenArraySS(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");
	/* body */
	body = PtrBodySSa(pos, size);
	size = (size_t)GetLenBodySS(pos);
	IfReadCheck(fm, body, size, "readcheck error");

	return 0;
}


/* save/load arraybody */
static int save_object_arraybody(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	IfWriteCheck(fm, pos + 8UL, 8UL, "writecheck error");
	/* array */
	array = PtrArrayAB(pos);
	size = (size_t)GetLenArrayAB(pos);
	IfDebug(writearray(fm, array, size), "writearray error.");
	/* body */
	body = PtrBodyABa(pos, size);
	size = (size_t)GetLenBodyAB(pos);
	IfWriteCheck(fm, body, size, "writecheck error");

	return 0;
}
static int load_object_arraybody(struct filememory *fm, addr pos)
{
	addr *array;
	byte *body;
	size_t size;

	IfReadCheck(fm, pos + 8UL, 8UL, "readcheck error");
	/* array */
	array = PtrArrayAB(pos);
	size = (size_t)GetLenArrayAB(pos);
	IfDebug(readarray(fm, array, size), "readarray error.");
	/* body */
	body = PtrBodyABa(pos, size);
	size = (size_t)GetLenBodyAB(pos);
	IfReadCheck(fm, body, size, "readcheck error");

	return 0;
}


/* save/load body2 */
static int save_object_body2(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB2(pos);
	IfWriteCheck(fm, pos + 8UL, size, "writecheck error");

	return 0;
}
static int load_object_body2(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB2(pos);
	IfReadCheck(fm, pos + 8UL, size, "readcheck error");

	return 0;
}


/* save/load body4 */
static int save_object_body4(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB4(pos);
	IfWriteCheck(fm, pos + 8UL, 8UL + size, "writecheck error.");

	return 0;
}
static int load_object_body4(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB4(pos);
	IfReadCheck(fm, pos + 8UL, 8UL + size, "readcheck error");

	return 0;
}


/* save/load body8 */
#ifdef LISP_ARCH_64BIT
static int save_object_body8(struct filememory *fm, addr pos)
{
	size_t size;

	size = (size_t)GetLenBodyB8(pos);
	IfWriteCheck(fm, pos + 8UL, 16UL + size, "writecheck error");

	return 0;
}
static int load_object_body8(struct filememory *fm, addr pos)
{
	size_t size;

	IfReadCheck(fm, pos + 8UL, 16UL, "readcheck error");
	size = (size_t)GetLenBodyB8(pos);
	IfReadCheck(fm, pos + 24UL, size, "readcheck error");

	return 0;
}
#else
static int save_object_body8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
static int load_object_body8(struct filememory *fm, addr pos)
{
	Abort("Invalid object size: size8 [32bit]");
	return 1;
}
#endif


/* save/load object */
typedef int (*save_object_call)(struct filememory *, addr);
typedef int (*load_object_call)(struct filememory *, addr);
save_object_call Heap_SaveObject[LISPSIZE_SIZE];
load_object_call Heap_LoadObject[LISPSIZE_SIZE];

static int save_object(struct filememory *fm, addr pos, size_t *ret)
{
	unsigned index;

	*ret = getobjectlength(pos);
	IfWriteCheck(fm, pos, 8UL, "writecheck error: save_object");
	index = (unsigned)GetStatusSize(pos);
	Check(LISPSIZE_SIZE <= index, "size error");
	return (Heap_SaveObject[index])(fm, pos);
}
static int load_object(struct filememory *fm, addr pos, size_t *ret)
{
	unsigned index;

	IfReadCheck(fm, pos + 1UL, 7UL, "readcheck error: load_object");
	index = (unsigned)GetStatusSize(pos);
	Check(LISPSIZE_SIZE <= index, "size error");
	if ((Heap_LoadObject[index])(fm, pos)) {
		Debug("Heap_LoadObject error");
		return 1;
	}
	*ret = getobjectlength(pos);

	return 0;
}


/* save/load space1 */
static int save_space1(struct filememory *fm, addr pos, size_t *ret)
{
	GetSizeSpace1(pos, ret);
	IfWriteCheck(fm, pos, 2UL, "writecheck error.");
	return 0;
}
static int load_space1(struct filememory *fm, addr pos, size_t *ret)
{
	IfDebug(getc_filememory(fm, pos + 1), "readcheck error.");
	GetSizeSpace1(pos, ret);
	return 0;
}


/* save/load space */
static int save_space(struct filememory *fm, addr pos, size_t *ret)
{
	size_t size;

	GetSizeSpace(pos, ret);
	IfDebug(putc_filememory(fm, pos[0]), "putc error.");
	GetValueSpace(pos, &size);
	IfWriteCheck(fm, &size, IdxSize, "writecheck error.");
	return 0;
}
static int load_space(struct filememory *fm, addr pos, size_t *ret)
{
	IfReadCheck(fm, pos + 8UL, IdxSize, "readcheck error.");
	GetSizeSpace(pos, ret);
	return 0;
}


/* save/load reserved */
static int save_reserved(struct filememory *fm, addr pos, size_t *ret)
{
	size_t size;

	GetSizeReserved(pos, ret);
	IfDebug(putc_filememory(fm, pos[0]), "putc error.");
	GetValueReserved(pos, &size);
	IfWriteCheck(fm, &size, IdxSize, "writecheck error.");
	return 0;
}
static int load_reserved(struct filememory *fm, addr pos, size_t *ret)
{
	IfReadCheck(fm, pos + 8UL, IdxSize, "readcheck error.");
	GetSizeReserved(pos, ret);
	return 0;
}


/* save-dump-stream */
static int save_object_stream(struct filememory *fm, addr pos, size_t *ret)
{
	if (save_stream(pos)) {
		Debug("save_stream error");
		return 1;
	}
	if (save_object(fm, pos, ret)) {
		Debug("save_object error");
		return 1;
	}

	return 0;
}


/* save/load dump */
static int save_dump(struct filememory *fm)
{
	addr pos;
	size_t size;

	for (pos = heap_root; pos < heap_front; pos += size) {
		switch (GetType(pos)) {
			case LISPSYSTEM_SPACE1:
				IfDebug(save_space1(fm, pos, &size), "save_space1 error.");
				break;

			case LISPSYSTEM_SPACE:
				IfDebug(save_space(fm, pos, &size), "save_space error.");
				break;

			case LISPSYSTEM_RESERVED:
				IfDebug(save_reserved(fm, pos, &size), "save_reserved error.");
				break;

			case LISPTYPE_STREAM:
				IfDebug(save_object_stream(fm, pos, &size), "save_object_stream error.");
				break;

			default:
				IfDebug(save_object(fm, pos, &size), "save_object error.");
				break;
		}
	}

	/* END check */
	IfDebug(putc_filememory(fm, (byte)LISPSYSTEM_END), "putc error.");

	return 0;
}
static int load_dump(struct filememory *fm)
{
	byte c;
	addr pos;
	size_t size;

	for (pos = heap_root; pos < heap_front; pos += size) {
		IfDebug(getc_filememory(fm, pos), "getc error.");
		switch (pos[0]) {
			case LISPSYSTEM_SPACE1:
				IfDebug(load_space1(fm, pos, &size), "load_space1 error.");
				break;

			case LISPSYSTEM_SPACE:
				IfDebug(load_space(fm, pos, &size), "load_space error.");
				break;

			case LISPSYSTEM_RESERVED:
				IfDebug(load_reserved(fm, pos, &size), "load_reserved error.");
				break;

			default:
				IfDebug(load_object(fm, pos, &size), "load_object error.");
				break;
		}
	}

	/* END check */
	IfDebug(getc_filememory(fm, &c), "getc error.");
	IfDebug(c != LISPSYSTEM_END, "end error.");

	return 0;
}


/* save/load info */
static size_t save_info_size(int index)
{
	size_t size;
	struct heapcell *ptr;

	size = 0;
	for (ptr = Info[index].root; ptr; ptr = ptr->next) {
		size ++;
	}

	return size;
}

static int save_info_child(struct filememory *fm, int index)
{
	addr *point, pos;
	struct heapcell *ptr;
	size_t i, size, count;

	/* index */
	IfWriteCheck(fm, &index, sizeoft(index), "writecheck error: index.");
	/* size */
	size = save_info_size(index);
	IfWriteCheck(fm, &size, IdxSize, "writecheck error: size.");
	/* info */
	for (ptr = Info[index].root; ptr; ptr = ptr->next) {
		count = ptr->count;
		point = ptr->point;
		IfWriteCheck(fm, &count, IdxSize, "writecheck error: count.");
		for (i = 0; i < HeapCount; i++) {
			IfWritePtr(fm, point[i], "writeaddr error: point.");
		}
	}

	/* error check */
	pos = NULL;
	IfWriteCheck(fm, &pos, sizeoft(pos), "writecheck error: null");

	return 0;
}
static int load_info_child(struct filememory *fm, int index)
{
	int check;
	addr *point, pos;
	struct heapinfo *info;
	struct heapcell *cell;
	size_t i, k, size, count;

	/* index */
	IfReadCheck(fm, &check, sizeoft(check), "readcheck error: index");
	IfDebug(check != index, "index error.");
	/* size */
	IfReadCheck(fm, &size, IdxSize, "readcheck error: size");
	/* info */
	info = &Info[index];
	cell = NULL;
	for (k = 0; k < size; k++) {
		IfReadCheck(fm, &count, IdxSize, "readcheck error: count");
		cell = cellexpand(info, cell);
		point = cell->point;
		cell->count = count;
		for (i = 0; i < HeapCount; i++) {
			IfReadPtr(fm, (void **)(point + i), "readaddr error.");
		}
	}

	/* error check */
	IfReadCheck(fm, &pos, sizeoft(pos), "readaddr error: null");
	IfDebug(pos != NULL, "readaddr error: check");

	return 0;
}

static int save_info(struct filememory *fm)
{
	int index;

	for (index = 0; index < LISPCLASS_Length; index++) {
		IfDebug(save_info_child(fm, index), "save_info_child error.");
	}
	/* error check */
	index = -1;
	IfWriteCheck(fm, &index, sizeoft(index), "writecheck error: check.");

	return 0;
}
static int load_info(struct filememory *fm)
{
	int index;

	for (index = 0; index < LISPCLASS_Length; index++) {
		IfDebug(load_info_child(fm, index), "load_info_child error.");
	}
	/* error check */
	IfReadCheck(fm, &index, sizeoft(index), "readcheck error: check.");
	IfDebug(index != -1, "end check error.");

	return 0;
}


/* save/load data */
static int save_data(struct filememory *fm)
{
	IfWritePtr(fm, heap_front, "writeptr error: heap_front");
	IfWritePtr(fm, heap_pos, "writeptr error: heap_pos");
	if (save_dump(fm)) {
		Debug("save_dump error");
		return 1;
	}

	return 0;
}
static int load_data(struct filememory *fm)
{
	IfReadPtr(fm, (void **)&heap_front, "readptr error: heap_front");
	IfReadPtr(fm, (void **)&heap_pos, "readptr error: heap_pos");
	if (load_dump(fm)) {
		Debug("load_dump error");
		return 1;
	}

	return 0;
}


/* save/load info */
int save_heap(struct filememory *fm)
{
	if (save_data(fm)) {
		Debug("save_data error.");
		return 1;
	}
	if (save_info(fm)) {
		Debug("save_info error.");
		return 1;
	}

	return 0;
}
int load_heap(struct filememory *fm)
{
	if (load_data(fm)) {
		Debug("load_data error.");
		return 1;
	}
	if (load_info(fm)) {
		Debug("load_info error.");
		return 1;
	}

	return 0;
}


/*
 *  initialize
 */
void init_heap(void)
{
	/* save-object */
	Heap_SaveObject[LISPSIZE_ARRAY2] = save_object_array2;
	Heap_SaveObject[LISPSIZE_ARRAY4] = save_object_array4;
	Heap_SaveObject[LISPSIZE_ARRAY8] = save_object_array8;
	Heap_SaveObject[LISPSIZE_SMALLSIZE] = save_object_smallsize;
	Heap_SaveObject[LISPSIZE_ARRAYBODY] = save_object_arraybody;
	Heap_SaveObject[LISPSIZE_BODY2] = save_object_body2;
	Heap_SaveObject[LISPSIZE_BODY4] = save_object_body4;
	Heap_SaveObject[LISPSIZE_BODY8] = save_object_body8;
	/* load-object */
	Heap_LoadObject[LISPSIZE_ARRAY2] = load_object_array2;
	Heap_LoadObject[LISPSIZE_ARRAY4] = load_object_array4;
	Heap_LoadObject[LISPSIZE_ARRAY8] = load_object_array8;
	Heap_LoadObject[LISPSIZE_SMALLSIZE] = load_object_smallsize;
	Heap_LoadObject[LISPSIZE_ARRAYBODY] = load_object_arraybody;
	Heap_LoadObject[LISPSIZE_BODY2] = load_object_body2;
	Heap_LoadObject[LISPSIZE_BODY4] = load_object_body4;
	Heap_LoadObject[LISPSIZE_BODY8] = load_object_body8;
}

